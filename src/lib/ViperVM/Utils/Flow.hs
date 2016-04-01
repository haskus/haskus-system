{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Control flow
module ViperVM.Utils.Flow
   ( flowSeq
   , flowSeqN
   , flowSeqE
   , flowSeqM
   , flowSeq'
   , flowMSeq
   , flowMSeqM
   , flowMTraceM
   , flowFinally
   , flowFinallyV
   , flowFinallyM
   , flowMatch
   , flowMap
   , flowOp2
   , flowRetry
   , flowBind
   , flowCatchE
   , flowCatch
   , flowMCatch
   , flowFusion
   , flowSet
   , flowRet
   , flowLift
   , FlowT (..)
   , Flow
   , liftFlowT
   )
where

import ViperVM.Utils.Variant
import ViperVM.Utils.HList

import Control.Monad
import Data.Proxy
import GHC.TypeLits
import Data.Foldable (traverse_)

-- | Bind two variants in sequence. By convention, we choose the first variant
-- value/type as the valid one.
-- (i.e. generalized EitherT binding with reversed parameters)
flowSeq :: forall x xs m y ys.
   ( Monad m
   , KnownNat (1 + Length ys)
   )
   => m (Flow xs x) -> (x -> m (Flow ys y)) -> m (Flow (Concat ys xs) y)
flowSeq v f = updateVariantFoldM (Proxy :: Proxy 0) f =<< v

-- | Connect a flow to the output n of another
flowSeqN :: forall x m l1 l2 l3 n.
   ( l3 ~ ReplaceAt n l1 l2
   , x ~ TypeAt n l1
   , Monad m
   , KnownNat (Length l2)
   , KnownNat n
   )
   => Proxy n -> m (Variant l1) -> (x -> m (Variant l2)) -> m (Variant l3)
flowSeqN _ v f = updateVariantFoldM (Proxy :: Proxy n) f =<< v

-- | Lift an Either into a flow
flowSeqE :: forall x (xs :: [*]) m a b.
   ( Monad m
   ) => m (Flow xs x) -> (x -> m (Either a b)) -> m (Flow (a ': xs) b)
flowSeqE v f = flowSeq v (liftEitherM . f)

-- | Lift a non-failing function into a flow
flowSeqM :: forall x (xs :: [*]) m a.
   ( Monad m
   ) => m (Flow xs x) -> (x -> m a) -> m (Flow xs a)
flowSeqM v f = v `flowSeq` f'
   where
      f' :: x -> m (Variant '[a])
      f' x = setVariant0 <$> f x


-- | Like `flowSeq` but specialised for `()` (similarly to `>>`)
flowSeq' :: forall xs y ys m.
   ( Monad m
   , KnownNat (1 + Length ys)
   )
   => m (Flow xs ()) -> m (Flow ys y) -> m (Flow (Concat ys xs) y)
flowSeq' v f = flowSeq v (const f)

-- | Compose like (>=>)
flowMSeq :: forall x xs m y ys a.
   ( Monad m
   , KnownNat (1 + Length ys)
   )
   => (a -> m (Flow xs x)) -> (x -> m (Flow ys y)) -> (a -> m (Flow (Concat ys xs) y))
flowMSeq v f = updateVariantFoldM (Proxy :: Proxy 0) f <=< v

-- | Lift a non-failing function into a flow
flowMSeqM :: forall x (xs :: [*]) m y b.
   ( Monad m
   ) => (b -> m (Flow xs x)) -> (x -> m y) -> (b -> m (Flow xs y))
flowMSeqM v f = v `flowMSeq` f'
   where
      f' :: x -> m (Flow '[] y)
      f' x = flowRet =<< f x

-- | Lift a non-failing tracing function into a flow
flowMTraceM :: forall x (xs :: [*]) m b.
   ( Monad m
   ) => (b -> m (Flow xs x)) -> (x -> m ()) -> (b -> m (Flow xs x))
flowMTraceM v f b = do
   r <- v b
   traverse_ f (getVariant0 r)
   return r

-- | Execute a statement regardless of the failure of the previous one
--
-- As we cannot report errors both in `xs` and `ys`, errors in `ys` are only
-- reported if `v` succeeds (i.e. there is no error in `xs`).
--
-- Said differently: errors in the inner block have the priority on errors in
-- the finally block
flowFinally :: forall m x xs ys y.
   ( Monad m
   , KnownNat (1 + Length ys)
   ) => m (Flow xs x) -> (Maybe x -> m (Flow ys y)) -> m (Flow (Concat ys xs) y)
flowFinally v f = do
   r  <- v
   updateVariantFoldM (Proxy :: Proxy 0) (const (f (getVariant0 r))) r

-- | Similar to `flowFinally`, but pass the whole variant to the finally block
flowFinallyV :: forall m x xs ys y.
   ( Monad m
   , KnownNat (1 + Length ys)
   ) => m (Flow xs x) -> (Variant (x ': xs) -> m (Flow ys y)) -> m (Flow (Concat ys xs) y)
flowFinallyV v f = do
   r  <- v
   updateVariantFoldM (Proxy :: Proxy 0) (const (f r)) r

-- | Execute a finally block that does not fail
flowFinallyM :: forall m x xs.
   ( Monad m
   ) => m (Flow xs x) -> (Maybe x -> m ()) -> m (Flow xs x)
flowFinallyM v f = do
   r  <- v
   f (getVariant0 r)
   return r

-- | Match a variant by using a tuple
flowMatch :: forall l t m a.  (Monad m , Matchable l t)
   => m (Variant l) -> (t -> m a) -> m a
flowMatch v f = f . matchVariant =<< v


-- | Map the variant
flowMap :: (Monad m) => m (Variant l) -> (Variant l -> b) -> m b
flowMap v f = f <$> v

-- | Combine two succeeding flows with the given operator
--
-- If the first flow fails, the second one isn't evaluated at all!
flowOp2 :: forall a b c as bs m.
   ( Monad m
   , KnownNat (Length (b ': bs))
   ) => (a -> b -> c) -> m (Flow as a) -> m (Flow bs b) -> m (Flow (Concat bs as) c)
flowOp2 op v1 v2 =
   flowSeq v1 $ \a ->
      flowSeq v2 $ \b -> do
         let
            v :: Variant '[c]
            v = setVariant0 (a `op` b)
         return v


-- | Retry a flow several times on error
flowRetry :: (Monad m) => Int -> m (Variant l) -> m (Variant l)
flowRetry n f = do
   r <- f
   case (n,getVariant0 r) of
      (0,_)       -> return r
      (_,Just _)  -> return r
      (_,Nothing) -> flowRetry (n-1) f

-- | Bind during a flow
flowBind :: Monad m => m a -> (a -> m b) -> m b
flowBind = (>>=)


-- | Catch all the values of type `a`, use Either
flowCatchE :: forall l a b m.
   ( Monad m
   , Catchable a l
   )
   => m (Variant l) -> (Either a (Variant (Filter a l)) -> m b) -> m b
flowCatchE v f = f . removeType =<< v

-- | Catch all the values of type `a`
flowCatch :: forall x xs xs' a m ys.
   ( Monad m
   , Catchable a xs
   , xs' ~ Filter a xs
   , KnownNat (1 + Length ys)
   )
   => m (Flow xs x) -> (a -> m (Flow ys x)) -> m (Flow (Concat ys xs') x)
flowCatch v f = do
   r <- v
   case pickVariant (Proxy :: Proxy 0) r of
      Right x -> flowRet x
      Left xs ->
         flowCatchE (return xs) $ \case
            Left a   -> appendVariant (Proxy :: Proxy xs') <$> f a
            Right v2 -> return (prependVariant (Proxy :: Proxy (x ': ys)) v2)

-- | Catch all the values of type `a`. The catcher can't fail.
flowMCatch :: 
   ( Catchable a xs
   , Monad m
   ) => m (Flow xs x) -> (a -> m x) -> m (Flow (Filter a xs) x)
flowMCatch v f = do
   r <- v
   case pickVariant (Proxy :: Proxy 0) r of
      Right x -> flowRet x
      Left xs ->
         flowCatchE (return xs) $ \case
            Left a   -> flowRet =<< f a
            Right ys -> return (prependVariant (Proxy :: Proxy '[x]) ys)
         

-- | Fusion variant values of the same type
flowFusion :: (Monad m, Liftable l (Nub l))
   => m (Variant l) -> m (Variant (Nub l))
flowFusion v = fusionVariant <$> v

-- | Set the first matching type of a Variant
flowSet :: forall a l n m.
   ( IsMember a l ~ 'True
   , n ~ IndexOf a l
   , a ~ TypeAt n l
   , KnownNat n
   , Monad m
   ) => a -> m (Variant l)
flowSet = return . setVariant

-- | Set the returned value
flowRet :: forall x xs m.
   ( Monad m
   ) => x -> m (Flow xs x)
flowRet = return . setVariant0

-- | Lift a flow into another
flowLift :: (Liftable xs ys , Monad m) => m (Variant xs) -> m (Variant ys)
flowLift = fmap liftVariant

-- | Flow monad transformer
newtype FlowT m (l :: [*]) a = FlowT
   { runFlowT :: m (Variant (a ': l))
   }

instance Monad m => Functor (FlowT m l) where
   fmap f (FlowT v) = FlowT (updateVariant0 f <$> v) 

type Flow xs x = Variant (x ': xs)

flowAp :: Monad m => m (Flow l (a->b)) -> m (Flow l a) -> m (Flow l b)
flowAp f x = do
   f' <- f
   let p = Proxy :: Proxy '[b]
   case headVariant f' of
      Left xs   -> return (prependVariant p xs)
      Right f'' -> do
         x' <- x
         return $ case headVariant x' of
            Left ys   -> prependVariant p ys
            Right x'' -> setVariant0 (f'' x'')


instance Monad m => Applicative (FlowT m l) where
   pure                = FlowT . return . setVariant0
   FlowT f <*> FlowT x = FlowT (flowAp f x)


flowMBind :: Monad m => FlowT m l a -> (a -> FlowT m l b) -> FlowT m l b
flowMBind (FlowT flowA) f = FlowT $ do
   a <- flowA
   let p = Proxy :: Proxy '[b]
   case headVariant a of
      Left xs -> return (prependVariant p xs)
      Right x -> let FlowT r = f x in r

instance Monad m => Monad (FlowT m l) where
   return = pure
   (>>=)  = flowMBind

-- | Lift a flow into a FlowT
liftFlowT :: (Liftable (x ': xs) (x ': ys) , Monad m)
   => m (Flow xs x) -> FlowT m ys x
liftFlowT = FlowT . flowLift
