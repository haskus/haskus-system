{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}


module ViperVM.Utils.Flow
   ( Flow
   , withFlow0
   , withFlow
   , flowRes
   , flowRet
   , flowRet'
   , flowSet
   , flowLift
   , flowTraverse
   , flowFor
   , (*~^>)
   , (>*~^>)
   , (~>)
   , (>~>)
   , (~#>)
   , (>~#>)
   , (#~>)
   , (>#~>)
   , (~^>)
   , (>~^>)
   , (#~^>)
   , (>#~^>)
   , (#!~>)
   , (>#!~>)
   , (#->)
   , (>#->)
   )
where

import ViperVM.Utils.Variant
import ViperVM.Utils.HList
import Data.Proxy
import GHC.TypeLits

type Flow m l = m (Variant l)

infixl 1  ~>,  ~#>,  #!~>,  #->
infixl 1 >~>, >~#>, >#!~>, >#->

infixr 1 #~>, >#~>

-- lifted actions in m
infixl 1  #~^>,  ~^>
infixl 1 >#~^>, >~^>

infixl 1 *~^>, >*~^>

-- | Extract single flow result
flowRes :: Functor m => Flow m '[x] -> m x
flowRes = fmap singleVariant

withFlow0 :: forall (k :: Nat) m l l2.
   ( KnownNat k
   , k ~ Length l2
   , Monad m )
   => Variant l -> (TypeAt 0 l -> Flow m l2) -> Flow m (ReplaceAt 0 l l2)
withFlow0 v f = updateVariantFoldM (Proxy :: Proxy 0) f v

(~>) :: forall (k :: Nat) m l l2 a.
   ( KnownNat k
   , k ~ Length l2
   , a ~ TypeAt 0 (a ': l)
   , Monad m )
   => Variant (a ': l) -> (a -> Flow m l2) -> Flow m (Concat l2 l)
(~>) = withFlow0

(~^>) :: forall m l x a.
   ( Monad m )
   => Variant (a ': l) -> (a -> m x) -> Flow m (x ': l)
(~^>) v f = v ~> (\x -> flowRet' =<< f x)

(>~>) :: forall (k :: Nat) m l l2 a.
   ( KnownNat k
   , k ~ Length l2
   , Monad m )
   => Flow m (a ': l) -> (a -> Flow m l2) -> Flow m (Concat l2 l)
(>~>) f g = f >>= (~> g)

(>~^>) :: forall m l x a.
   ( Monad m )
   => Flow m (a ': l) -> (a -> m x) -> Flow m (x ': l)
(>~^>) f g = f >>= (~^> g)

withFlow ::
   ( Liftable xs zs
   , Liftable (Filter a l) zs
   , zs ~ Fusion xs (Filter a l)
   , Monad m
   , Catchable a l
   ) => Variant l -> (a -> Flow m xs) -> Flow m zs
withFlow v f = case removeType v of
   Left a   -> liftVariant <$> f a
   Right ys -> return (liftVariant ys)

(#~>) ::
   ( Liftable xs zs
   , Liftable (Filter a l) zs
   , zs ~ Fusion xs (Filter a l)
   , Monad m
   , Catchable a l
   ) => Variant l -> (a -> Flow m xs) -> Flow m zs
(#~>) = withFlow

-- | Take the first output, fusion the result
(~#>) ::
   ( Liftable xs zs
   , Liftable l zs
   , zs ~ Fusion xs l
   , Monad m
   ) => Variant (a ': l) -> (a -> Flow m xs) -> Flow m zs
(~#>) v f = case headVariant v of
   Right a -> liftVariant <$> f a
   Left  u -> return (liftVariant u)

-- | Catch all input except the first
(*~^>) ::
   ( Monad m
   ) => Variant (a ': l) -> (Variant l -> m ()) -> Flow m (a ': l)
(*~^>) v f = case headVariant v of
   Right _ -> return v
   Left  u -> f u >> return v

-- | Catch all input except the first
(>*~^>) ::
   ( Monad m
   ) => m (Variant (a ': l)) -> (Variant l -> m ()) -> Flow m (a ': l)
(>*~^>) f g = f >>= (*~^> g)

-- | Take the first output, fusion the result
(>~#>) ::
   ( Liftable xs zs
   , Liftable l zs
   , zs ~ Fusion xs l
   , Monad m
   ) => m (Variant (a ': l)) -> (a -> Flow m xs) -> Flow m zs
(>~#>) f g = f >>= (~#> g)

(#~^>) ::
   ( Liftable '[x] zs
   , Liftable (Filter a l) zs
   , zs ~ Fusion '[x] (Filter a l)
   , Monad m
   , Catchable a l
   ) => Variant l -> (a -> m x) -> Flow m zs
(#~^>) v f = withFlow v (\x -> flowRet' =<< f x)

(#!~>) ::
   ( Liftable (Filter a l) zs
   , zs ~ Filter a l
   , Monad m
   , Catchable a l
   ) => Variant l -> (a -> Flow m zs) -> Flow m zs
(#!~>) v f = case removeType v of
   Left a   -> liftVariant <$> f a
   Right ys -> return (liftVariant ys)

(>#!~>) ::
   ( Liftable (Filter a l) zs
   , zs ~ Filter a l
   , Monad m
   , Catchable a l
   ) => m (Variant l) -> (a -> Flow m zs) -> Flow m zs
(>#!~>) f g = f >>= (#!~> g)

(>#~>) ::
   ( Liftable xs zs
   , Liftable (Filter a l) zs
   , zs ~ Fusion xs (Filter a l)
   , Monad m
   , Catchable a l
   ) => m (Variant l) -> (a -> Flow m xs) -> Flow m zs
(>#~>) f g = f >>= (#~> g)

(>#~^>) ::
   ( Liftable '[x] zs
   , Liftable (Filter a l) zs
   , zs ~ Fusion '[x] (Filter a l)
   , Monad m
   , Catchable a l
   ) => m (Variant l) -> (a -> m x) -> Flow m zs
(>#~^>) f g = f >>= (#~^> g)

flowRet :: Monad m => x -> Flow m (x ': xs)
flowRet = return . setVariant0

flowRet' :: Monad m => x -> Flow m '[x]
flowRet' = flowRet

flowSet :: (Member x xs, Monad m) => x -> Flow m xs
flowSet = return . setVariant

-- | Lift a flow into another
flowLift :: (Liftable xs ys , Monad m) => m (Variant xs) -> m (Variant ys)
flowLift = fmap liftVariant


(#->) :: 
   ( Monad m
   , Catchable a l
   )=> Variant l -> (a -> m ()) -> m ()
(#->) v f = case removeType v of
   Left a  -> f a
   Right _ -> return ()

(>#->) :: 
   ( Monad m
   , Catchable a l
   )=> m (Variant l) -> (a -> m ()) -> m ()
(>#->) f g = f >>= (#-> g)

flowTraverse :: forall m a b xs.
   ( Monad m
   ) => (a -> Flow m (b ': xs)) -> [a] -> Flow m ([b] ': xs)
flowTraverse f = go []
   where
      go :: [b] -> [a] -> Flow m ([b] ': xs)
      go rs []     = flowRet (reverse rs)
      go rs (a:as) = do
         r <- f a
         case headVariant r of
            Right b -> go (b:rs) as
            Left __ -> r ~^> const (return (undefined :: [b]))

flowFor :: forall m a b xs.
   ( Monad m
   ) => [a] -> (a -> Flow m (b ': xs)) -> Flow m ([b] ': xs)
flowFor xs f = flowTraverse f xs
