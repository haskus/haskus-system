{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}

-- | First-class control-flow (based on Variant)
module ViperVM.Utils.Flow
   ( Flow
   , IOV
   -- * Flow utils
   , flowRes
   , flowRet0
   , flowRet1
   , flowRet0'
   , flowSet
   , flowLift
   , flowTraverse
   , flowFor
   , flowTraverseFilter
   , flowForFilter
   , Liftable
   , Catchable
   -- * Non-variant single operations
   , (|>)
   , (<|)
   , (||>)
   , (<||)
   -- * Named operators
   , flowMap
   , flowBind
   , flowBind'
   , flowMatch
   , flowMatchFail
   -- * First element operations
   , (.~.>)
   , (>.~.>)
   , (.~+>)
   , (>.~+>)
   , (.~^^>)
   , (>.~^^>)
   , (.~^>)
   , (>.~^>)
   , (.~$>)
   , (>.~$>)
   , (.~|>)
   , (>.~|>)
   , (.~=>)
   , (>.~=>)
   , (.~!>)
   , (>.~!>)
   -- * First element, pure variant
   , (.-.>)
   , (>.-.>)
   , (<.-.)
   , (<.-.<)
   -- * Functor, applicative equivalents
   , (<$<)
   , (<*<)
   , (<|<)
   -- * First element, const variant
   , (.~~.>)
   , (>.~~.>)
   , (.~~+>)
   , (>.~~+>)
   , (.~~^^>)
   , (>.~~^^>)
   , (.~~^>)
   , (>.~~^>)
   , (.~~$>)
   , (>.~~$>)
   , (.~~|>)
   , (>.~~|>)
   , (.~~=>)
   , (>.~~=>)
   , (.~~!>)
   , (>.~~!>)
   -- * Tail operations
   , (..~.>)
   , (>..~.>)
   , (..-.>)
   , (>..-.>)
   , (..-..>)
   , (>..-..>)
   , (..~..>)
   , (>..~..>)
   , (..~^^>)
   , (>..~^^>)
   , (..~^>)
   , (>..~^>)
   , (..%~^>)
   , (>..%~^>)
   , (..%~^^>)
   , (>..%~^^>)
   , (..~=>)
   , (>..~=>)
   , (..~!>)
   , (>..~!>)
   , (..~!!>)
   , (>..~!!>)
   , (..%~!!>)
   , (>..%~!!>)
   , (..%~!>)
   , (>..%~!>)
   -- * Caught element operations
   , (%~.>)
   , (>%~.>)
   , (%~+>)
   , (>%~+>)
   , (%~^^>)
   , (>%~^^>)
   , (%~^>)
   , (>%~^>)
   , (%~$>)
   , (>%~$>)
   , (%~|>)
   , (>%~|>)
   , (%~=>)
   , (>%~=>)
   , (%~!>)
   , (>%~!>)
   , (%~!!>)
   , (>%~!!>)
   -- * Helpers
   , makeFlowOp
   , makeFlowOpM
   , selectTail
   , selectFirst
   , selectType
   , applyConst
   , applyPure
   , applyM
   , applyF
   , combineFirst
   , combineSameTail
   , combineEither
   , combineConcat
   , combineUnion
   , combineLiftUnselected
   , combineLiftBoth
   , combineSingle
   , liftV
   , liftF
   )
where

import ViperVM.Utils.Variant
import ViperVM.Utils.HList
import ViperVM.Utils.Types

-- | Control-flow
type Flow m (l :: [*]) = m (Variant l)

type IOV l = Flow IO l

----------------------------------------------------------
-- Flow utils
----------------------------------------------------------

-- | Return in the first element
flowRet0 :: Monad m => x -> Flow m (x ': xs)
flowRet0 = return . setVariant0

-- | Return in the second element
flowRet1 :: Monad m => x -> Flow m (y ': x ': xs)
flowRet1 = return . setVariant1

-- | Return a single element
flowRet0' :: Monad m => x -> Flow m '[x]
flowRet0' = flowRet0

-- | Return in the first well-typed element
flowSet :: (Member x xs, Monad m) => x -> Flow m xs
flowSet = return . setVariant

-- | Lift a flow into another
flowLift :: (Liftable xs ys , Monad m) => Flow m xs -> Flow m ys
flowLift = fmap liftVariant

-- | Traverse a list and stop on first error
flowTraverse :: forall m a b xs.
   ( Monad m
   ) => (a -> Flow m (b ': xs)) -> [a] -> Flow m ([b] ': xs)
flowTraverse f = go (flowRet0 [])
   where
      go :: Flow m ([b] ': xs) -> [a] -> Flow m ([b] ': xs)
      go rs []     = rs >.-.> reverse
      go rs (a:as) = go rs' as
         where
            -- execute (f a) if previous execution succedded.
            -- prepend the result to the list
            rs' = rs >.~$> \bs -> (f a >.-.> (:bs))

-- | Traverse a list and stop on first error
flowFor :: forall m a b xs.
   ( Monad m
   ) => [a] -> (a -> Flow m (b ': xs)) -> Flow m ([b] ': xs)
flowFor = flip flowTraverse

-- | Traverse a list and return only valid values
flowTraverseFilter :: forall m a b xs.
   ( Monad m
   ) => (a -> Flow m (b ': xs)) -> [a] -> m [b]
flowTraverseFilter f = go
   where
      go :: [a] -> m [b]
      go []     = return []
      go (a:as) = do
         f a >.~.> (\b -> (b:) <$> go as)
             >..~.> const (go as)

-- | Traverse a list and return only valid values
flowForFilter :: forall m a b xs.
   ( Monad m
   ) => [a] -> (a -> Flow m (b ': xs)) -> m [b]
flowForFilter = flip flowTraverseFilter


-- | Extract single flow result
flowRes :: Functor m => Flow m '[x] -> m x
flowRes = fmap singleVariant


-- | Lift an operation on a Variant into an operation on a flow
liftm :: Monad m => (Variant x -> a -> m b) -> Flow m x -> a -> m b
{-# INLINE liftm #-}
liftm op x a = do
   x' <- x
   op x' a

----------------------------------------------------------
-- Single element not wrapped into a variant
----------------------------------------------------------

-- | Apply a function
(|>) :: a -> (a -> b) -> b
x |> f = f x

infixl 0 |>

-- | Apply a function
(<|) :: (a -> b) -> a -> b
f <| x = f x

infixr 0 <|

-- | Apply a function in a Functor
(||>) :: Functor f => f a -> (a -> b) -> f b
x ||> f = fmap f x

infixl 0 ||>

-- | Apply a function in a Functor
(<||) :: Functor f => (a -> b) -> f a -> f b
f <|| x = fmap f x

infixr 0 <||

----------------------------------------------------------
-- Named operators
----------------------------------------------------------

-- | Map a pure function onto the correct value in the flow
flowMap :: Monad m => Flow m (x ': xs) -> (x -> y) -> Flow m (y ': xs)
flowMap = (>.-.>)

-- | Bind two flows in a monadish way (error types union)
flowBind :: forall xs ys zs m x.
   ( Liftable xs zs
   , Liftable ys zs
   , zs ~ Union xs ys
   , Monad m
   ) => Flow m (x ': ys) -> (x -> Flow m xs) -> Flow m zs
flowBind = (>.~|>)

-- | Bind two flows in a monadic way (constant error types)
flowBind' :: Monad m => Flow m (x ': xs) -> (x -> Flow m (y ': xs)) -> Flow m (y ': xs)
flowBind' = (>.~$>)

-- | Match a value in a flow
flowMatch :: forall x xs zs m.
   ( Monad m
   , Catchable x xs
   , Liftable (Filter x xs) zs
   ) => Flow m xs -> (x -> Flow m zs) -> Flow m zs
flowMatch = (>%~^>)

-- | Match a value in a flow and use a non-returning failure in this case
flowMatchFail :: forall x xs m.
   ( Monad m
   , Catchable x xs
   ) => Flow m xs -> (x -> m ()) -> Flow m (Filter x xs)
flowMatchFail = (>%~!!>)

----------------------------------------------------------
-- First element operations
----------------------------------------------------------

-- | Extract the first value, set the first value
(.~.>) :: forall m l x a.
   ( Monad m )
   => Variant (a ': l) -> (a -> m x) -> Flow m (x ': l)
(.~.>) v f = makeFlowOp selectFirst (applyM f) combineFirst v

infixl 0 .~.>

-- | Extract the first value, set the first value
(>.~.>) :: forall m l x a.
   ( Monad m )
   => Flow m (a ': l) -> (a -> m x) -> Flow m (x ': l)
(>.~.>) = liftm (.~.>)

infixl 0 >.~.>

-- | Extract the first value, concat the result
(.~+>) :: forall (k :: Nat) m l l2 a.
   ( KnownNat k
   , k ~ Length l2
   , a ~ TypeAt 0 (a ': l)
   , Monad m )
   => Variant (a ': l) -> (a -> Flow m l2) -> Flow m (Concat l2 l)
(.~+>) v f = makeFlowOp selectFirst (applyF f) combineConcat v

infixl 0 .~+>

-- | Extract the first value, concat the results
(>.~+>) :: forall (k :: Nat) m l l2 a.
   ( KnownNat k
   , k ~ Length l2
   , Monad m )
   => Flow m (a ': l) -> (a -> Flow m l2) -> Flow m (Concat l2 l)
(>.~+>) = liftm (.~+>)

infixl 0 >.~+>

-- | Extract the first value, lift both
(.~^^>) :: forall m a xs ys zs.
   ( Monad m
   , Liftable xs zs
   , Liftable ys zs
   ) => Variant (a ': ys) -> (a -> Flow m xs) -> Flow m zs
(.~^^>) v f = makeFlowOp selectFirst (applyF f) combineLiftBoth v

infixl 0 .~^^>


-- | Extract the first value, lift both
(>.~^^>) :: forall m a xs ys zs.
   ( Monad m
   , Liftable xs zs
   , Liftable ys zs
   ) => Flow m (a ': ys) -> (a -> Flow m xs) -> Flow m zs
(>.~^^>) = liftm (.~^^>)

infixl 0 >.~^^>

-- | Extract the first value, lift unselected
(.~^>) :: forall m a ys zs.
   ( Monad m
   , Liftable ys zs
   ) => Variant (a ': ys) -> (a -> Flow m zs) -> Flow m zs
(.~^>) v f = makeFlowOp selectFirst (applyF f) combineLiftUnselected v

infixl 0 .~^>

-- | Extract the first value, lift unselected
(>.~^>) :: forall m a ys zs.
   ( Monad m
   , Liftable ys zs
   ) => Flow m (a ': ys) -> (a -> Flow m zs) -> Flow m zs
(>.~^>) = liftm (.~^>)

infixl 0 >.~^>

-- | Extract the first value, use the same tail
(.~$>) :: forall m x xs a.
   ( Monad m
   ) => Variant (a ': xs) -> (a -> Flow m (x ': xs)) -> Flow m (x ': xs)
(.~$>) v f = makeFlowOp selectFirst (applyF f) combineSameTail v

infixl 0 .~$>

-- | Extract the first value, use the same tail
(>.~$>) :: forall m x xs a.
   ( Monad m
   ) => Flow m (a ': xs) -> (a -> Flow m (x ': xs)) -> Flow m (x ': xs)
(>.~$>) = liftm (.~$>)

infixl 0 >.~$>

-- | Take the first output, union the result
(.~|>) ::
   ( Liftable xs zs
   , Liftable ys zs
   , zs ~ Union xs ys
   , Monad m
   ) => Variant (a ': ys) -> (a -> Flow m xs) -> Flow m zs
(.~|>) v f = makeFlowOp selectFirst (applyF f) combineUnion v

infixl 0 .~|>

-- | Take the first output, fusion the result
(>.~|>) ::
   ( Liftable xs zs
   , Liftable ys zs
   , zs ~ Union xs ys
   , Monad m
   ) => Flow m (a ': ys) -> (a -> Flow m xs) -> Flow m zs
(>.~|>) = liftm (.~|>)

infixl 0 >.~|>

-- | Extract the first value and perform effect. Passthrough the input value
(.~=>) ::
   ( Monad m
   ) => Variant (a ': l) -> (a -> m ()) -> Flow m (a ': l)
(.~=>) v f = case headVariant v of
   Right u -> f u >> return v
   Left  _ -> return v

infixl 0 .~=>

-- | Extract the first value and perform effect. Passthrough the input value
(>.~=>) ::
   ( Monad m
   ) => Flow m (a ': l) -> (a -> m ()) -> Flow m (a ': l)
(>.~=>) = liftm (.~=>)

infixl 0 >.~=>

-- | Extract the first value and perform effect.
(.~!>) ::
   ( Monad m
   ) => Variant (a ': l) -> (a -> m ()) -> m ()
(.~!>) v f = case headVariant v of
   Right u -> f u
   Left  _ -> return ()

infixl 0 .~!>

-- | Extract the first value and perform effect.
(>.~!>) ::
   ( Monad m
   ) => Flow m (a ': l) -> (a -> m ()) -> m ()
(>.~!>) = liftm (.~!>)

infixl 0 >.~!>

----------------------------------------------------------
-- First element, pure variant
----------------------------------------------------------

-- | Extract the first value, set the first value
(.-.>) :: forall m l x a.
   ( Monad m )
   => Variant (a ': l) -> (a -> x) -> Flow m (x ': l)
(.-.>) v f = makeFlowOp selectFirst (applyPure (liftV f)) combineFirst v

infixl 0 .-.>

-- | Extract the first value, set the first value
(>.-.>) :: forall m l x a.
   ( Monad m )
   => Flow m (a ': l) -> (a -> x) -> Flow m (x ': l)
(>.-.>) = liftm (.-.>)

infixl 0 >.-.>

-- | Extract the first value, set the first value
(<.-.) :: forall m l x a.
   ( Monad m )
   => (a -> x) -> Variant (a ': l) -> Flow m (x ': l)
(<.-.) = flip (.-.>)

infixr 0 <.-.

-- | Extract the first value, set the first value
(<.-.<) :: forall m l x a.
   ( Monad m )
   => (a -> x) -> Flow m (a ': l) -> Flow m (x ': l)
(<.-.<) = flip (>.-.>)

infixr 0 <.-.<

----------------------------------------------------------
-- Functor, applicative
----------------------------------------------------------

-- | Functor <$> equivalent
(<$<) :: forall m l a b.
   ( Monad m )
   => (a -> b) -> Flow m (a ': l) -> Flow m (b ': l)
(<$<) = (<.-.<)

infixl 4 <$<

-- | Applicative <*> equivalent
(<*<) :: forall m l a b.
   ( Monad m )
   => Flow m ((a -> b) ': l) -> Flow m (a ': l) -> Flow m (b ': l)
(<*<) mf mg = mf >.~$> (mg >.-.>)

infixl 4 <*<

-- | Applicative <*> equivalent, with error union
(<|<) :: forall m xs ys zs y z.
   ( Monad m
   , Liftable xs zs
   , Liftable ys zs
   , zs ~ Union xs ys
   ) => Flow m ((y -> z) ': xs) -> Flow m (y ': ys) -> Flow m (z ': zs)
(<|<) mf mg = 
   mf >..-..> liftVariant
      >.~$> (\f -> mg >..-..> liftVariant
                      >.-.> f
            )

infixl 4 <|<

----------------------------------------------------------
-- First element, const variant
----------------------------------------------------------

-- | Extract the first value, set the first value
(.~~.>) :: forall m l x a.
   ( Monad m )
   => Variant (a ': l) -> m x -> Flow m (x ': l)
(.~~.>) v f = v .~.> const f

infixl 0 .~~.>

-- | Extract the first value, set the first value
(>.~~.>) :: forall m l x a.
   ( Monad m )
   => Flow m (a ': l) -> m x -> Flow m (x ': l)
(>.~~.>) = liftm (.~~.>)

infixl 0 >.~~.>

-- | Extract the first value, concat the result
(.~~+>) :: forall (k :: Nat) m l l2 a.
   ( KnownNat k
   , k ~ Length l2
   , a ~ TypeAt 0 (a ': l)
   , Monad m )
   => Variant (a ': l) -> Flow m l2 -> Flow m (Concat l2 l)
(.~~+>) v f = v .~+> const f

infixl 0 .~~+>

-- | Extract the first value, concat the results
(>.~~+>) :: forall (k :: Nat) m l l2 a.
   ( KnownNat k
   , k ~ Length l2
   , Monad m )
   => Flow m (a ': l) -> Flow m l2 -> Flow m (Concat l2 l)
(>.~~+>) = liftm (.~~+>)

infixl 0 >.~~+>

-- | Extract the first value, lift the result
(.~~^^>) :: forall m a xs ys zs.
   ( Monad m
   , Liftable xs zs
   , Liftable ys zs
   ) => Variant (a ': ys) -> Flow m xs -> Flow m zs
(.~~^^>) v f = v .~^^> const f

infixl 0 .~~^^>


-- | Extract the first value, lift the result
(>.~~^^>) :: forall m a xs ys zs.
   ( Monad m
   , Liftable xs zs
   , Liftable ys zs
   ) => Flow m (a ': ys) -> Flow m xs -> Flow m zs
(>.~~^^>) = liftm (.~~^^>)

infixl 0 >.~~^^>

-- | Extract the first value, connect to the expected output
(.~~^>) :: forall m a ys zs.
   ( Monad m
   , Liftable ys zs
   ) => Variant (a ': ys) -> Flow m zs -> Flow m zs
(.~~^>) v f = v .~^> const f

infixl 0 .~~^>

-- | Extract the first value, connect to the expected output
(>.~~^>) :: forall m a ys zs.
   ( Monad m
   , Liftable ys zs
   ) => Flow m (a ': ys) -> Flow m zs -> Flow m zs
(>.~~^>) = liftm (.~~^>)

infixl 0 >.~~^>

-- | Extract the first value, use the same output type
(.~~$>) :: forall m x xs a.
   ( Monad m
   ) => Variant (a ': xs) -> Flow m (x ': xs) -> Flow m (x ': xs)
(.~~$>) v f = v .~$> const f

infixl 0 .~~$>

-- | Extract the first value, use the same output type
(>.~~$>) :: forall m x xs a.
   ( Monad m
   ) => Flow m (a ': xs) -> Flow m (x ': xs) -> Flow m (x ': xs)
(>.~~$>) = liftm (.~~$>)

infixl 0 >.~~$>

-- | Take the first output, fusion the result
(.~~|>) ::
   ( Liftable xs zs
   , Liftable ys zs
   , zs ~ Union xs ys
   , Monad m
   ) => Variant (a ': ys) -> Flow m xs -> Flow m zs
(.~~|>) v f = v .~|> const f

infixl 0 .~~|>

-- | Take the first output, fusion the result
(>.~~|>) ::
   ( Liftable xs zs
   , Liftable ys zs
   , zs ~ Union xs ys
   , Monad m
   ) => Flow m (a ': ys) -> Flow m xs -> Flow m zs
(>.~~|>) = liftm (.~~|>)

infixl 0 >.~~|>

-- | Extract the first value and perform effect. Passthrough the input value
(.~~=>) ::
   ( Monad m
   ) => Variant (a ': l) -> m () -> Flow m (a ': l)
(.~~=>) v f = v .~=> const f

infixl 0 .~~=>

-- | Extract the first value and perform effect. Passthrough the input value
(>.~~=>) ::
   ( Monad m
   ) => Flow m (a ': l) -> m () -> Flow m (a ': l)
(>.~~=>) = liftm (.~~=>)

infixl 0 >.~~=>

-- | Extract the first value and perform effect.
(.~~!>) ::
   ( Monad m
   ) => Variant (a ': l) -> m () -> m ()
(.~~!>) v f = v .~!> const f

infixl 0 .~~!>

-- | Extract the first value and perform effect.
(>.~~!>) ::
   ( Monad m
   ) => Flow m (a ': l) -> m () -> m ()
(>.~~!>) = liftm (.~~!>)

infixl 0 >.~~!>


----------------------------------------------------------
-- Tail operations
----------------------------------------------------------

-- | Extract the tail, set the first value
(..~.>) ::
   ( Monad m
   ) => Variant (a ': l) -> (Variant l -> m a) -> m a
(..~.>) v f = makeFlowOp selectTail (applyVM f) combineSingle v

infixl 0 ..~.>

-- | Extract the tail, set the first value
(>..~.>) ::
   ( Monad m
   ) => Flow m (a ': l) -> (Variant l -> m a) -> m a
(>..~.>) = liftm (..~.>)

infixl 0 >..~.>

-- | Extract the tail, set the first value (pure function)
(..-.>) ::
   ( Monad m
   ) => Variant (a ': l) -> (Variant l -> a) -> Flow m '[a]
(..-.>) v f = case headVariant v of
   Right u -> flowRet0 u
   Left  l -> flowRet0 (f l)

infixl 0 ..-.>

-- | Extract the tail, set the first value (pure function)
(>..-.>) ::
   ( Monad m
   ) => Flow m (a ': l) -> (Variant l -> a) -> Flow m '[a]
(>..-.>) = liftm (..-.>)

infixl 0 >..-.>

-- | Extract the tail, set the tail
(..-..>) ::
   ( Monad m
   ) => Variant (a ': l) -> (Variant l -> Variant xs) -> Flow m (a ': xs)
(..-..>) v f = case headVariant v of
   Right u -> flowRet0 u
   Left  l -> return (prependVariant (Proxy :: Proxy '[a]) (f l))

infixl 0 ..-..>

-- | Extract the tail, set the tail
(>..-..>) ::
   ( Monad m
   ) => Flow m (a ': l) -> (Variant l -> Variant xs) -> Flow m (a ': xs)
(>..-..>) = liftm (..-..>)

infixl 0 >..-..>

-- | Extract the tail, set the tail
(..~..>) ::
   ( Monad m
   ) => Variant (a ': l) -> (Variant l -> Flow m xs) -> Flow m (a ': xs)
(..~..>) v f = case headVariant v of
   Right u -> flowRet0 u
   Left  l -> prependVariant (Proxy :: Proxy '[a]) <$> f l

infixl 0 ..~..>

-- | Extract the tail, set the tail
(>..~..>) ::
   ( Monad m
   ) => Flow m (a ': l) -> (Variant l -> Flow m xs) -> Flow m (a ': xs)
(>..~..>) = liftm (..~..>)

infixl 0 >..~..>

-- | Extract the tail, lift the result
(..~^^>) ::
   ( Monad m
   , Liftable xs (a ': zs)
   ) => Variant (a ': l) -> (Variant l -> Flow m xs) -> Flow m (a ': zs)
(..~^^>) v f = case headVariant v of
   Right u -> flowRet0 u
   Left  l -> liftVariant <$> f l

infixl 0 ..~^^>

-- | Extract the tail, lift the result
(>..~^^>) ::
   ( Monad m
   , Liftable xs (a ': zs)
   ) => Flow m  (a ': l) -> (Variant l -> Flow m xs) -> Flow m (a ': zs)
(>..~^^>) = liftm (..~^^>)

infixl 0 >..~^^>

-- | Extract the tail, connect the result
(..~^>) ::
   ( Monad m
   , Member a zs
   ) => Variant (a ': l) -> (Variant l -> Flow m zs) -> Flow m zs
(..~^>) v f = case headVariant v of
   Right u -> flowSet u
   Left  l -> f l

infixl 0 ..~^>

-- | Extract the tail, connect the result
(>..~^>) ::
   ( Monad m
   , Member a zs
   ) => Flow m (a ': l) -> (Variant l -> Flow m zs) -> Flow m zs
(>..~^>) = liftm (..~^>)

infixl 0 >..~^>

-- | Match in the tail, connect to the expected result
(..%~^>) ::
   ( Monad m
   , Catchable a xs
   , Liftable (Filter a xs) ys
   ) => Variant (x ': xs) -> (a -> Flow m ys) -> Flow m (x ': ys)
(..%~^>) v f = v ..~..> (\v' -> v' %~^> f)

infixl 0 ..%~^>

-- | Match in the tail, connect to the expected result
(>..%~^>) ::
   ( Monad m
   , Catchable a xs
   , Liftable (Filter a xs) ys
   ) => Flow m (x ': xs) -> (a -> Flow m ys) -> Flow m (x ': ys)
(>..%~^>) = liftm (..%~^>)

infixl 0 >..%~^>

-- | Match in the tail, lift to the expected result
(..%~^^>) ::
   ( Monad m
   , Catchable a xs
   , Liftable (Filter a xs) zs
   , Liftable ys zs
   ) => Variant (x ': xs) -> (a -> Flow m ys) -> Flow m (x ': zs)
(..%~^^>) v f = v ..~..> (\v' -> v' %~^^> f)

infixl 0 ..%~^^>

-- | Match in the tail, lift to the expected result
(>..%~^^>) ::
   ( Monad m
   , Catchable a xs
   , Liftable (Filter a xs) zs
   , Liftable ys zs
   ) => Flow m (x ': xs) -> (a -> Flow m ys) -> Flow m (x ': zs)
(>..%~^^>) = liftm (..%~^^>)

infixl 0 >..%~^^>

-- | Extract the tail and perform an effect. Passthrough the input value
(..~=>) ::
   ( Monad m
   ) => Variant (x ': xs) -> (Variant xs -> m ()) -> Flow m (x ': xs)
(..~=>) v f = case headVariant v of
   Right _ -> return v
   Left  l -> f l >> return v

infixl 0 ..~=>

-- | Extract the tail and perform an effect. Passthrough the input value
(>..~=>) ::
   ( Monad m
   ) => Flow m (x ': xs) -> (Variant xs -> m ()) -> Flow m (x ': xs)
(>..~=>) = liftm (..~=>)

infixl 0 >..~=>

-- | Extract the tail and perform an effect
(..~!>) ::
   ( Monad m
   ) => Variant (x ': xs) -> (Variant xs -> m ()) -> m ()
(..~!>) v f = case headVariant v of
   Right _ -> return ()
   Left  l -> f l

infixl 0 ..~!>

-- | Extract the tail and perform an effect
(>..~!>) ::
   ( Monad m
   ) => Flow m (x ': xs) -> (Variant xs -> m ()) -> m ()
(>..~!>) = liftm (..~!>)

infixl 0 >..~!>

-- | Extract the tail and perform an effect
(..~!!>) ::
   ( Monad m
   ) => Variant (x ': xs) -> (Variant xs -> m ()) -> m x
(..~!!>) v f = case headVariant v of
   Right x -> return x
   Left xs -> f xs >> error "..~!!> error"

infixl 0 ..~!!>

-- | Extract the tail and perform an effect
(>..~!!>) ::
   ( Monad m
   ) => Flow m (x ': xs) -> (Variant xs -> m ()) -> m x
(>..~!!>) = liftm (..~!!>)

infixl 0 >..~!!>

-- | Match in the tail and perform an effect
(..%~!!>) ::
   ( Monad m
   , Catchable y xs
   ) => Variant (x ': xs) -> (y -> m ()) -> Flow m (x ': Filter y xs)
(..%~!!>) v f = v ..~..> (\xs -> xs %~!!> f)

infixl 0 ..%~!!>

-- | Match in the tail and perform an effect
(>..%~!!>) ::
   ( Monad m
   , Catchable y xs
   ) => Flow m (x ': xs) -> (y -> m ()) -> Flow m (x ': Filter y xs)
(>..%~!!>) = liftm (..%~!!>)

infixl 0 >..%~!!>

-- | Match in the tail and perform an effect
(..%~!>) ::
   ( Monad m
   , Catchable y xs
   ) => Variant (x ': xs) -> (y -> m ()) -> m ()
(..%~!>) v f = case headVariant v of
   Right _ -> return ()
   Left xs -> xs %~!> f

infixl 0 ..%~!>

-- | Match in the tail and perform an effect
(>..%~!>) ::
   ( Monad m
   , Catchable y xs
   ) => Flow m (x ': xs) -> (y -> m ()) -> m ()
(>..%~!>) = liftm (..%~!>)

infixl 0 >..%~!>

----------------------------------------------------------
-- Caught element operations
----------------------------------------------------------

-- | Catch element, set the first value
(%~.>) ::
   ( ys ~ Filter x xs
   , Monad m
   , Catchable x xs
   ) => Variant xs -> (x -> m y) -> Flow m (y ': ys)
(%~.>) v f = case catchVariant v of
   Right x -> flowRet0 =<< f x
   Left ys -> prependVariant (Proxy :: Proxy '[y]) <$> return ys

infixl 0 %~.>

-- | Catch element, set the first value
(>%~.>) ::
   ( ys ~ Filter x xs
   , Monad m
   , Catchable x xs
   ) => Flow m xs -> (x -> m y) -> Flow m (y ': ys)
(>%~.>) = liftm (%~.>)

infixl 0 >%~.>

-- | Catch element, concat the result
(%~+>) :: forall x xs ys m.
   ( Monad m
   , Catchable x xs
   , KnownNat (Length ys)
   ) => Variant xs -> (x -> Flow m ys) -> Flow m (Concat ys (Filter x xs))
(%~+>) v f = case catchVariant v of
   Right x -> appendVariant  (Proxy :: Proxy (Filter x xs)) <$> f x
   Left ys -> prependVariant (Proxy :: Proxy ys)            <$> return ys

infixl 0 %~+>

-- | Catch element, concat the result
(>%~+>) :: forall x xs ys m.
   ( Monad m
   , Catchable x xs
   , KnownNat (Length ys)
   ) => Flow m xs -> (x -> Flow m ys) -> Flow m (Concat ys (Filter x xs))
(>%~+>) = liftm (%~+>)

infixl 0 >%~+>

-- | Catch element, lift the result
(%~^^>) :: forall x xs ys zs m.
   ( Monad m
   , Catchable x xs
   , Liftable (Filter x xs) zs
   , Liftable ys zs
   ) => Variant xs -> (x -> Flow m ys) -> Flow m zs
(%~^^>) v f = case catchVariant v of
   Right x -> liftVariant <$> f x
   Left ys -> liftVariant <$> return ys

infixl 0 %~^^>

-- | Catch element, lift the result
(>%~^^>) :: forall x xs ys zs m.
   ( Monad m
   , Catchable x xs
   , Liftable (Filter x xs) zs
   , Liftable ys zs
   ) => Flow m xs -> (x -> Flow m ys) -> Flow m zs
(>%~^^>) = liftm (%~^^>)

infixl 0 >%~^^>

-- | Catch element, connect to the expected output
(%~^>) :: forall x xs zs m.
   ( Monad m
   , Catchable x xs
   , Liftable (Filter x xs) zs
   ) => Variant xs -> (x -> Flow m zs) -> Flow m zs
(%~^>) v f = case catchVariant v of
   Right x -> f x
   Left ys -> return (liftVariant ys)

infixl 0 %~^>

-- | Catch element, connect to the expected output
(>%~^>) :: forall x xs zs m.
   ( Monad m
   , Catchable x xs
   , Liftable (Filter x xs) zs
   ) => Flow m xs -> (x -> Flow m zs) -> Flow m zs
(>%~^>) = liftm (%~^>)

infixl 0 >%~^>

-- | Catch element, use the same output type
(%~$>) :: forall x xs m.
   ( Monad m
   , Catchable x xs
   ) => Variant xs -> (x -> Flow m xs) -> Flow m xs
(%~$>) v f = case catchVariant v of
   Right x -> f x
   Left _  -> return v

infixl 0 %~$>

-- | Catch element, use the same output type
(>%~$>) :: forall x xs m.
   ( Monad m
   , Catchable x xs
   ) => Flow m xs -> (x -> Flow m xs) -> Flow m xs
(>%~$>) = liftm (%~$>)

infixl 0 >%~$>

-- | Catch element, fusion the result
(%~|>) :: forall x xs ys zs m.
   ( Monad m
   , Catchable x xs
   , Liftable (Filter x xs) zs
   , Liftable ys zs
   , zs ~ Union (Filter x xs) ys
   ) => Variant xs -> (x -> Flow m ys) -> Flow m zs
(%~|>) v f = case catchVariant v of
   Right x -> liftVariant <$> f x
   Left ys -> return (liftVariant ys)

infixl 0 %~|>

-- | Catch element, fusion the result
(>%~|>) :: forall x xs ys zs m.
   ( Monad m
   , Catchable x xs
   , Liftable (Filter x xs) zs
   , Liftable ys zs
   , zs ~ Union (Filter x xs) ys
   ) => Flow m xs -> (x -> Flow m ys) -> Flow m zs
(>%~|>) = liftm (%~|>)

infixl 0 >%~|>

-- | Catch element and perform effect. Passthrough the input value.
(%~=>) :: forall x xs m.
   ( Monad m
   , Catchable x xs
   ) => Variant xs -> (x -> m ()) -> Flow m xs
(%~=>) v f = case catchVariant v of
   Right x -> f x >> return v
   Left _  -> return v

infixl 0 %~=>

-- | Catch element and perform effect. Passthrough the input value.
(>%~=>) :: forall x xs m.
   ( Monad m
   , Catchable x xs
   ) => Flow m xs -> (x -> m ()) -> Flow m xs
(>%~=>) = liftm (%~=>)

infixl 0 >%~=>

-- | Catch element and perform effect.
(%~!>) :: forall x xs m.
   ( Monad m
   , Catchable x xs
   ) => Variant xs -> (x -> m ()) -> m ()
(%~!>) v f = case catchVariant v of
   Right x -> f x
   Left _  -> return ()

infixl 0 %~!>

-- | Catch element and perform effect.
(>%~!>) :: forall x xs m.
   ( Monad m
   , Catchable x xs
   ) => Flow m xs -> (x -> m ()) -> m ()
(>%~!>) = liftm (%~!>)

infixl 0 >%~!>

-- | Catch element and perform effect.
(%~!!>) :: forall x xs m.
   ( Monad m
   , Catchable x xs
   ) => Variant xs -> (x -> m ()) -> Flow m (Filter x xs)
(%~!!>) v f = case catchVariant v of
   Right x -> f x >> error "%~!!> error"
   Left u  -> return u

infixl 0 %~!!>

-- | Catch element and perform effect.
(>%~!!>) :: forall x xs m.
   ( Monad m
   , Catchable x xs
   ) => Flow m xs -> (x -> m ()) -> Flow m (Filter x xs)
(>%~!!>) = liftm (%~!!>)

infixl 0 >%~!!>

--------------------------------------------------------------
-- Helpers
--------------------------------------------------------------


-- | Make a flow operator
makeFlowOp :: Monad m =>
      (Variant as -> Either (Variant bs) (Variant cs))
      -> (Variant cs -> Flow m ds)
      -> (Either (Variant bs) (Variant ds) -> es)
      -> Variant as -> m es
{-# INLINE makeFlowOp #-}
makeFlowOp select apply combine v = combine <$> traverse apply (select v)

-- | Make a flow operator
makeFlowOpM :: Monad m =>
      (Variant as -> Either (Variant bs) (Variant cs))
      -> (Variant cs -> Flow m ds)
      -> (Either (Variant bs) (Variant ds) -> es)
      -> Flow m as -> m es
{-# INLINE makeFlowOpM #-}
makeFlowOpM select apply combine v = v >>= makeFlowOp select apply combine


-- | Select the first value
selectFirst :: Variant (x ': xs) -> Either (Variant xs) (Variant '[x])
{-# INLINE selectFirst #-}
selectFirst = fmap setVariant0 . headVariant

-- | Select the tail
selectTail :: Variant (x ': xs) -> Either (Variant '[x]) (Variant xs)
{-# INLINE selectTail #-}
selectTail = flipEither . selectFirst
   where
      flipEither (Left x)  = Right x
      flipEither (Right x) = Left x

-- | Select by type
selectType ::
   ( Catchable x xs
   ) => Variant xs -> Either (Variant (Filter x xs)) (Variant '[x])
{-# INLINE selectType #-}
selectType = fmap setVariant0 . catchVariant

-- | Const application
applyConst :: Flow m ys -> (Variant xs -> Flow m ys)
{-# INLINE applyConst #-}
applyConst = const

-- | Pure application
applyPure :: Monad m => (Variant xs -> Variant ys) -> Variant xs -> Flow m ys
{-# INLINE applyPure #-}
applyPure f = return . f

-- | Lift a monadic function
applyM :: Monad m => (a -> m b) -> Variant '[a] -> Flow m '[b]
{-# INLINE applyM #-}
applyM = liftF

-- | Lift a monadic function
applyVM :: Monad m => (Variant a -> m b) -> Variant a -> Flow m '[b]
{-# INLINE applyVM #-}
applyVM f = fmap setVariant0 . f

-- | Lift a monadic function
applyF :: Monad m => (a -> Flow m b) -> Variant '[a] -> Flow m b
{-# INLINE applyF #-}
applyF f = f . singleVariant

-- | Set the first value (the "correct" one)
combineFirst :: forall x xs. Either (Variant xs) (Variant '[x]) -> Variant (x ': xs)
{-# INLINE combineFirst #-}
combineFirst = \case
   Right x -> appendVariant (Proxy :: Proxy xs) x
   Left xs -> prependVariant (Proxy :: Proxy '[x]) xs

-- | Set the first value, keep the same tail type 
combineSameTail :: Either (Variant xs) (Variant (x ': xs)) -> Variant (x ': xs)
{-# INLINE combineSameTail #-}
combineSameTail = \case
   Right x -> x
   Left xs -> prependVariant (Proxy :: Proxy '[x]) xs

-- | Return the valid variant unmodified
combineEither :: Either (Variant xs) (Variant xs) -> Variant xs
{-# INLINE combineEither #-}
combineEither = \case
   Right x -> x
   Left x  -> x

-- | Concatenate unselected values
combineConcat :: forall xs ys.
   ( KnownNat (Length xs)
   ) => Either (Variant ys) (Variant xs) -> Variant (Concat xs ys)
{-# INLINE combineConcat #-}
combineConcat = \case
   Right xs -> appendVariant (Proxy :: Proxy ys) xs
   Left ys  -> prependVariant (Proxy :: Proxy xs) ys

-- | Union
combineUnion ::
   ( Liftable xs (Union xs ys)
   , Liftable ys (Union xs ys)
   ) => Either (Variant ys) (Variant xs) -> Variant (Union xs ys)
{-# INLINE combineUnion #-}
combineUnion = \case
   Right xs -> liftVariant xs
   Left  ys -> liftVariant ys

-- | Lift unselected
combineLiftUnselected ::
   ( Liftable ys xs
   ) => Either (Variant ys) (Variant xs) -> Variant xs
{-# INLINE combineLiftUnselected #-}
combineLiftUnselected = \case
   Right xs -> xs
   Left ys  -> liftVariant ys

-- | Lift both
combineLiftBoth ::
   ( Liftable ys zs
   , Liftable xs zs
   ) => Either (Variant ys) (Variant xs) -> Variant zs
{-# INLINE combineLiftBoth #-}
combineLiftBoth = \case
   Right xs -> liftVariant xs
   Left ys  -> liftVariant ys

-- | Single value
combineSingle :: Either (Variant '[x]) (Variant '[x]) -> x
{-# INLINE combineSingle #-}
combineSingle = \case
   Right x -> singleVariant x
   Left  x -> singleVariant x


-- | Lift a pure function into a Variant to Variant function
liftV :: (a -> b) -> Variant '[a] -> Variant '[b]
liftV = updateVariant0

-- | Lift a function into a Flow
liftF :: Monad m => (a -> m b) -> Variant '[a] -> Flow m '[b]
liftF = updateVariantM (Proxy :: Proxy 0)
