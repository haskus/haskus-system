{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

-- | First-class control-flow (based on Variant)
module ViperVM.Utils.Flow
   ( Flow
   , IOV
   -- * Flow utils
   , flowRes
   , flowRet
   , flowRet1
   , flowRet'
   , flowSet
   , flowLift
   , flowTraverse
   , flowFor
   , Liftable
   , Catchable
   -- * Non-variant single operations
   , (|>)
   , (<|)
   -- * First element operations
   , (.~.>)
   , (>.~.>)
   , (.-.>)
   , (>.-.>)
   , (<.-.)
   , (<.-.<)
   , (.~:>)
   , (>.~:>)
   , (.~^^>)
   , (>.~^^>)
   , (.~#>)
   , (>.~#>)
   , (.~->)
   , (>.~->)
   , (.~&>)
   , (>.~&>)
   , (.~=>)
   , (>.~=>)
   , (.~!>)
   , (>.~!>)
   -- * Tail operations
   , (..~.>)
   , (>..~.>)
   , (..-.>)
   , (>..-.>)
   , (..~..>)
   , (>..~..>)
   , (..~^^>)
   , (>..~^^>)
   , (..~#>)
   , (>..~#>)
   , (..%~#>)
   , (>..%~#>)
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
   , (%~:>)
   , (>%~:>)
   , (%~^^>)
   , (>%~^^>)
   , (%~#>)
   , (>%~#>)
   , (%~->)
   , (>%~->)
   , (%~&>)
   , (>%~&>)
   , (%~=>)
   , (>%~=>)
   , (%~!>)
   , (>%~!>)
   , (%~!!>)
   , (>%~!!>)
   )
where

import ViperVM.Utils.Variant
import ViperVM.Utils.HList
import Data.Proxy
import GHC.TypeLits

-- | Control-flow
type Flow m (l :: [*]) = m (Variant l)

type IOV l = Flow IO l

----------------------------------------------------------
-- Flow utils
----------------------------------------------------------

-- | Return in the first element
flowRet :: Monad m => x -> Flow m (x ': xs)
flowRet = return . setVariant0

-- | Return in the second element
flowRet1 :: Monad m => x -> Flow m (y ': x ': xs)
flowRet1 = return . setVariant1

-- | Return a single element
flowRet' :: Monad m => x -> Flow m '[x]
flowRet' = flowRet

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
flowTraverse f = go (flowRet [])
   where
      go :: Flow m ([b] ': xs) -> [a] -> Flow m ([b] ': xs)
      go rs []     = rs >.-.> reverse
      go rs (a:as) = go rs' as
         where
            -- execute (f a) if previous execution succedded.
            -- prepend the result to the list
            rs' = rs >.~-> \bs -> (f a >.-.> (:bs))

-- | Traverse a list and stop on first error
flowFor :: forall m a b xs.
   ( Monad m
   ) => [a] -> (a -> Flow m (b ': xs)) -> Flow m ([b] ': xs)
flowFor = flip flowTraverse

-- | Extract single flow result
flowRes :: Functor m => Flow m '[x] -> m x
flowRes = fmap singleVariant


-- | Lift an operation on a Variant into an operation on a flow
liftm :: Monad m => (Variant x -> a -> m b) -> Flow m x -> a -> m b
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

----------------------------------------------------------
-- First element operations
----------------------------------------------------------

-- | Extract the first value, set the first value
(.~.>) :: forall m l x a.
   ( Monad m )
   => Variant (a ': l) -> (a -> m x) -> Flow m (x ': l)
(.~.>) v f = updateVariantM (Proxy :: Proxy 0) f v

infixl 0 .~.>

-- | Extract the first value, set the first value
(>.~.>) :: forall m l x a.
   ( Monad m )
   => Flow m (a ': l) -> (a -> m x) -> Flow m (x ': l)
(>.~.>) = liftm (.~.>)

infixl 0 >.~.>

-- | Extract the first value, set the first value
(.-.>) :: forall m l x a.
   ( Monad m )
   => Variant (a ': l) -> (a -> x) -> Flow m (x ': l)
(.-.>) v f = return (updateVariant (Proxy :: Proxy 0) f v)

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

-- | Extract the first value, concat the result
(.~:>) :: forall (k :: Nat) m l l2 a.
   ( KnownNat k
   , k ~ Length l2
   , a ~ TypeAt 0 (a ': l)
   , Monad m )
   => Variant (a ': l) -> (a -> Flow m l2) -> Flow m (Concat l2 l)
(.~:>) v f= updateVariantFoldM (Proxy :: Proxy 0) f v

infixl 0 .~:>

-- | Extract the first value, concat the results
(>.~:>) :: forall (k :: Nat) m l l2 a.
   ( KnownNat k
   , k ~ Length l2
   , Monad m )
   => Flow m (a ': l) -> (a -> Flow m l2) -> Flow m (Concat l2 l)
(>.~:>) = liftm (.~:>)

infixl 0 >.~:>

-- | Extract the first value, lift the result
(.~^^>) :: forall m a xs ys zs.
   ( Monad m
   , Liftable xs zs
   , Liftable ys zs
   ) => Variant (a ': ys) -> (a -> Flow m xs) -> Flow m zs
(.~^^>) v f = case headVariant v of
   Right a -> liftVariant <$> f a
   Left ys -> return (liftVariant ys)

infixl 0 .~^^>


-- | Extract the first value, lift the result
(>.~^^>) :: forall m a xs ys zs.
   ( Monad m
   , Liftable xs zs
   , Liftable ys zs
   ) => Flow m (a ': ys) -> (a -> Flow m xs) -> Flow m zs
(>.~^^>) = liftm (.~^^>)

infixl 0 >.~^^>

-- | Extract the first value, connect to the expected output
(.~#>) :: forall m a ys zs.
   ( Monad m
   , Liftable ys zs
   ) => Variant (a ': ys) -> (a -> Flow m zs) -> Flow m zs
(.~#>) v f = case headVariant v of
   Right a -> f a
   Left ys -> return (liftVariant ys)

infixl 0 .~#>

-- | Extract the first value, connect to the expected output
(>.~#>) :: forall m a ys zs.
   ( Monad m
   , Liftable ys zs
   ) => Flow m (a ': ys) -> (a -> Flow m zs) -> Flow m zs
(>.~#>) = liftm (.~#>)

infixl 0 >.~#>

-- | Extract the first value, use the same output type
(.~->) :: forall m x xs.
   ( Monad m
   ) => Variant (x ': xs) -> (x -> Flow m (x ': xs)) -> Flow m (x ': xs)
(.~->) v f = case headVariant v of
   Right a -> f a
   Left _  -> return v

infixl 0 .~->

-- | Extract the first value, use the same output type
(>.~->) :: forall m x xs.
   ( Monad m
   ) => Flow m (x ': xs) -> (x -> Flow m (x ': xs)) -> Flow m (x ': xs)
(>.~->) = liftm (.~->)

infixl 0 >.~->

-- | Take the first output, fusion the result
(.~&>) ::
   ( Liftable xs zs
   , Liftable ys zs
   , zs ~ Fusion xs ys
   , Monad m
   ) => Variant (a ': ys) -> (a -> Flow m xs) -> Flow m zs
(.~&>) v f = case headVariant v of
   Right a -> liftVariant <$> f a
   Left ys -> return (liftVariant ys)

infixl 0 .~&>

-- | Take the first output, fusion the result
(>.~&>) ::
   ( Liftable xs zs
   , Liftable ys zs
   , zs ~ Fusion xs ys
   , Monad m
   ) => Flow m (a ': ys) -> (a -> Flow m xs) -> Flow m zs
(>.~&>) = liftm (.~&>)

infixl 0 >.~&>

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
-- Tail operations
----------------------------------------------------------

-- | Extract the tail, set the first value
(..~.>) ::
   ( Monad m
   ) => Variant (a ': l) -> (Variant l -> Flow m '[a]) -> Flow m '[a]
(..~.>) v f = case headVariant v of
   Right u -> flowRet u
   Left  l -> f l

infixl 0 ..~.>

-- | Extract the tail, set the first value
(>..~.>) ::
   ( Monad m
   ) => Flow m (a ': l) -> (Variant l -> Flow m '[a]) -> Flow m '[a]
(>..~.>) = liftm (..~.>)

infixl 0 >..~.>

-- | Extract the tail, set the first value (pure function)
(..-.>) ::
   ( Monad m
   ) => Variant (a ': l) -> (Variant l -> a) -> Flow m '[a]
(..-.>) v f = case headVariant v of
   Right u -> flowRet u
   Left  l -> flowRet (f l)

infixl 0 ..-.>

-- | Extract the tail, set the first value (pure function)
(>..-.>) ::
   ( Monad m
   ) => Flow m (a ': l) -> (Variant l -> a) -> Flow m '[a]
(>..-.>) = liftm (..-.>)

infixl 0 >..-.>

-- | Extract the tail, set the tail
(..~..>) ::
   ( Monad m
   ) => Variant (a ': l) -> (Variant l -> Flow m xs) -> Flow m (a ': xs)
(..~..>) v f = case headVariant v of
   Right u -> flowRet u
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
   Right u -> flowRet u
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
(..~#>) ::
   ( Monad m
   , Member a zs
   ) => Variant (a ': l) -> (Variant l -> Flow m zs) -> Flow m zs
(..~#>) v f = case headVariant v of
   Right u -> flowSet u
   Left  l -> f l

infixl 0 ..~#>

-- | Extract the tail, connect the result
(>..~#>) ::
   ( Monad m
   , Member a zs
   ) => Flow m (a ': l) -> (Variant l -> Flow m zs) -> Flow m zs
(>..~#>) = liftm (..~#>)

infixl 0 >..~#>

-- | Match in the tail, connect to the expected result
(..%~#>) ::
   ( Monad m
   , Catchable a xs
   , Liftable (Filter a xs) ys
   ) => Variant (x ': xs) -> (a -> Flow m ys) -> Flow m (x ': ys)
(..%~#>) v f = v ..~..> (\v' -> v' %~#> f)

infixl 0 ..%~#>

-- | Match in the tail, connect to the expected result
(>..%~#>) ::
   ( Monad m
   , Catchable a xs
   , Liftable (Filter a xs) ys
   ) => Flow m (x ': xs) -> (a -> Flow m ys) -> Flow m (x ': ys)
(>..%~#>) = liftm (..%~#>)

infixl 0 >..%~#>

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
(%~.>) v f = case removeType v of
   Left x   -> flowRet =<< f x
   Right ys -> prependVariant (Proxy :: Proxy '[y]) <$> return ys

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
(%~:>) :: forall x xs ys m.
   ( Monad m
   , Catchable x xs
   , KnownNat (Length ys)
   ) => Variant xs -> (x -> Flow m ys) -> Flow m (Concat ys (Filter x xs))
(%~:>) v f = case removeType v of
   Left x   -> appendVariant  (Proxy :: Proxy (Filter x xs)) <$> f x
   Right ys -> prependVariant (Proxy :: Proxy ys)            <$> return ys

infixl 0 %~:>

-- | Catch element, concat the result
(>%~:>) :: forall x xs ys m.
   ( Monad m
   , Catchable x xs
   , KnownNat (Length ys)
   ) => Flow m xs -> (x -> Flow m ys) -> Flow m (Concat ys (Filter x xs))
(>%~:>) = liftm (%~:>)

infixl 0 >%~:>

-- | Catch element, lift the result
(%~^^>) :: forall x xs ys zs m.
   ( Monad m
   , Catchable x xs
   , Liftable (Filter x xs) zs
   , Liftable ys zs
   ) => Variant xs -> (x -> Flow m ys) -> Flow m zs
(%~^^>) v f = case removeType v of
   Left x   -> liftVariant <$> f x
   Right ys -> liftVariant <$> return ys

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
(%~#>) :: forall x xs zs m.
   ( Monad m
   , Catchable x xs
   , Liftable (Filter x xs) zs
   ) => Variant xs -> (x -> Flow m zs) -> Flow m zs
(%~#>) v f = case removeType v of
   Left x   -> f x
   Right ys -> return (liftVariant ys)

infixl 0 %~#>

-- | Catch element, connect to the expected output
(>%~#>) :: forall x xs zs m.
   ( Monad m
   , Catchable x xs
   , Liftable (Filter x xs) zs
   ) => Flow m xs -> (x -> Flow m zs) -> Flow m zs
(>%~#>) = liftm (%~#>)

infixl 0 >%~#>

-- | Catch element, use the same output type
(%~->) :: forall x xs m.
   ( Monad m
   , Catchable x xs
   ) => Variant xs -> (x -> Flow m xs) -> Flow m xs
(%~->) v f = case removeType v of
   Left x  -> f x
   Right _ -> return v

infixl 0 %~->

-- | Catch element, use the same output type
(>%~->) :: forall x xs m.
   ( Monad m
   , Catchable x xs
   ) => Flow m xs -> (x -> Flow m xs) -> Flow m xs
(>%~->) = liftm (%~->)

infixl 0 >%~->

-- | Catch element, fusion the result
(%~&>) :: forall x xs ys zs m.
   ( Monad m
   , Catchable x xs
   , Liftable (Filter x xs) zs
   , Liftable ys zs
   , zs ~ Fusion (Filter x xs) ys
   ) => Variant xs -> (x -> Flow m ys) -> Flow m zs
(%~&>) v f = case removeType v of
   Left x   -> liftVariant <$> f x
   Right ys -> return (liftVariant ys)

infixl 0 %~&>

-- | Catch element, fusion the result
(>%~&>) :: forall x xs ys zs m.
   ( Monad m
   , Catchable x xs
   , Liftable (Filter x xs) zs
   , Liftable ys zs
   , zs ~ Fusion (Filter x xs) ys
   ) => Flow m xs -> (x -> Flow m ys) -> Flow m zs
(>%~&>) = liftm (%~&>)

infixl 0 >%~&>

-- | Catch element and perform effect. Passthrough the input value.
(%~=>) :: forall x xs m.
   ( Monad m
   , Catchable x xs
   ) => Variant xs -> (x -> m ()) -> Flow m xs
(%~=>) v f = case removeType v of
   Left x  -> f x >> return v
   Right _ -> return v

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
(%~!>) v f = case removeType v of
   Left x  -> f x
   Right _ -> return ()

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
(%~!!>) v f = case removeType v of
   Left x  -> f x >> error "%~!!> error"
   Right u -> return u

infixl 0 %~!!>

-- | Catch element and perform effect.
(>%~!!>) :: forall x xs m.
   ( Monad m
   , Catchable x xs
   ) => Flow m xs -> (x -> m ()) -> Flow m (Filter x xs)
(>%~!!>) = liftm (%~!!>)

infixl 0 >%~!!>
