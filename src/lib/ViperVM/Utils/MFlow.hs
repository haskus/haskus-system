{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}


module ViperVM.Utils.MFlow
   ( MFlow
   , withFlow0
   , withFlow
   , flowRet
   , flowRet'
   , flowSet
   , flowLift
   , (~>)
   , (#~>)
   , (>~>)
   , (>#~>)
   , (#->)
   )
where

import ViperVM.Utils.Variant
import ViperVM.Utils.HList
import Data.Proxy
import GHC.TypeLits

type MFlow m l = m (Variant l)

infixl 1 #~>, #->, ~>
infixl 1 >#~>, >~>

withFlow0 :: forall (k :: Nat) m l l2.
   ( KnownNat k
   , k ~ Length l2
   , Monad m )
   => Variant l -> (TypeAt 0 l -> MFlow m l2) -> MFlow m (ReplaceAt 0 l l2)
withFlow0 v f = updateVariantFoldM (Proxy :: Proxy 0) f v

(~>) :: forall (k :: Nat) m l l2.
   ( KnownNat k
   , k ~ Length l2
   , Monad m )
   => Variant l -> (TypeAt 0 l -> MFlow m l2) -> MFlow m (ReplaceAt 0 l l2)
(~>) = withFlow0

(>~>) :: forall (k :: Nat) m l l2.
   ( KnownNat k
   , k ~ Length l2
   , Monad m )
   => m (Variant l) -> (TypeAt 0 l -> MFlow m l2) -> MFlow m (ReplaceAt 0 l l2)
(>~>) f g = f >>= (~> g)

withFlow ::
   ( Liftable xs zs
   , Liftable (Filter a l) zs
   , zs ~ Fusion xs (Filter a l)
   , Monad m
   , Catchable a l
   ) => Variant l -> (a -> MFlow m xs) -> MFlow m zs
withFlow v f = case removeType v of
   Left a   -> liftVariant <$> f a
   Right ys -> return (liftVariant ys)

(#~>) ::
   ( Liftable xs zs
   , Liftable (Filter a l) zs
   , zs ~ Fusion xs (Filter a l)
   , Monad m
   , Catchable a l
   ) => Variant l -> (a -> MFlow m xs) -> MFlow m zs
(#~>) = withFlow

(>#~>) ::
   ( Liftable xs zs
   , Liftable (Filter a l) zs
   , zs ~ Fusion xs (Filter a l)
   , Monad m
   , Catchable a l
   ) => m (Variant l) -> (a -> MFlow m xs) -> MFlow m zs
(>#~>) f g = f >>= (#~> g)

flowRet :: Monad m => x -> MFlow m (x ': xs)
flowRet = return . setVariant0

flowRet' :: Monad m => x -> MFlow m '[x]
flowRet' = flowRet

flowSet :: (Member x xs, Monad m) => x -> MFlow m xs
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
