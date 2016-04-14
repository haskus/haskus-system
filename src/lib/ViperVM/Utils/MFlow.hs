{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}


module ViperVM.Utils.MFlow
   ( MFlow
   , withFlow0
   , withFlow
   , catch
   , return0
   , return'
   , (~>)
   , (>~>)
   )
where

import ViperVM.Utils.Variant
import ViperVM.Utils.HList
import Data.Proxy
import GHC.TypeLits

type MFlow m l = m (Variant l)

withFlow0 :: forall (k :: Nat) m l l2.
   ( KnownNat k
   , k ~ Length l2
   , Monad m )
   => Variant l -> (TypeAt 0 l -> MFlow m l2) -> MFlow m (ReplaceAt 0 l l2)
withFlow0 v f = updateVariantFoldM (Proxy :: Proxy 0) f v


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

(~>) ::
   ( Liftable xs zs
   , Liftable (Filter a l) zs
   , zs ~ Fusion xs (Filter a l)
   , Monad m
   , Catchable a l
   ) => Variant l -> (a -> MFlow m xs) -> MFlow m zs
(~>) = withFlow

(>~>) ::
   ( Liftable xs zs
   , Liftable (Filter a l) zs
   , zs ~ Fusion xs (Filter a l)
   , Monad m
   , Catchable a l
   ) => m (Variant l) -> (a -> MFlow m xs) -> MFlow m zs
(>~>) f g = f >>= (~> g)

return0 :: Monad m => x -> MFlow m (x ': xs)
return0 = return . setVariant0

return' :: Monad m => x -> MFlow m '[x]
return' = return0

catch :: 
   ( Monad m
   , Catchable a l
   )=> Variant l -> (a -> m ()) -> m ()
catch v f = case removeType v of
   Left a  -> f a
   Right _ -> return ()
