{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

module ViperVM.Utils.Flow.Monad
   ( (>>=)
   , return
   , return'
   , throw
   , lift
   )
where

import ViperVM.Utils.Flow
import ViperVM.Utils.Variant
import ViperVM.Utils.HList
import qualified Prelude as P
import Prelude((.),($),Monad,Either(Left,Right))


(>>=) :: forall m a b l l2.
   ( Monad m
   , Liftable l (b ': Fusion l l2)
   , Liftable (b ': l2) (b ': Fusion l l2)
   ) => FlowT m l a -> (a -> FlowT m l2 b) -> FlowT m (Fusion l l2) b
(>>=) f g = FlowT $ do
   f' <- runFlowT f
   case headVariant f' of
      Left l  -> flowLift (P.return l)
      Right a -> flowLift (runFlowT (g a))

throw :: Monad m => a -> FlowT m '[a] ()
throw = FlowT . P.return . setVariant1

return :: Monad m => a -> FlowT m xs a
return = FlowT . P.return . setVariant0

return' :: Monad m => a -> FlowT m '[] a
return' = return

lift :: forall a m l l2.
   ( Liftable (a ': l) (a ': l2)
   , Monad m
   ) => FlowT m l a -> FlowT m l2 a
lift (FlowT f) = FlowT (flowLift f)
