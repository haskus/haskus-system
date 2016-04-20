{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Common type functions
module ViperVM.Utils.Types
   ( IfThenElse
   , Modulo
   )
where

import GHC.TypeLits

-- | If-then-else
type family IfThenElse c (t :: Nat) (e :: Nat) where
   IfThenElse 'True  t e = t
   IfThenElse 'False t e = e

-- | Modulo
type family Modulo (a :: Nat) (b :: Nat) where
   Modulo a b = Modulo' (a <=? b) a b

-- | Helper for Modulo
type family Modulo' c a b where
   Modulo' 'True  a b = a
   Modulo' 'False a b = Modulo' ((a-b) <=? b) (a-b) b

