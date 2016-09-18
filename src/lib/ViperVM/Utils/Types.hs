{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Common type functions
module ViperVM.Utils.Types
   ( If
   , IfNat
   , Modulo
   , module GHC.TypeLits
   , Proxy (..)
   )
where

import GHC.TypeLits
import Data.Proxy

-- | If-then-else
type family If c t e where
   If 'True  t e = t
   If 'False t e = e

-- | If-then-else
type family IfNat c (t :: Nat) (e :: Nat) where
   IfNat 'True  t e = t
   IfNat 'False t e = e

-- | Modulo
type family Modulo (a :: Nat) (b :: Nat) where
   Modulo a b = Modulo' (a <=? b) a b

-- | Helper for Modulo
type family Modulo' c a b where
   Modulo' 'True  a b = a
   Modulo' 'False a b = Modulo' ((a-b) <=? b) (a-b) b

