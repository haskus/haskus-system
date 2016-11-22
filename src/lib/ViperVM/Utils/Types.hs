{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Common type functions
module ViperVM.Utils.Types
   ( Nat
   , Symbol
   , natValue
   , natValue'
   , symbolValue
   , KnownNat
   , KnownSymbol
   , CmpNat
   , CmpSymbol
   , type (<=?)
   , type (<=)
   , type (+)
   , type (-)
   , type (*)
   , type (^)
   , If
   , IfNat
   , Modulo
   , Same
   , Proxy (..)
   , TypeError
   , ErrorMessage (..)
   )
where

import GHC.TypeLits
import Data.Proxy

-- | Get a Nat value
natValue :: forall (n :: Nat) a. (KnownNat n, Num a) => a
{-# INLINE natValue #-}
natValue = fromIntegral (natVal (Proxy :: Proxy n))

-- | Get a Nat value
natValue' :: forall (n :: Nat). KnownNat n => Word
{-# INLINE natValue' #-}
natValue' = natValue @n

-- | Get a Symbol value
symbolValue :: forall (s :: Symbol). (KnownSymbol s) => String
{-# INLINE symbolValue #-}
symbolValue = symbolVal (Proxy :: Proxy s)

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

-- | Type equality to Nat
type family Same a b :: Nat where
   Same a a = 1
   Same a b = 0
