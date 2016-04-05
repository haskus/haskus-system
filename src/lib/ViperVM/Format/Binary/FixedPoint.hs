{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ViperVM.Format.Binary.FixedPoint
   ( FixedPoint
   , toFixedPoint
   , fromFixedPoint
   )
where

import ViperVM.Format.Binary.BitField

import Data.Bits
import Foreign.Storable
import Foreign.CStorable
import Data.Proxy
import GHC.TypeLits

-- | Fixed-point number
-- `w` is the backing type
-- `n` is the number of bits for the numerator
-- `d` is the number of bits for the denominator
newtype FixedPoint w (n :: Nat) (d :: Nat) = FixedPoint (BitFields w
   '[ BitField n "numerator"   w
    , BitField d "denominator" w
    ])
   deriving (Storable,CStorable)

deriving instance forall w n d.
   ( Integral w
   , Bits w
   , Field w
   , BitSize w ~ (n + d)
   , KnownNat n
   , KnownNat d
   ) => Eq (FixedPoint w n d)

deriving instance forall w n d.
   ( Integral w
   , Bits w
   , Field w
   , BitSize w ~ (n + d)
   , KnownNat n
   , KnownNat d
   , Show w
   ) => Show (FixedPoint w n d)

-- | Convert to a fixed point value
toFixedPoint :: forall a w (n :: Nat) (d :: Nat).
   ( RealFrac a
   , BitSize w ~ (n + d)
   , KnownNat n
   , KnownNat d
   , Bits w
   , Field w
   , Num w
   , Integral w
   ) => a -> FixedPoint w n d
toFixedPoint a = FixedPoint
      $ BitFields (round (a * 2^dbits))
   where
      dbits = natVal (Proxy :: Proxy d)

-- | Convert from a fixed-point value
fromFixedPoint :: forall a w (n :: Nat) (d :: Nat).
   ( RealFrac a
   , BitSize w ~ (n + d)
   , KnownNat n
   , KnownNat d
   , Bits w
   , Field w
   , Num w
   , Integral w
   ) => FixedPoint w n d -> a
fromFixedPoint (FixedPoint bf) = w / 2^dbits
   where
      w = fromIntegral (bitFieldsBits bf)
      dbits = natVal (Proxy :: Proxy d)
