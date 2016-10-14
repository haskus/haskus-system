{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Fixed-point numbers
module ViperVM.Format.Binary.FixedPoint
   ( FixedPoint
   , toFixedPoint
   , fromFixedPoint
   )
where

import ViperVM.Format.Binary.BitField
import ViperVM.Format.Binary.Bits
import ViperVM.Format.Binary.Word
import ViperVM.Utils.Types

import Foreign.Storable
import Foreign.CStorable

-- | Fixed-point number
-- `w` is the backing type
-- `i` is the number of bits for the integer part (before the readix point)
-- `f` is the number of bits for the fractional part (after the radix point)
newtype FixedPoint w (i :: Nat) (f :: Nat) = FixedPoint (BitFields w
   '[ BitField i "integer"    w
    , BitField f "fractional" w
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
toFixedPoint a = FixedPoint $ BitFields (round (a * 2^natValue' @d))

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
fromFixedPoint (FixedPoint bf) = w / 2^(natValue' @d)
   where
      w = fromIntegral (bitFieldsBits bf)
