{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module ViperVM.Format.Binary.Unum
   ( Unum
   , I
   , Neg
   , Rcp
   , Infinite
   , Log2
   , UnumNumbers
   , UnumSize
   , unumSize
   , encode
   , negate
   , reciprocate
   )
where

import Prelude hiding (negate)

import GHC.TypeLits
import Data.Proxy

import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Bits
import ViperVM.Utils.Types
import ViperVM.Utils.HList

-- | An Unum
--
-- 0 (and its reciprocal) is always included.
-- Numbers have to be >= 1 and sorted.
--
-- e.g., Unum '[] => /0 .. 0 .. /0
--       Unum '[I 1] => /0 .. -1 .. 0 .. 1 .. /0
--       Unum '[I 1, I 2] => /0 .. -2 .. -1 .. -/2 .. 0 .. /2 .. 1 .. 2 .. /0
--       Unum '[I 1, PI]  => /0 .. -PI .. -1 .. -/PI .. 0 .. /PI .. 1 .. PI .. /0
data Unum (xs :: [*])

data I (n :: Nat)
data Neg a
data Rcp a
type Infinite = Rcp (I 0)

type family Simplify a where
   Simplify a = Simplify' 'True a

type family Simplify' loop a where
   Simplify' l (Rcp (Rcp x))  = Simplify x
   Simplify' l (Neg (Neg x))  = Simplify x
   Simplify' l (Neg (I 0))    = I 0
   Simplify' l (Rcp (I 1))    = I 1
   Simplify' l (Neg Infinite) = Infinite -- infinite is special
   Simplify' l (Rcp (Neg x))  = Simplify (Neg (Rcp x)) -- Neg are outer
   Simplify' 'True (Rcp x)    = Simplify' 'False (Rcp (Simplify x))
   Simplify' 'True (Neg x)    = Simplify' 'False (Neg (Simplify x))
   Simplify' 'False (Rcp x)   = Rcp (Simplify x)
   Simplify' 'False (Neg x)   = Neg (Simplify x)
   Simplify' l x              = x

-- | Compute the precise numbers set
type family UnumNumbers x where
   -- add /0 (infinite), add reciprocals, add negations, nub
   UnumNumbers (Unum xs) = Nub (AddNeg (AddRcp (Snoc xs Infinite)))

-- | Positive numbers in the unums
type family UnumPositives x where
   UnumPositives (Unum xs) = Nub (AddRcp (Snoc xs Infinite))

-- | Indexable numbers
type family UnumIndexables x where
   UnumIndexables u =
      Nub (Concat (UnumPositives u) (Reverse (MapNeg (UnumPositives u))))

-- | Compute the number of bits required
type family UnumSize x where
   UnumSize x = 1 + Log2 (Length (UnumNumbers x)) -- add 1 for ubit

unumSize :: forall u.
   ( KnownNat (UnumSize u)
   ) => Proxy u -> Word
unumSize _ = fromIntegral (natVal (Proxy :: Proxy (UnumSize u)))

type family Div2 n where
  Div2 0 = 0
  Div2 1 = 0
  Div2 n = Div2 (n - 2) + 1

type family Log2 n where
  Log2 0 = 0
  Log2 1 = 0
  Log2 n = Log2 (Div2 n) + 1

-- | Backing word for the unum
type family BackingWord x where
   BackingWord x = BackingWord' (UnumSize x)

type family BackingWord' (n :: Nat) where
   BackingWord' n =
       If (n <=? 8) Word8
      (If (n <=? 16) Word16
      (If (n <=? 32) Word32
      (If (n <=? 64) Word64
       Word64 -- FIXME
      )))

type family MapRcp xs where
   MapRcp '[] = '[]
   MapRcp (x ': xs) = Simplify (Rcp x) ': MapRcp xs

type family MapNeg xs where
   MapNeg '[] = '[]
   MapNeg (x ': xs) = Simplify (Neg x) ': MapNeg xs

type family AddRcp xs where
   AddRcp xs = Concat (Reverse (MapRcp xs)) xs

type family AddNeg xs where
   AddNeg xs = Concat (Reverse (MapNeg xs)) xs

newtype U u = U (BackingWord u)

instance
   ( Show (BackingWord u)
   , FiniteBits (BackingWord u)
   , KnownNat (UnumSize u)
   ) => Show (U u) where
   show (U w) = "Unum: " ++ drop (finiteBitSize w - fromIntegral (unumSize (Proxy :: Proxy u))) (bitsToString w)

-- | Encode a number
encode :: forall i x u.
   ( i ~ IndexOf (Simplify x) (UnumIndexables u)
   , KnownNat i
   , Num (BackingWord u)
   , Bits (BackingWord u)
   ) => Proxy u -> Proxy x -> Bool -> U u
encode _ _ b = if b then U w else U (setBit w 0)
   where
      w = fromIntegral (natVal (Proxy :: Proxy i)) `shiftL` 1

{-# INLINE encode #-}

-- | Negate a number
negate ::
   ( FiniteBits (BackingWord u)
   , Num (BackingWord u)
   ) => U u -> U u
negate (U w) = U (complement w + 1)

{-# INLINE negate #-}

-- | Reciprocate a number
reciprocate :: forall u.
   ( FiniteBits (BackingWord u)
   , Num (BackingWord u)
   , KnownNat (UnumSize u)
   ) => U u -> U u
reciprocate (U w) = U (w `xor` m + 1)
   where
      s = unumSize (Proxy :: Proxy u)
      m = makeMask (s-1)

{-# INLINE reciprocate #-}
