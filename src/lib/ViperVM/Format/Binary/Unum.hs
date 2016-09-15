{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ViperVM.Format.Binary.Unum
   ( Unum
   , UnumNum (..)
   , I
   , U (..)
   , Neg
   , Rcp
   , Infinite
   , Log2
   , UnumNumbers
   , UnumSize
   , BackingWord
   , UBit (..)
   , unumSize
   , unumZero
   , unumInfinite
   , unumEncode
   , unumBits
   , unumNegate
   , unumReciprocate
   , unumLabels
   , Sign (..)
   , unumSign
   -- * SORN
   , SORN
   , SORNBackingWord
   , sornBits
   , sornSize
   , sornEmpty
   , sornFull
   , sornNonInfinite
   , sornNonZero
   , sornSingle
   , sornInsert
   , sornMember
   , sornRemove
   , sornUnion
   , sornIntersect
   , sornComplement
   , sornNegate
   , sornElems
   , sornFromElems
   , SornAdd (..)
   )
where

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


class UnumNum a where
   unumLabel :: a -> String

data I (n :: Nat)
data Neg a
data Rcp a
data Uncertain a

instance KnownNat n => UnumNum (I n) where
   unumLabel _ = show (natVal (Proxy :: Proxy n))

instance UnumNum x => UnumNum (Rcp x) where
   unumLabel _ = "/" ++ unumLabel (undefined :: x)

instance UnumNum x => UnumNum (Neg x) where
   unumLabel _ = "-" ++ unumLabel (undefined :: x)

instance UnumNum x => UnumNum (Uncertain x) where
   unumLabel _ = unumLabel (undefined :: x) ++ ".."

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

-- | All unum members
type family UnumMembers x where
   UnumMembers u = MakeMembers (UnumIndexables u)

type family MakeMembers xs where
   MakeMembers '[]       = '[]
   MakeMembers (x ': xs) = x ': Uncertain x ': MakeMembers xs
 

data GetLabel = GetLabel

instance  forall a r.
   ( UnumNum a
   , r ~ [String]
   ) => ApplyAB GetLabel (a, [String]) r where
   applyAB _ (x,xs) = unumLabel x : xs

-- | Unum labels
unumLabels :: forall u v.
   ( HFoldr' GetLabel [String] v [String]
   , v ~ UnumMembers u
   ) => Proxy u -> [String]
unumLabels _ = hFoldr' GetLabel ([] :: [String]) (undefined :: HList v)

-- | Compute the number of bits required
type family UnumSize x where
   UnumSize x = 1 + Log2 (Length (UnumNumbers x)) -- add 1 for ubit

-- | Size of an unum in bits
unumSize :: forall u.
   ( KnownNat (UnumSize u)
   ) => Proxy u -> Word
unumSize _ = fromIntegral (natVal (Proxy :: Proxy (UnumSize u)))

-- | Zero
unumZero :: forall u.
   ( Num (BackingWord u)
   , Bits (BackingWord u)
   , Encodable (I 0) u
   ) => U u
unumZero = unumEncode (Proxy :: Proxy u) (Proxy :: Proxy (I 0)) Number

-- | Infinite
unumInfinite :: forall u.
   ( Num (BackingWord u)
   , Bits (BackingWord u)
   , Encodable Infinite u
   ) => U u
unumInfinite = unumEncode (Proxy :: Proxy u) (Proxy :: Proxy Infinite) Number

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

instance Eq (BackingWord u) => Eq (U u) where
   U x == U y = x == y

instance forall u v.
   ( HFoldr' GetLabel [String] v [String]
   , v ~ UnumMembers u
   , Integral (BackingWord u)
   ) => Show (U u) where
   show (U w) = unumLabels (Proxy :: Proxy u) !! fromIntegral w

unumBits :: forall u.
   ( FiniteBits (BackingWord u)
   , KnownNat (UnumSize u)
   ) => U u -> String
unumBits (U w) = drop (finiteBitSize w - fromIntegral (unumSize (Proxy :: Proxy u))) (bitsToString w)

type Encodable x u =
   ( KnownNat (IndexOf (Simplify x) (UnumIndexables u)))


-- | Uncertainty bit
data UBit
   = Number -- ^ Precise number
   | Range  -- ^ Range above the number
   deriving (Show,Eq)

-- | Encode a number
unumEncode :: forall i x u.
   ( i ~ IndexOf (Simplify x) (UnumIndexables u)
   , KnownNat i
   , Num (BackingWord u)
   , Bits (BackingWord u)
   ) => Proxy u -> Proxy x -> UBit -> U u
unumEncode _ _ b = case b of
      Number -> U w
      Range  -> U (setBit w 0)
   where
      w = fromIntegral (natVal (Proxy :: Proxy i)) `shiftL` 1

{-# INLINE unumEncode #-}

-- | Negate a number
unumNegate :: forall u.
   ( FiniteBits (BackingWord u)
   , Num (BackingWord u)
   , KnownNat (UnumSize u)
   ) => U u -> U u
unumNegate (U w) = U (maskLeastBits s (complement w + 1))
   where
      s = unumSize (Proxy :: Proxy u)

{-# INLINE unumNegate #-}

-- | Reciprocate a number
unumReciprocate :: forall u.
   ( FiniteBits (BackingWord u)
   , Num (BackingWord u)
   , KnownNat (UnumSize u)
   ) => U u -> U u
unumReciprocate (U w) = U (w `xor` m + 1)
   where
      s = unumSize (Proxy :: Proxy u)
      m = makeMask (s-1)

{-# INLINE unumReciprocate #-}

data Sign
   = Positive
   | Negative
   deriving (Show,Eq)

-- | Get unum sign
unumSign :: forall u.
   ( Bits (BackingWord u)
   , KnownNat (UnumSize u)
   ) => U u -> Sign
unumSign (U w) = if testBit w n then Negative else Positive
   where
      n = fromIntegral (unumSize (Proxy :: Proxy u) - 1)

type family SORNSize u where
   SORNSize u = 2 * Length (UnumNumbers u)

type family SORNBackingWord u where
   SORNBackingWord u = BackingWord' (SORNSize u)

newtype SORN u = SORN (SORNBackingWord u)

instance forall u v.
   ( KnownNat (SORNSize u)
   , Bits (SORNBackingWord u)
   , Num (BackingWord u)
   , Integral (BackingWord u)
   , HFoldr' GetLabel [String] v [String]
   , v ~ UnumMembers u
   ) => Show (SORN u) where
   show = show . sornElems
   

-- | Show SORN bits
sornBits :: forall u s.
   ( FiniteBits (SORNBackingWord u)
   , KnownNat (UnumSize u)
   , s ~ SORNSize u
   , KnownNat s
   ) => SORN u -> String
sornBits (SORN w) = drop (finiteBitSize w - fromIntegral (natVal (Proxy :: Proxy s))) (bitsToString w)



-- | Size of a SORN in bits
sornSize :: forall u s.
   ( s ~ SORNSize u
   , KnownNat s
   ) => Proxy u -> Word
sornSize _ = fromIntegral (natVal (Proxy :: Proxy s))

-- | Empty SORN
sornEmpty :: (Bits (SORNBackingWord u)) => SORN u
sornEmpty = SORN zeroBits

-- | Full SORN
sornFull :: forall u.
   ( FiniteBits (SORNBackingWord u)
   , KnownNat (SORNSize u)
   ) => SORN u
sornFull = SORN (maskLeastBits s (complement zeroBits))
   where
      s = sornSize (Proxy :: Proxy u)

-- | Full SORN without infinite
sornNonInfinite ::
   ( Bits (SORNBackingWord u)
   , Integral (BackingWord u)
   , Bits (BackingWord u)
   , Encodable Infinite u
   ) => SORN u
sornNonInfinite = sornRemove (SORN (complement zeroBits)) inf
   where
      inf = unumEncode (Proxy :: Proxy u) (Proxy :: Proxy Infinite) Number

-- | Full SORN without infinite
sornNonZero ::
   ( Bits (SORNBackingWord u)
   , Integral (BackingWord u)
   , Bits (BackingWord u)
   , Encodable (I 0) u
   ) => SORN u
sornNonZero = sornRemove (SORN (complement zeroBits)) unumZero

-- | SORN singleton
sornSingle ::
   ( Integral (BackingWord u)
   , Bits (SORNBackingWord u)
   ) => U u -> SORN u
sornSingle = sornInsert sornEmpty

-- | Insert in a SORN
sornInsert :: forall u.
   ( Bits (SORNBackingWord u)
   , Integral (BackingWord u)
   ) => SORN u -> U u -> SORN u
sornInsert (SORN w) (U v) = SORN (setBit w (fromIntegral v))

-- | Remove in a SORN
sornRemove :: forall u.
   ( Bits (SORNBackingWord u)
   , Integral (BackingWord u)
   ) => SORN u -> U u -> SORN u
sornRemove (SORN w) (U v) = SORN (clearBit w (fromIntegral v))

-- | Test membership in a SORN
sornMember :: forall u.
   ( Bits (SORNBackingWord u)
   , Integral (BackingWord u)
   ) => SORN u -> U u -> Bool
sornMember (SORN w) (U x) = testBit w (fromIntegral x)

-- | Union of two SORNs
sornUnion :: forall u.
   ( Bits (SORNBackingWord u)
   ) => SORN u -> SORN u -> SORN u
sornUnion (SORN w) (SORN v) = SORN (w .|. v)

-- | Intersection of two SORNs
sornIntersect :: forall u.
   ( Bits (SORNBackingWord u)
   ) => SORN u -> SORN u -> SORN u
sornIntersect (SORN w) (SORN v) = SORN (w .&. v)

-- | Complement the SORN
sornComplement ::
   ( Bits (SORNBackingWord u)
   ) => SORN u -> SORN u
sornComplement (SORN x) = SORN (complement x)

-- | Negate a SORN
sornNegate :: forall u.
   ( FiniteBits (SORNBackingWord u)
   , FiniteBits (BackingWord u)
   , Integral (BackingWord u)
   , KnownNat (SORNSize u)
   , KnownNat (UnumSize u)
   ) => SORN u -> SORN u
sornNegate = sornFromElems . fmap unumNegate . sornElems

-- | Elements in the SORN
sornElems :: forall u s.
   ( s ~ SORNSize u
   , KnownNat s
   , Bits (SORNBackingWord u)
   , Num (BackingWord u)
   ) => SORN u -> [U u]
sornElems (SORN x) = foldl b [] (reverse ([s `shiftR` 1 .. s-1]
                                  ++ [0 .. (s-1) `shiftR` 1]))
   where
      s      = fromIntegral (natVal (Proxy :: Proxy s))
      b us i = if testBit x i
                  then U (fromIntegral i) : us
                  else us

-- | Create a SORN from its elements
sornFromElems ::
   ( Integral (BackingWord u)
   , Bits (SORNBackingWord u)
   ) => [U u] -> SORN u
sornFromElems = foldl sornInsert sornEmpty


class SornAdd u where
   sornAddU :: U u -> U u -> SORN u

   sornAdd ::
      ( KnownNat (SORNSize u)
      , Bits (SORNBackingWord u)
      , Num (BackingWord u)
      ) => SORN u -> SORN u -> SORN u
   sornAdd a b =
      foldl sornUnion sornEmpty [ sornAddU x y
                                | x <- sornElems a
                                , y <- sornElems b
                                ]
   sornSubU :: 
      ( FiniteBits (BackingWord u)
      , Num (BackingWord u)
      , KnownNat (UnumSize u)
      ) => U u -> U u -> SORN u
   sornSubU a b = sornAddU a (unumNegate b)

   sornSub ::
      ( KnownNat (SORNSize u)
      , Bits (SORNBackingWord u)
      , FiniteBits (BackingWord u)
      , Num (BackingWord u)
      , KnownNat (UnumSize u)
      ) => SORN u -> SORN u -> SORN u
   sornSub a b =
      foldl sornUnion sornEmpty [ sornSubU x y
                                | x <- sornElems a
                                , y <- sornElems b
                                ]
