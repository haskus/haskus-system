module ViperVM.Format.Binary.BitField
   ( BitField(..)
   , Bit(..)
   , flatten
   , length
   , drop
   , take
   , priority
   )
where

import Prelude hiding (drop,take,length)

import Data.Word
import Data.Bits

data BitField
   = Fixed32 Word Word32      -- ^ Fixed n least significant bits
   | Var Word                 -- ^ Variable bits
   | Seq BitField BitField    -- ^ Sequence
   | Empty
   deriving (Show)

-- | Seq smart constructor
sq :: BitField -> BitField -> BitField
sq Empty x = x
sq x Empty = x
sq x y     = Seq x y


length :: BitField -> Word
length (Fixed32 n _) = n
length (Var n)       = n
length (Seq x y)     = length x + length y
length Empty         = 0

drop :: Word -> BitField -> BitField
drop _ Empty         = Empty
drop k (Fixed32 n x)
   | k >= n          = Empty
   | otherwise       = Fixed32 (n-k) x
drop k (Var n)
   | k >= n          = Empty
   | otherwise       = Var (n-k)
drop k (Seq x y)
   | k == length x   = y
   | k > length x    = drop (k - length x) y
   | otherwise       = sq (drop k x) y

take :: Word -> BitField -> BitField
take _ Empty         = Empty
take k (Fixed32 n x)
   | k >= n          = Fixed32 n x
   | otherwise       = Fixed32 k (x `shiftR` fromIntegral (n-k))
take k (Var n)
   | k >= n          = Var n
   | otherwise       = Var k
take k (Seq x y)
   | k == length x   = x
   | k < length x    = take k x
   | otherwise       = sq x (take (k-length x) y)


data Bit = Zero | One | Unset deriving (Show,Eq)

instance Ord Bit where
   compare Zero  Zero  = EQ
   compare One   One   = EQ
   compare Unset Unset = EQ
   compare Zero  _     = LT
   compare One   Unset = LT
   compare One   Zero  = GT
   compare Unset _     = GT

flatten :: BitField -> [Bit]
flatten Empty = []
flatten (Fixed32 n x) = fmap (boolBit . testBit x . fromIntegral) [n-1, n-2..0]
   where
      boolBit True  = One
      boolBit False = Zero
flatten (Var n) = replicate (fromIntegral n) Unset
flatten (Seq x y) = flatten x ++ flatten y

instance Eq BitField where
   (==) x y = flatten x == flatten y

instance Ord BitField where
   compare x y = compare (flatten x) (flatten y)

hasVar :: BitField -> Bool
hasVar (Var _)       = True
hasVar (Seq x y)     = hasVar x || hasVar y
hasVar _             = False

priority :: BitField -> BitField -> Ordering
priority x y = if hasVar x' || hasVar y'
      then case (any (== GT) cs, any (== LT) cs) of
         (False,False) -> EQ
         (False,True)  -> LT
         (True,False)  -> GT
         (True,True)   -> EQ
      else EQ
   where
      cs = zipWith cmp (flatten x') (flatten y')
      cmp Unset Unset = EQ
      cmp _     Unset = LT
      cmp Unset _     = GT
      cmp _     _     = EQ
      l = min (length x) (length y)
      (x',y') = (take l x, take l y)

