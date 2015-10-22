module ViperVM.Format.Binary.BitOps
   ( makeMask
   , maskLeastBits
   , bitOffset
   , byteOffset
   , reverseBitsWord8
   , reverseLeastBits
   , bitsToString
   , bitsFromString
   )
where

import Data.Word
import Data.Bits
import Data.List (foldl')

import ViperVM.Format.Binary.BitOps.BitReverse

-- | makeMask 3 = 00000111
makeMask :: (Bits a, Num a) => Int -> a
makeMask n = (1 `shiftL` fromIntegral n) - 1
{-# SPECIALIZE makeMask :: Int -> Int #-}
{-# SPECIALIZE makeMask :: Int -> Word #-}
{-# SPECIALIZE makeMask :: Int -> Word8 #-}
{-# SPECIALIZE makeMask :: Int -> Word16 #-}
{-# SPECIALIZE makeMask :: Int -> Word32 #-}
{-# SPECIALIZE makeMask :: Int -> Word64 #-}

-- | Keep only the n least-significant bits of the given value
maskLeastBits :: (Bits a, Num a) => Int -> a -> a
maskLeastBits n v = v .&. makeMask n
{-# INLINE maskLeastBits #-}

-- | Compute bit offset (equivalent to x `mod` 8 but faster)
bitOffset :: Int -> Int
bitOffset n = makeMask 3 .&. n
{-# INLINE bitOffset #-}

-- | Compute byte offset (equivalent to x `div` 8 but faster)
byteOffset :: Int -> Int
byteOffset n = n `shiftR` 3
{-# INLINE byteOffset #-}

-- | Reverse the @n@ least important bits of the given value. The higher bits
-- are set to 0.
reverseLeastBits :: Bits a => Int -> a -> a
reverseLeastBits n value = rec value n zeroBits
   where
      -- rec v i r, where
      --    v is orginal value shifted
      --    i is the remaining number of bits
      --    r is current value
      rec :: Bits b => b -> Int -> b -> b
      rec v 0 r 
         | v == zeroBits = r
      rec v i r
         | v == zeroBits = r `shiftL` i  -- fill remaining bits with 0
         | otherwise     = rec (v `shiftR` 1) (i-1) ((r `shiftL` 1) .|. (v .&. bit 0))



bitsToString :: FiniteBits a => a -> String
bitsToString x = fmap b [s, s-1 .. 0]
   where
      s   = finiteBitSize x - 1
      b v = if testBit x v then '1' else '0'


bitsFromString :: Bits a => String -> a
bitsFromString xs = foldl' b zeroBits (reverse xs `zip` [0..])
   where
      b x ('0',i) = clearBit x i
      b x ('1',i) = setBit x i
      b _ (c,_)   = error $ "Invalid character in the string: " ++ [c]

-- | Reverse bits in a Word8
--
-- The different implementation are in ViperVM.Format.Binary.BitReverse.  The
-- current one has been selected through benchmarking (see ViperVM benchmarks).
reverseBitsWord8 :: Word8 -> Word8
reverseBitsWord8 = reverseBits4Ops
