module ViperVM.Format.Binary.BitOps
   ( makeMask
   , mask
   , bitOffset
   , byteOffset
   , reverseLeastBits
   )
where

import Data.Word
import Data.Bits

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
mask :: (Bits a, Num a) => Int -> a -> a
mask n v = v .&. makeMask n
{-# INLINE mask #-}

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
