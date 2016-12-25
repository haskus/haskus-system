{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Reverse bits
--
-- There are several algorithms performing the same thing here (reversing bits
-- into words of different sizes). There are benchmarks for them in Haskus's
-- "bench" directory. The fastest one for the current architecture should be
-- selected below. If you find that another algorithm is faster on your
-- architecture, please report it.
module Haskus.Format.Binary.Bits.Reverse
   ( 
   -- * Generic
     BitReversable (..)
   , reverseBitsGeneric
   -- * Algorithms
   , reverseBitsObvious
   , reverseBits3Ops
   , reverseBits4Ops
   , reverseBitsTable
   , reverseBits7Ops
   , reverseBits5LgN
   , liftReverseBits
   )
where

import Haskus.Format.Binary.Buffer
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Bits.Basic

---------------------------------------------------
-- Generic and specialized reverseBits
---------------------------------------------------


-- | Reverse bits in a Word
reverseBitsGeneric :: (FiniteBits a, Integral a) => a -> a
reverseBitsGeneric = liftReverseBits reverseBits4Ops

-- | Data whose bits can be reversed
class BitReversable w where
   reverseBits :: w -> w

instance BitReversable Word8 where
   reverseBits = reverseBits4Ops

instance BitReversable Word16 where
   reverseBits = reverseBits5LgN

instance BitReversable Word32 where
   reverseBits = reverseBits5LgN

instance BitReversable Word64 where
   reverseBits = reverseBits5LgN

instance BitReversable Word where
   reverseBits = reverseBits5LgN


---------------------------------------------------
-- Bit reversal algorithms
---------------------------------------------------

-- Algorithms and explanations adapted from:
-- http://graphics.stanford.edu/~seander/bithacks.html#ReverseByteWith64Bits

-- Reverse the bits the obvious way
-- ================================
--
--
-- unsigned int v;     // input bits to be reversed
-- unsigned int r = v; // r will be reversed bits of v; first get LSB of v
-- int s = sizeof(v) * CHAR_BIT - 1; // extra shift needed at end
-- 
-- for (v >>= 1; v; v >>= 1)
-- {   
--   r <<= 1;
--   r |= v & 1;
--   s--;
-- }
-- r <<= s; // shift when v's highest bits are zero
--
-- On October 15, 2004, Michael Hoisie pointed out a bug in the original
-- version. Randal E. Bryant suggested removing an extra operation on May 3,
-- 2005. Behdad Esfabod suggested a slight change that eliminated one iteration
-- of the loop on May 18, 2005. Then, on February 6, 2007, Liyong Zhou
-- suggested a better version that loops while v is not 0, so rather than
-- iterating over all bits it stops early. 

-- | Obvious recursive verion
reverseBitsObvious :: FiniteBits a => a -> a
reverseBitsObvious x = rec x (x `shiftR` 1) (finiteBitSize x - 1)
   where
      rec :: FiniteBits a => a -> a -> Int -> a
      rec !r !v !s 
         | v == zeroBits = r `shiftL` s
         | otherwise     = rec ((r `shiftL` 1) .|. (v .&. bit 0)) (v `shiftR` 1) (s - 1)

{-# SPECIALIZE reverseBitsObvious :: Word8  -> Word8  #-}
{-# SPECIALIZE reverseBitsObvious :: Word16 -> Word16 #-}
{-# SPECIALIZE reverseBitsObvious :: Word32 -> Word32 #-}
{-# SPECIALIZE reverseBitsObvious :: Word64 -> Word64 #-}

-- Reverse the bits in a byte with 3 operations (64-bit multiply and modulus division) 
-- ===================================================================================
-- 
-- unsigned char b; // reverse this (8-bit) byte
--  
-- b = (b * 0x0202020202ULL & 0x010884422010ULL) % 1023;
-- 
-- The multiply operation creates five separate copies of the 8-bit byte
-- pattern to fan-out into a 64-bit value. The AND operation selects the bits
-- that are in the correct (reversed) positions, relative to each 10-bit groups
-- of bits. The multiply and the AND operations copy the bits from the original
-- byte so they each appear in only one of the 10-bit sets. The reversed
-- positions of the bits from the original byte coincide with their relative
-- positions within any 10-bit set. The last step, which involves modulus
-- division by 2^10 - 1, has the effect of merging together each set of 10 bits
-- (from positions 0-9, 10-19, 20-29, ...) in the 64-bit value. They do not
-- overlap, so the addition steps underlying the modulus division behave like
-- or operations.
-- 
-- This method was attributed to Rich Schroeppel in the Programming Hacks
-- section of Beeler, M., Gosper, R. W., and Schroeppel, R. HAKMEM. MIT AI Memo
-- 239, Feb. 29, 1972.

-- | Reverse bits in a Word8 (3 64-bit operations, modulus division)
reverseBits3Ops :: Word8 -> Word8
{-# INLINE reverseBits3Ops #-}
reverseBits3Ops x = fromIntegral x'
   where
      !x' = ((fromIntegral x * 0x0202020202 :: Word64) .&. 0x010884422010) `mod` 1023


-- Reverse the bits in a byte with 4 operations (64-bit multiply, no division) 
-- ===========================================================================
--
-- unsigned char b; // reverse this (8-bit) byte
--  
-- b = ((b * 0x80200802ULL) & 0x0884422110ULL) * 0x0101010101ULL >> 32;
-- 
-- The following shows the flow of the bit values with the boolean variables a,
-- b, c, d, e, f, g, and h, which comprise an 8-bit byte. Notice how the first
-- multiply fans out the bit pattern to multiple copies, while the last
-- multiply combines them in the fifth byte from the right. 
--
--
--                                                                                         abcd efgh (-> hgfe dcba)
-- *                                                      1000 0000  0010 0000  0000 1000  0000 0010 (0x80200802)
-- -------------------------------------------------------------------------------------------------
--                                             0abc defg  h00a bcde  fgh0 0abc  defg h00a  bcde fgh0
-- &                                           0000 1000  1000 0100  0100 0010  0010 0001  0001 0000 (0x0884422110)
-- -------------------------------------------------------------------------------------------------
--                                             0000 d000  h000 0c00  0g00 00b0  00f0 000a  000e 0000
-- *                                           0000 0001  0000 0001  0000 0001  0000 0001  0000 0001 (0x0101010101)
-- -------------------------------------------------------------------------------------------------
--                                             0000 d000  h000 0c00  0g00 00b0  00f0 000a  000e 0000
--                                  0000 d000  h000 0c00  0g00 00b0  00f0 000a  000e 0000
--                       0000 d000  h000 0c00  0g00 00b0  00f0 000a  000e 0000
--            0000 d000  h000 0c00  0g00 00b0  00f0 000a  000e 0000
-- 0000 d000  h000 0c00  0g00 00b0  00f0 000a  000e 0000
-- -------------------------------------------------------------------------------------------------
-- 0000 d000  h000 dc00  hg00 dcb0  hgf0 dcba  hgfe dcba  hgfe 0cba  0gfe 00ba  00fe 000a  000e 0000
-- >> 32
-- -------------------------------------------------------------------------------------------------
--                                             0000 d000  h000 dc00  hg00 dcb0  hgf0 dcba  hgfe dcba  
-- &                                                                                       1111 1111
-- -------------------------------------------------------------------------------------------------
--                                                                                         hgfe dcba
-- Note that the last two steps can be combined on some processors because the
-- registers can be accessed as bytes; just multiply so that a register stores
-- the upper 32 bits of the result and the take the low byte. Thus, it may take
-- only 6 operations.
-- 
-- Devised by Sean Anderson, July 13, 2001. 

-- | Reverse bits in a Word8 (4 64-bit operations, no division)
reverseBits4Ops :: Word8 -> Word8
{-# INLINE reverseBits4Ops #-}
reverseBits4Ops x = fromIntegral x'
   where
      !x' = (((fromIntegral x * 0x80200802 :: Word64) .&. 0x0884422110) * 0x0101010101) `shiftR` 32


-- Reverse bits using a lookup table
-- =================================

-- | Reverse bits using a lookup table
reverseBitsTable :: Word8 -> Word8
{-# INLINE reverseBitsTable #-}
reverseBitsTable x = bitsTable `bufferIndex` (fromIntegral x)


-- fill the table by using another method
bitsTable :: Buffer
bitsTable = bufferPackByteList $ fmap reverseBits4Ops [0..255]

-- Reverse the bits in a byte with 7 operations (no 64-bit)
-- ========================================================
-- 
-- b = ((b * 0x0802LU & 0x22110LU) | (b * 0x8020LU & 0x88440LU)) * 0x10101LU >> 16; 
-- 
-- Make sure you assign or cast the result to an unsigned char to remove
-- garbage in the higher bits. Devised by Sean Anderson, July 13, 2001. Typo
-- spotted and correction supplied by Mike Keith, January 3, 2002. 


-- | Reverse bits in a Word8 (7 no 64-bit operations, no division)
reverseBits7Ops :: Word8 -> Word8
{-# INLINE reverseBits7Ops #-}
reverseBits7Ops b' = fromIntegral x'
   where
      b   = fromIntegral b' :: Word32
      !x' = ((((b * 0x0802) .&. 0x22110) .|. ((b * 0x8020) .&. 0x88440)) * 0x10101) `shiftR` 16


-- Reverse an N-bit quantity in parallel in 5 * lg(N) operations
-- =============================================================
-- 
-- unsigned int v; // 32-bit word to reverse bit order
-- 
-- // swap odd and even bits
-- v = ((v >> 1) & 0x55555555) | ((v & 0x55555555) << 1);
-- // swap consecutive pairs
-- v = ((v >> 2) & 0x33333333) | ((v & 0x33333333) << 2);
-- // swap nibbles ... 
-- v = ((v >> 4) & 0x0F0F0F0F) | ((v & 0x0F0F0F0F) << 4);
-- // swap bytes
-- v = ((v >> 8) & 0x00FF00FF) | ((v & 0x00FF00FF) << 8);
-- // swap 2-byte long pairs
-- v = ( v >> 16             ) | ( v               << 16);
-- 
-- The following variation is also O(lg(N)), however it requires more
-- operations to reverse v. Its virtue is in taking less slightly memory by
-- computing the constants on the fly.
-- 
-- unsigned int s = sizeof(v) * CHAR_BIT; // bit size; must be power of 2 
-- unsigned int mask = ~0;         
-- while ((s >>= 1) > 0) 
-- {
--   mask ^= (mask << s);
--   v = ((v >> s) & mask) | ((v << s) & ~mask);
-- }
-- 
-- These methods above are best suited to situations where N is large. If you
-- use the above with 64-bit ints (or larger), then you need to add more lines
-- (following the pattern); otherwise only the lower 32 bits will be reversed
-- and the result will be in the lower 32 bits.
-- 
-- See Dr. Dobb's Journal 1983, Edwin Freed's article on Binary Magic Numbers
-- for more information. The second variation was suggested by Ken Raeburn on
-- September 13, 2005. Veldmeijer mentioned that the first version could do
-- without ANDS in the last line on March 19, 2006. 

-- | "Parallel" recursive version
reverseBits5LgN :: FiniteBits a => a -> a
reverseBits5LgN x = rec (finiteBitSize x `shiftR` 1) (complement zeroBits) x
   where
      rec :: FiniteBits a => Int -> a -> a -> a
      rec !s !mask !v
         | s <= 0        = v
         | otherwise     = rec (s `shiftR` 1) mask' v'
            where
               mask' = mask `xor` (mask `shiftL` s)
               v'    =      ((v `shiftR` s) .&. mask')
                        .|. ((v `shiftL` s) .&. complement mask')

{-# SPECIALIZE reverseBits5LgN :: Word8  -> Word8  #-}
{-# SPECIALIZE reverseBits5LgN :: Word16 -> Word16 #-}
{-# SPECIALIZE reverseBits5LgN :: Word32 -> Word32 #-}
{-# SPECIALIZE reverseBits5LgN :: Word64 -> Word64 #-}



-- | Convert a function working on Word8 to one working on any Word
--
-- The number of bits in the Word must be a multiple of 8
liftReverseBits :: (FiniteBits a, Integral a) => (Word8 -> Word8) -> a -> a
liftReverseBits f w = rec zeroBits 0
   where
      nb = finiteBitSize w `shiftR` 3 -- div 8
      f' = fromIntegral . f . fromIntegral
      rec !v !o
         | o == nb    = v
         | otherwise = rec v' (o+1)
               where
                  -- multiplication by 8 replaced with (`shiftL` 3)
                  v' = v .|. ((f' (w `shiftR` (o `shiftL` 3))) `shiftL` ((nb-1-o) `shiftL` 3))

{-# SPECIALIZE liftReverseBits :: (Word8 -> Word8) -> Word8  -> Word8  #-}
{-# SPECIALIZE liftReverseBits :: (Word8 -> Word8) -> Word16 -> Word16 #-}
{-# SPECIALIZE liftReverseBits :: (Word8 -> Word8) -> Word32 -> Word32 #-}
{-# SPECIALIZE liftReverseBits :: (Word8 -> Word8) -> Word64 -> Word64 #-}

