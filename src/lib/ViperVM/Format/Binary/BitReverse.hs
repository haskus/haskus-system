{-# LANGUAGE BangPatterns #-}
module ViperVM.Format.Binary.BitReverse
   ( reverseBits
   , reverseBitsTable
   )
where

import qualified Data.ByteString as BS
import Data.Bits
import Data.Word

-- | Reverse bits in a Word8
--
-- http://graphics.stanford.edu/~seander/bithacks.html#ReverseByteWith64Bits
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
reverseBits :: Word8 -> Word8
reverseBits x = fromIntegral x'
   where
      !x' = (((fromIntegral x * 0x80200802 :: Word64) .&. 0x0884422110) * 0x0101010101) `shiftR` 32

-- | Reverse bits using a lookup table
reverseBitsTable :: Word8 -> Word8
reverseBitsTable x = bitsTable `BS.index` (fromIntegral x)

bitsTable :: BS.ByteString
bitsTable = BS.pack $ fmap reverseBits [0..255]

