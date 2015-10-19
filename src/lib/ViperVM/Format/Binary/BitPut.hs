module ViperVM.Format.Binary.BitPut
   ( BitPutState(..)
   , newBitPutState
   , putBits
   , putBitsBS
   , getBitPutBS
   , getBitPutLBS
   )
where

import qualified Data.Binary.Builder as B
import Data.Binary.Builder ( Builder )

import ViperVM.Format.Binary.BitOrder
import ViperVM.Format.Binary.BitOps

import Data.ByteString as BS
import Data.ByteString.Lazy as LBS
import Data.ByteString.Unsafe as BS

import Data.Bits
import Data.Word

-- | BitPut state
data BitPutState = BitPutState
   { bitPutStateBuilder          :: !Builder       -- ^ Builder
   , bitPutStateCurrent          :: !Word8         -- ^ Current byte
   , bitPutStateOffset           :: !Int           -- ^ Current offset
   , bitPutStateBitOrder         :: !BitOrder      -- ^ Bit order
   }

newBitPutState :: BitOrder -> BitPutState
newBitPutState = BitPutState B.empty 0 0

putBits :: (Num a, Bits a, Integral a) => Int -> a -> BitPutState -> BitPutState
putBits n w s@(BitPutState builder b o bo) = s'
   where
      -- number of bits that will be stored in the current byte
      cn = min (8-o) n

      -- new state
      s' = case n of
            0 -> s
            _ -> putBits (n-cn) w' (flush (BitPutState builder b' (o+cn) bo))
      
      -- new current byte
      b' = shl (selectBits w) .|. b

      -- Word containing the remaining (n-cn) bits to store in its LSB
      w' = case bo of
         BB -> w
         BL -> w `shiftR` cn
         LL -> w `shiftR` cn
         LB -> w

      -- Select bits to store in the current byte.
      -- Put them in the correct order and return them in the least-significant
      -- bits of the returned value
      selectBits :: (Num a, Bits a, Integral a) => a -> Word8
      selectBits x = fromIntegral $ case bo of
         BB ->                       maskLeastBits cn $ x `shiftR` (n-cn)
         LB -> reverseLeastBits cn $ maskLeastBits cn $ x `shiftR` (n-cn)
         LL ->                       maskLeastBits cn x
         BL -> reverseLeastBits cn $ maskLeastBits cn x

      -- shift left at the correct position
      shl :: Word8 -> Word8
      shl x = case bo of
         BB -> x `shiftL` (8-o-cn)
         BL -> x `shiftL` (8-o-cn)
         LL -> x `shiftL` o
         LB -> x `shiftL` o

      -- flush the current byte if it is full
      flush s2@(BitPutState b2 w2 o2 bo2)
        | o2 == 8   = BitPutState (b2 `mappend` B.singleton w2) 0 0 bo2
        | otherwise = s2


-- | Put a 'ByteString'.
--
-- Examples: 3 bits are already written in the current byte
--    BB: ABCDEFGH IJKLMNOP -> xxxABCDE FGHIJKLM NOPxxxxx
--    LL: ABCDEFGH IJKLMNOP -> LMNOPxxx DEFGHIJK xxxxxABC
--    BL: ABCDEFGH IJKLMNOP -> xxxPONML KJIHGFED CBAxxxxx
--    LB: ABCDEFGH IJKLMNOP -> EDCBAxxx MLKJIHGF xxxxxPON
putBitsBS :: BS.ByteString -> BitPutState -> BitPutState
putBitsBS bs s
   | BS.null bs = s
   | otherwise  = case s of
      (BitPutState builder b 0 BB) -> BitPutState (builder `mappend` B.fromByteString bs) b 0 BB
      (BitPutState builder b 0 LL) -> BitPutState (builder `mappend` B.fromByteString (BS.reverse bs)) b 0 LL
      (BitPutState builder b 0 LB) -> BitPutState (builder `mappend` B.fromByteString (rev bs)) b 0 LB
      (BitPutState builder b 0 BL) -> BitPutState (builder `mappend` B.fromByteString (rev (BS.reverse bs))) b 0 BL
      (BitPutState _ _ _ BB)       -> putBitsBS (BS.unsafeTail bs) (putBits 8 (BS.unsafeHead bs) s)
      (BitPutState _ _ _ LL)       -> putBitsBS (BS.unsafeInit bs) (putBits 8 (BS.unsafeLast bs) s)
      (BitPutState _ _ _ BL)       -> putBitsBS (BS.unsafeInit bs) (putBits 8 (BS.unsafeLast bs) s)
      (BitPutState _ _ _ LB)       -> putBitsBS (BS.unsafeTail bs) (putBits 8 (BS.unsafeHead bs) s)
   where
      rev    = BS.map (reverseLeastBits 8)


flushIncomplete :: BitPutState -> BitPutState
flushIncomplete s@(BitPutState b w o bo)
  | o == 0 = s
  | otherwise = (BitPutState (b `mappend` B.singleton w) 0 0 bo)

getBitPutLBS :: BitPutState -> LBS.ByteString
getBitPutLBS = B.toLazyByteString . bitPutStateBuilder . flushIncomplete 

getBitPutBS :: BitPutState -> BS.ByteString
getBitPutBS = LBS.toStrict . getBitPutLBS
