module ViperVM.Format.Binary.BitPut
   ( BitPutState(..)
   , newBitPutState
   , putBits
   , putBitsBS
   , getBitPutBS
   , getBitPutLBS
   -- * Monadic
   , BitPut
   , BitPutT
   , runBitPut
   , runBitPutT
   , putBitsM
   , putBitBoolM
   , putBitsBSM
   , changeBitPutOrder
   , withBitPutOrder
   )
where

import Data.Serialize.Builder (Builder)
import qualified Data.Serialize.Builder as B
import Control.Monad.State
import Control.Monad.Identity

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
   , bitPutStateOffset           :: !Word          -- ^ Current offset
   , bitPutStateBitOrder         :: !BitOrder      -- ^ Bit order
   }

newBitPutState :: BitOrder -> BitPutState
newBitPutState = BitPutState B.empty 0 0

putBits :: (Integral a, FiniteBits a, BitReversable a) => Word -> a -> BitPutState -> BitPutState
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
         BL -> w `shiftR` fromIntegral cn
         LL -> w `shiftR` fromIntegral cn
         LB -> w

      -- Select bits to store in the current byte.
      -- Put them in the correct order and return them in the least-significant
      -- bits of the returned value
      selectBits :: (Num a, FiniteBits a, BitReversable a, Integral a) => a -> Word8
      selectBits x = fromIntegral $ case bo of
         BB ->                       maskLeastBits cn $ x `shiftR` (fromIntegral $ n-cn)
         LB -> reverseLeastBits cn $ maskLeastBits cn $ x `shiftR` (fromIntegral $ n-cn)
         LL ->                       maskLeastBits cn x
         BL -> reverseLeastBits cn $ maskLeastBits cn x

      -- shift left at the correct position
      shl :: Word8 -> Word8
      shl x = case bo of
         BB -> x `shiftL` (8 - fromIntegral o - fromIntegral cn)
         BL -> x `shiftL` (8 - fromIntegral o - fromIntegral cn)
         LL -> x `shiftL` fromIntegral o
         LB -> x `shiftL` fromIntegral o

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
      rev    = BS.map reverseBits


flushIncomplete :: BitPutState -> BitPutState
flushIncomplete s@(BitPutState b w o bo)
  | o == 0 = s
  | otherwise = (BitPutState (b `mappend` B.singleton w) 0 0 bo)

getBitPutLBS :: BitPutState -> LBS.ByteString
getBitPutLBS = B.toLazyByteString . bitPutStateBuilder . flushIncomplete 

getBitPutBS :: BitPutState -> BS.ByteString
getBitPutBS = LBS.toStrict . getBitPutLBS


type BitPutT m a = StateT BitPutState m a
type BitPut a    = BitPutT Identity a

runBitPutT :: Monad m => BitOrder -> BitPutT m a -> m BS.ByteString
runBitPutT bo m = getBitPutBS <$> execStateT m (newBitPutState bo)

runBitPut :: BitOrder -> BitPut a -> BS.ByteString
runBitPut bo m = runIdentity (runBitPutT bo m)

putBitsM :: (Monad m, Integral a, FiniteBits a, BitReversable a) => Word -> a -> BitPutT m ()
putBitsM n w = modify (putBits n w)

putBitBoolM :: (Monad m) => Bool -> BitPutT m ()
putBitBoolM b = putBitsM 1 (if b then 1 else  0 :: Word)

putBitsBSM :: Monad m => BS.ByteString -> BitPutT m ()
putBitsBSM bs = modify (putBitsBS bs)

-- | Change the current bit ordering
--
-- Be careful to change the outer bit ordering (B* to L* or the inverse) only
-- on bytes boundaries! Otherwise, you will write the same bits more than once.
changeBitPutOrder :: Monad m => BitOrder -> BitPutT m ()
changeBitPutOrder bo = modify (\s -> s { bitPutStateBitOrder = bo })

-- | Change the bit ordering for the wrapped BitPut
--
-- Be careful, this function uses changeBitPutOrder internally.
withBitPutOrder :: Monad m => BitOrder -> BitPutT m a -> BitPutT m a
withBitPutOrder bo m = do
   bo' <- gets bitPutStateBitOrder
   changeBitPutOrder bo
   v <- m
   changeBitPutOrder bo'
   return v
