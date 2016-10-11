-- | Bit putter
module ViperVM.Format.Binary.Bits.Put
   ( BitPutState(..)
   , newBitPutState
   , putBits
   , putBitsBuffer
   , getBitPutBuffer
   , getBitPutBufferList
   -- * Monadic
   , BitPut
   , BitPutT
   , runBitPut
   , runBitPutT
   , putBitsM
   , putBitBoolM
   , putBitsBufferM
   , changeBitPutOrder
   , withBitPutOrder
   )
where

import Control.Monad.State
import Control.Monad.Identity

import ViperVM.Format.Binary.BufferBuilder as B
import ViperVM.Format.Binary.Buffer
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.BufferList (BufferList)
import ViperVM.Format.Binary.Bits.Order
import ViperVM.Format.Binary.Bits


-- | BitPut state
data BitPutState = BitPutState
   { bitPutStateBuilder          :: !BufferBuilder -- ^ Builder
   , bitPutStateCurrent          :: !Word8         -- ^ Current byte
   , bitPutStateOffset           :: !Word          -- ^ Current offset
   , bitPutStateBitOrder         :: !BitOrder      -- ^ Bit order
   }

-- | Create a new BitPut state
newBitPutState :: BitOrder -> BitPutState
newBitPutState = BitPutState mempty 0 0

-- | Put bits
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
      selectBits :: (FiniteBits a, BitReversable a, Integral a) => a -> Word8
      selectBits x = fromIntegral $ case bo of
         BB ->                       maskLeastBits cn $ x `shiftR` fromIntegral (n-cn)
         LB -> reverseLeastBits cn $ maskLeastBits cn $ x `shiftR` fromIntegral (n-cn)
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
        | o2 == 8   = BitPutState (b2 `mappend` B.fromWord8 w2) 0 0 bo2
        | otherwise = s2


-- | Put a Buffer
--
-- Examples: 3 bits are already written in the current byte
--    BB: ABCDEFGH IJKLMNOP -> xxxABCDE FGHIJKLM NOPxxxxx
--    LL: ABCDEFGH IJKLMNOP -> LMNOPxxx DEFGHIJK xxxxxABC
--    BL: ABCDEFGH IJKLMNOP -> xxxPONML KJIHGFED CBAxxxxx
--    LB: ABCDEFGH IJKLMNOP -> EDCBAxxx MLKJIHGF xxxxxPON
putBitsBuffer :: Buffer -> BitPutState -> BitPutState
putBitsBuffer bs s
   | isBufferEmpty bs = s
   | otherwise  = case s of
      (BitPutState builder b 0 BB) -> BitPutState (builder `mappend` B.fromBuffer bs) b 0 BB
      (BitPutState builder b 0 LL) -> BitPutState (builder `mappend` B.fromBuffer (bufferReverse bs)) b 0 LL
      (BitPutState builder b 0 LB) -> BitPutState (builder `mappend` B.fromBuffer (rev bs)) b 0 LB
      (BitPutState builder b 0 BL) -> BitPutState (builder `mappend` B.fromBuffer (rev (bufferReverse bs))) b 0 BL
      (BitPutState _ _ _ BB)       -> putBitsBuffer (bufferUnsafeTail bs) (putBits 8 (bufferUnsafeHead bs) s)
      (BitPutState _ _ _ LL)       -> putBitsBuffer (bufferUnsafeInit bs) (putBits 8 (bufferUnsafeLast bs) s)
      (BitPutState _ _ _ BL)       -> putBitsBuffer (bufferUnsafeInit bs) (putBits 8 (bufferUnsafeLast bs) s)
      (BitPutState _ _ _ LB)       -> putBitsBuffer (bufferUnsafeTail bs) (putBits 8 (bufferUnsafeHead bs) s)
   where
      rev    = bufferMap reverseBits

-- | Flush the current byte
flushIncomplete :: BitPutState -> BitPutState
flushIncomplete s@(BitPutState b w o bo)
  | o == 0    = s
  | otherwise = BitPutState (b `mappend` B.fromWord8 w) 0 0 bo

-- | Get a lazy byte string
getBitPutBufferList :: BitPutState -> BufferList
getBitPutBufferList = toBufferList . bitPutStateBuilder . flushIncomplete 

-- | Get a Buffer
getBitPutBuffer :: BitPutState -> Buffer
getBitPutBuffer =  toBuffer . bitPutStateBuilder . flushIncomplete

-- | BitPut monad transformer
type BitPutT m a = StateT BitPutState m a

-- | BitPut monad
type BitPut a    = BitPutT Identity a

-- | Evaluate a BitPut monad
runBitPutT :: Monad m => BitOrder -> BitPutT m a -> m Buffer
runBitPutT bo m = getBitPutBuffer <$> execStateT m (newBitPutState bo)

-- | Evaluate a BitPut monad
runBitPut :: BitOrder -> BitPut a -> Buffer
runBitPut bo m = runIdentity (runBitPutT bo m)

-- | Put bits (monadic)
putBitsM :: (Monad m, Integral a, FiniteBits a, BitReversable a) => Word -> a -> BitPutT m ()
putBitsM n w = modify (putBits n w)

-- | Put a single bit (monadic)
putBitBoolM :: (Monad m) => Bool -> BitPutT m ()
putBitBoolM b = putBitsM 1 (if b then 1 else  0 :: Word)

-- | Put a Buffer (monadic)
putBitsBufferM :: Monad m => Buffer -> BitPutT m ()
putBitsBufferM bs = modify (putBitsBuffer bs)

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
