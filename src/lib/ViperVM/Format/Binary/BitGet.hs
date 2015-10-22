{-# LANGUAGE BangPatterns #-}

module ViperVM.Format.Binary.BitGet
   ( BitGetState(..)
   , newBitGetState
   , skipBits
   , getBits
   , getBitsChecked
   , getBitsBS
   -- * Monadic
   , BitGet
   , BitGetT
   , runBitGet
   , runBitGetT
   , skipBitsM
   , getBitsM
   , getBitsCheckedM
   , getBitBoolM
   , changeBitGetOrder
   , withBitGetOrder
   )
where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (poke)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.State
import Control.Monad.Identity

import ViperVM.Format.Binary.BitOrder
import ViperVM.Format.Binary.BitOps

-- | BitGet state
data BitGetState = BitGetState
   { bitGetStateInput      :: {-# UNPACK #-} !ByteString -- ^ Input
   , bitGetStateBitOffset  :: {-# UNPACK #-} !Word       -- ^ Bit offset (0-7)
   , bitGetStateBitOrder   ::                !BitOrder   -- ^ Bit order
   } deriving (Show)

-- | Create a new BitGetState
newBitGetState :: BitOrder -> ByteString -> BitGetState
newBitGetState bo bs = BitGetState bs 0 bo

-- | Skip the given number of bits from the input
skipBits :: Word -> BitGetState -> BitGetState
skipBits o (BitGetState bs n bo) = BitGetState (BS.unsafeDrop d bs) n' bo
   where
      !o' = (n+o)
      !d  = fromIntegral $ byteOffset o'
      !n' = bitOffset o'

-- | Read the given number of bits and put the result in a word
getBits :: (Integral a, FiniteBits a, BitReversable a) => Word -> BitGetState -> a
getBits nbits (BitGetState bs off bo) = rec zeroBits 0 bs off nbits
   where
      -- w   = current result
      -- n   = number of valid bits in w
      -- i   = input bytestring
      -- r   = number of remaining bits to read
      -- o   = bit offset in input bytestring
      rec w _ _ _ 0 = w
      rec w n i o r = rec nw (n+nb) (BS.tail i) o' (r-nb)
         where 
            -- current Word8
            c  = BS.head i
            -- number of bits to take from the current Word8
            nb = min (8-o) r
            -- bits taken from the current Word8 and put in correct order in least-significant bits
            tc = fromIntegral $ case bo of
                  BB -> maskLeastBits nb $               c `shiftR` (8 - fromIntegral nb - fromIntegral o)
                  BL -> maskLeastBits nb $ (reverseBits c) `shiftR` (fromIntegral o)
                  LB -> maskLeastBits nb $ (reverseBits c) `shiftR` (8 - fromIntegral nb - fromIntegral o)
                  LL -> maskLeastBits nb $               c `shiftR` (fromIntegral o)
            -- mix new bits with the current result
            nw = case bo of
                  BB -> (w `shiftL` fromIntegral nb) .|. tc
                  LB -> (w `shiftL` fromIntegral nb) .|. tc
                  BL -> (tc `shiftL` fromIntegral n) .|. w
                  LL -> (tc `shiftL` fromIntegral n) .|. w
            -- new offset ((o + nb) `mod` 8)
            o' = bitOffset (o + nb)

-- | Perform some checks before calling getBits
--
-- Check that the number of bits to read is not greater than the first parameter
getBitsChecked :: (Integral a, FiniteBits a, BitReversable a) => Word -> Word -> BitGetState -> a
getBitsChecked m n s
   | n > m     = error $ "Tried to read more than " ++ show m ++ " bits (" ++ show n ++")"
   | otherwise = getBits n s
{-# INLINE getBitsChecked #-}

-- | Read the given number of bytes and return them in Big-Endian order
--
-- Examples:
--    BB: xxxABCDE FGHIJKLM NOPxxxxx -> ABCDEFGH IJKLMNOP
--    LL: LMNOPxxx DEFGHIJK xxxxxABC -> ABCDEFGH IJKLMNOP
--    BL: xxxPONML KJIHGFED CBAxxxxx -> ABCDEFGH IJKLMNOP
--    LB: EDCBAxxx MLKJIHGF xxxxxPON -> ABCDEFGH IJKLMNOP
getBitsBS :: Int -> BitGetState -> ByteString
getBitsBS n (BitGetState bs o bo) =
   let 
      bs'  = BS.unsafeTake (n+1) bs
      bs'' = BS.unsafeTake n bs
      rev  = BS.map (reverseLeastBits 8)
   in case (o,bo) of
      (0,BB) -> bs''
      (0,LL) -> BS.reverse bs''
      (0,LB) -> rev bs''
      (0,BL) -> rev . BS.reverse $ bs''
      (_,LL) -> getBitsBS n (BitGetState (BS.reverse bs') (8-o) BB)
      (_,BL) -> rev . BS.reverse $ getBitsBS n (BitGetState bs' o BB)
      (_,LB) -> rev . BS.reverse $ getBitsBS n (BitGetState bs' o LL)
      (_,BB) -> unsafePerformIO $ do
         let len = n+1
         ptr <- mallocBytes len
         let f r i = do
               let
                  w  = BS.unsafeIndex bs (len-i)
                  w' = (w `shiftL` fromIntegral o) .|. r
                  r' = w `shiftR` (8-fromIntegral o)
               poke (ptr `plusPtr` (len-i)) w'
               return r'
         foldM_ f 0 [1..len]
         BS.unsafeInit <$> BS.unsafePackMallocCStringLen (ptr,len)




type BitGetT m a = StateT BitGetState m a
type BitGet a    = BitGetT Identity a

runBitGetT :: Monad m => BitOrder -> BitGetT m a -> BS.ByteString -> m a
runBitGetT bo m bs = evalStateT m (newBitGetState bo bs)

runBitGet :: BitOrder -> BitGet a -> BS.ByteString -> a
runBitGet bo m bs = runIdentity (runBitGetT bo m bs)

-- | Skip the given number of bits from the input (monadic version)
skipBitsM :: Monad m => Word -> BitGetT m ()
skipBitsM = modify . skipBits

-- | Read the given number of bits and put the result in a word
getBitsM :: (Integral a, FiniteBits a, BitReversable a, Monad m) => Word -> BitGetT m a
getBitsM n = do
   v <- gets (getBits n)
   skipBitsM n
   return v

-- | Perform some checks before calling getBitsM
getBitsCheckedM :: (Integral a, FiniteBits a, BitReversable a, Monad m) => Word -> Word -> BitGetT m a
getBitsCheckedM m n = do
   v <- gets (getBitsChecked m n)
   skipBitsM n
   return v

-- | Get a bit and convert it into a Bool
getBitBoolM :: (Monad m) => BitGetT m Bool
getBitBoolM = do
   v <- getBitsM 1
   return ((v :: Word) == 1)

-- | Change the current bit ordering
--
-- Be careful to change the outer bit ordering (B* to L* or the inverse) only
-- on bytes boundaries! Otherwise, you will read the same bits more than once.
changeBitGetOrder :: Monad m => BitOrder -> BitGetT m ()
changeBitGetOrder bo = modify (\s -> s { bitGetStateBitOrder = bo })

-- | Change the bit ordering for the wrapped BitGet
--
-- Be careful, this function uses changeBitGetOrder internally.
withBitGetOrder :: Monad m => BitOrder -> BitGetT m a -> BitGetT m a
withBitGetOrder bo m = do
   bo' <- gets bitGetStateBitOrder
   changeBitGetOrder bo
   v <- m
   changeBitGetOrder bo'
   return v

