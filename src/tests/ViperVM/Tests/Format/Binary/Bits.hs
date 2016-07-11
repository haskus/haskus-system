module ViperVM.Tests.Format.Binary.Bits 
   ( testsBits
   )
where

import Distribution.TestSuite (Test,testGroup)
import Distribution.TestSuite.QuickCheck (testProperty)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (elements,choose,vectorOf)

import ViperVM.Tests.Common

import ViperVM.Format.Binary.Bits.Put
import ViperVM.Format.Binary.Bits.Get
import ViperVM.Format.Binary.Bits.Order
import ViperVM.Format.Binary.Bits.Reverse
import ViperVM.Format.Binary.Bits

import ViperVM.Format.Binary.Buffer
import ViperVM.Format.Binary.Get
import ViperVM.Format.Binary.Put
import ViperVM.Format.Binary.VariableLength
import ViperVM.Format.Binary.Word

testsBits :: Test
testsBits = testGroup "Binary bits" $
   [ testGroup "Bits to/from string"
      [ testProperty "Bits from string \"01010011\" (Word8)" (bitsFromString "01010011" == (83 :: Word8))
      , testProperty "Bits from string reverse (Word64)" prop_bits_from_string
      , testProperty "Bits to string (Word8)"            (prop_bits_to_string :: Word8  -> Bool)
      , testProperty "Bits to string (Word16)"           (prop_bits_to_string :: Word16 -> Bool)
      , testProperty "Bits to string (Word32)"           (prop_bits_to_string :: Word32 -> Bool)
      , testProperty "Bits to string (Word64)"           (prop_bits_to_string :: Word64 -> Bool)
      ]
   , testGroup "Bit put/bit get"
      [ testProperty "Bit put/get Word8  - 8  bits"      (prop_reverse_word 8  :: Word8  -> ArbitraryBitOrder -> Bool)
      , testProperty "Bit put/get Word16 - 16 bits"      (prop_reverse_word 16 :: Word16 -> ArbitraryBitOrder -> Bool)
      , testProperty "Bit put/get Word32 - 32 bits"      (prop_reverse_word 32 :: Word32 -> ArbitraryBitOrder -> Bool)
      , testProperty "Bit put/get Word64 - 64 bits"      (prop_reverse_word 64 :: Word64 -> ArbitraryBitOrder -> Bool)
      , testProperty "Bit put/get Word8  - [1,8]  bits"  (prop_reverse_word_size :: Size8  -> Word8   -> ArbitraryBitOrder -> Bool)
      , testProperty "Bit put/get Word16 - [1,16] bits"  (prop_reverse_word_size :: Size16 -> Word16  -> ArbitraryBitOrder -> Bool)
      , testProperty "Bit put/get Word32 - [1,32] bits"  (prop_reverse_word_size :: Size32 -> Word32  -> ArbitraryBitOrder -> Bool)
      , testProperty "Bit put/get Word64 - [1,64] bits"  (prop_reverse_word_size :: Size64 -> Word64  -> ArbitraryBitOrder -> Bool)
      , testProperty "Monadic BitPut/BitGet, two parts of two Word64"
         (prop_split_word :: Size64 -> Size64 -> Word64 -> Word64 -> ArbitraryBitOrder -> Bool)
      , testProperty "Monadic BitPut/BitGet, bytestring with offset" prop_reverse_bs
      ]
   , testGroup "Variable length (LEB128)"
      [ testProperty "Put/Get reverse (Word8)"           (prop_uleb128_reverse :: Word8 -> Bool)
      , testProperty "Put/Get reverse (Word16)"          (prop_uleb128_reverse :: Word16 -> Bool)
      , testProperty "Put/Get reverse (Word32)"          (prop_uleb128_reverse :: Word32 -> Bool)
      , testProperty "Put/Get reverse (Word64)"          (prop_uleb128_reverse :: Word64 -> Bool)
      ]
   , testGroup "Reverse bits (Word8)"
      [ testProperty "Reverse bits in a Word8"  (reverseBits (0x28 :: Word8) == 0x14)
      , testProperty "Bijective: obvious"       (isBijective (reverseBitsObvious :: Word8 -> Word8))
      , testProperty "Bijective: 3Ops"          (isBijective reverseBits3Ops)
      , testProperty "Bijective: 4Ops"          (isBijective reverseBits4Ops)
      , testProperty "Bijective: lookup table"  (isBijective reverseBitsTable)
      , testProperty "Bijective: 7Ops"          (isBijective reverseBits7Ops)
      , testProperty "Bijective: 5LgN"          (isBijective (reverseBits5LgN :: Word8 -> Word8))
      , testProperty "Equivalent: obvious"      (isEquivalent (reverseBits :: Word8 -> Word8) reverseBitsObvious)
      , testProperty "Equivalent: 3Ops"         (isEquivalent (reverseBits :: Word8 -> Word8) reverseBits3Ops)
      , testProperty "Equivalent: 4Ops"         (isEquivalent (reverseBits :: Word8 -> Word8) reverseBits4Ops)
      , testProperty "Equivalent: lookup table" (isEquivalent (reverseBits :: Word8 -> Word8) reverseBitsTable)
      , testProperty "Equivalent: 7Ops"         (isEquivalent (reverseBits :: Word8 -> Word8) reverseBits7Ops)
      , testProperty "Equivalent: 5LgN"         (isEquivalent (reverseBits :: Word8 -> Word8) reverseBits5LgN)
      ]
   , testGroup "Reverse bits (Word16)"
      [ testProperty "Reverse bits in a Word16" (reverseBits (0x2817 :: Word16) == 0xe814)
      , testProperty "Bijective: obvious"       (isBijective (                reverseBitsObvious :: Word16 -> Word16))
      , testProperty "Bijective: 3Ops"          (isBijective (liftReverseBits reverseBits3Ops    :: Word16 -> Word16))
      , testProperty "Bijective: 4Ops"          (isBijective (liftReverseBits reverseBits4Ops    :: Word16 -> Word16))
      , testProperty "Bijective: lookup table"  (isBijective (liftReverseBits reverseBitsTable   :: Word16 -> Word16))
      , testProperty "Bijective: 7Ops"          (isBijective (liftReverseBits reverseBits7Ops    :: Word16 -> Word16))
      , testProperty "Bijective: 5LgN"          (isBijective (                reverseBits5LgN    :: Word16 -> Word16))
      , testProperty "Equivalent: obvious"      (isEquivalent (reverseBits :: Word16 -> Word16) reverseBitsObvious)
      , testProperty "Equivalent: 3Ops"         (isEquivalent (reverseBits :: Word16 -> Word16) (liftReverseBits reverseBits3Ops))
      , testProperty "Equivalent: 4Ops"         (isEquivalent (reverseBits :: Word16 -> Word16) (liftReverseBits reverseBits4Ops))
      , testProperty "Equivalent: lookup table" (isEquivalent (reverseBits :: Word16 -> Word16) (liftReverseBits reverseBitsTable))
      , testProperty "Equivalent: 7Ops"         (isEquivalent (reverseBits :: Word16 -> Word16) (liftReverseBits reverseBits7Ops))
      , testProperty "Equivalent: 5LgN"         (isEquivalent (reverseBits :: Word16 -> Word16) reverseBits5LgN)
      ]
   , testGroup "Reverse bits (Word32)"
      [ testProperty "Reverse bits in a Word32" (reverseBits (0x28173456 :: Word32) == 0x6a2ce814)
      , testProperty "Bijective: obvious"       (isBijective (                reverseBitsObvious :: Word32 -> Word32))
      , testProperty "Bijective: 3Ops"          (isBijective (liftReverseBits reverseBits3Ops    :: Word32 -> Word32))
      , testProperty "Bijective: 4Ops"          (isBijective (liftReverseBits reverseBits4Ops    :: Word32 -> Word32))
      , testProperty "Bijective: lookup table"  (isBijective (liftReverseBits reverseBitsTable   :: Word32 -> Word32))
      , testProperty "Bijective: 7Ops"          (isBijective (liftReverseBits reverseBits7Ops    :: Word32 -> Word32))
      , testProperty "Bijective: 5LgN"          (isBijective (                reverseBits5LgN    :: Word32 -> Word32))
      , testProperty "Equivalent: obvious"      (isEquivalent (reverseBits :: Word32 -> Word32) reverseBitsObvious)
      , testProperty "Equivalent: 3Ops"         (isEquivalent (reverseBits :: Word32 -> Word32) (liftReverseBits reverseBits3Ops))
      , testProperty "Equivalent: 4Ops"         (isEquivalent (reverseBits :: Word32 -> Word32) (liftReverseBits reverseBits4Ops))
      , testProperty "Equivalent: lookup table" (isEquivalent (reverseBits :: Word32 -> Word32) (liftReverseBits reverseBitsTable))
      , testProperty "Equivalent: 7Ops"         (isEquivalent (reverseBits :: Word32 -> Word32) (liftReverseBits reverseBits7Ops))
      , testProperty "Equivalent: 5LgN"         (isEquivalent (reverseBits :: Word32 -> Word32) reverseBits5LgN)
      ]
   , testGroup "Reverse bits (Word64)"
      [ testProperty "Reverse bits in a Word64" (reverseBits (0x2800017003450060 :: Word64) == 0x0600a2c00e800014)
      , testProperty "Bijective: obvious"       (isBijective (                reverseBitsObvious :: Word64 -> Word64))
      , testProperty "Bijective: 3Ops"          (isBijective (liftReverseBits reverseBits3Ops    :: Word64 -> Word64))
      , testProperty "Bijective: 4Ops"          (isBijective (liftReverseBits reverseBits4Ops    :: Word64 -> Word64))
      , testProperty "Bijective: lookup table"  (isBijective (liftReverseBits reverseBitsTable   :: Word64 -> Word64))
      , testProperty "Bijective: 7Ops"          (isBijective (liftReverseBits reverseBits7Ops    :: Word64 -> Word64))
      , testProperty "Bijective: 5LgN"          (isBijective (                reverseBits5LgN    :: Word64 -> Word64))
      , testProperty "Equivalent: obvious"      (isEquivalent (reverseBits :: Word64 -> Word64) reverseBitsObvious)
      , testProperty "Equivalent: 3Ops"         (isEquivalent (reverseBits :: Word64 -> Word64) (liftReverseBits reverseBits3Ops))
      , testProperty "Equivalent: 4Ops"         (isEquivalent (reverseBits :: Word64 -> Word64) (liftReverseBits reverseBits4Ops))
      , testProperty "Equivalent: lookup table" (isEquivalent (reverseBits :: Word64 -> Word64) (liftReverseBits reverseBitsTable))
      , testProperty "Equivalent: 7Ops"         (isEquivalent (reverseBits :: Word64 -> Word64) (liftReverseBits reverseBits7Ops))
      , testProperty "Equivalent: 5LgN"         (isEquivalent (reverseBits :: Word64 -> Word64) reverseBits5LgN)
      ]
   ]

newtype ArbitraryBitOrder = ArbitraryBitOrder BitOrder deriving (Show)

instance Arbitrary ArbitraryBitOrder where
   arbitrary = elements $ fmap ArbitraryBitOrder [BB,LB,BL,LL]
   shrink (ArbitraryBitOrder x) = case x of
      LL -> fmap ArbitraryBitOrder [BB,LB,BL]
      BL -> fmap ArbitraryBitOrder [BB,LB]
      LB -> fmap ArbitraryBitOrder [BB]
      BB -> fmap ArbitraryBitOrder []

class Size x where
   fromSize :: x -> Word

newtype Size8  = Size8  Word deriving (Show)
newtype Size16 = Size16 Word deriving (Show)
newtype Size32 = Size32 Word deriving (Show)
newtype Size64 = Size64 Word deriving (Show)

instance Size Size8  where fromSize (Size8  x) = x
instance Size Size16 where fromSize (Size16 x) = x
instance Size Size32 where fromSize (Size32 x) = x
instance Size Size64 where fromSize (Size64 x) = x

instance Arbitrary Size8  where arbitrary = fmap Size8  $ choose (1,8)
instance Arbitrary Size16 where arbitrary = fmap Size16 $ choose (1,16)
instance Arbitrary Size32 where arbitrary = fmap Size32 $ choose (1,32)
instance Arbitrary Size64 where arbitrary = fmap Size64 $ choose (1,64)


newtype BitString = BitString String deriving (Show)

instance Arbitrary BitString where
   arbitrary = fmap BitString $ vectorOf 64 (elements ['0','1'])

-- | Test that a random BitString (i.e. a string with length 64 and only
-- composed of 0s and 1s) can be converted into a Word64 and back into a string
prop_bits_from_string :: BitString -> Bool
prop_bits_from_string (BitString s) = bitsToString (bitsFromString s :: Word64) == s

-- | Test that a word can be converted into a BitString and back
prop_bits_to_string :: FiniteBits a => a -> Bool
prop_bits_to_string x = bitsFromString (bitsToString x) == x

-- | Test that words of the given length can be written and read back with
-- BitGet/BitPut. Test every bit ordering.
prop_reverse_word :: (Integral a, FiniteBits a, BitReversable a) => Word -> a -> ArbitraryBitOrder -> Bool
prop_reverse_word n w (ArbitraryBitOrder bo) = maskLeastBits n w == dec
   where
      enc = getBitPutBuffer  $ putBits n w $ newBitPutState bo
      dec = getBits n $ newBitGetState bo enc

-- | Test that a ByteString can be written and read back with
-- BitGet/BitPut. Test every bit ordering.
prop_reverse_bs :: Word64 -> Size64 -> ArbitraryBuffer -> ArbitraryBitOrder -> Bool
prop_reverse_bs w s (ArbitraryBuffer bs) (ArbitraryBitOrder bo) = runBitGet bo dec (runBitPut bo enc)
   where
      len = bufferSize bs
      enc = do
         putBitsM (fromSize s) w
         putBitsBufferM bs
      dec = do
         w2  <- getBitsM (fromSize s)
         bs' <- getBitsBSM (fromIntegral len)
         return (bs == bs' && w2 == maskLeastBits (fromSize s) w)

-- | Test that words with arbitrary (but still valid) lengths can be written and
-- read back with BitGet/BitPut. Test every bit ordering.
prop_reverse_word_size :: (Integral a, FiniteBits a, BitReversable a, Size s) => s -> a -> ArbitraryBitOrder -> Bool
prop_reverse_word_size n w bo = prop_reverse_word (fromSize n) w bo

-- | Write two parts of two words and read them back
prop_split_word :: (Num a, Integral a, FiniteBits a, BitReversable a,
                    Num b, Integral b, FiniteBits b, BitReversable b,
                    Size s1, Size s2) => s1 -> s2 -> a -> b -> ArbitraryBitOrder -> Bool
prop_split_word s1 s2 w1 w2 (ArbitraryBitOrder bo) = runBitGet bo dec (runBitPut bo enc)
   where
      enc = do
         putBitsM (fromSize s1) w1
         putBitsM (fromSize s2) w2
      dec = do
         v1 <- getBitsM (fromSize s1)
         v2 <- getBitsM (fromSize s2)
         return (v1 == maskLeastBits (fromSize s1) w1 && v2 == maskLeastBits (fromSize s2) w2)

-- | Test that ULEB128 decoder can read back what has been written with ULEB128
-- encoder
prop_uleb128_reverse :: (Integral a, Bits a) => a -> Bool
prop_uleb128_reverse w = w == runGetOrFail getULEB128 (runPut (putULEB128 w))
