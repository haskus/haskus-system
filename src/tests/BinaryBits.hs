module BinaryBits where

import Distribution.TestSuite (Test,testGroup)
import Distribution.TestSuite.QuickCheck (testProperty)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (elements,choose,vectorOf)

import Data.Word
import Data.Bits

import ViperVM.Format.Binary.BitPut
import ViperVM.Format.Binary.BitGet
import ViperVM.Format.Binary.BitOrder
import ViperVM.Format.Binary.BitOps
import ViperVM.Format.Binary.Get
import ViperVM.Format.Binary.Put
import ViperVM.Format.Binary.VariableLength

tests :: IO [Test]
tests = return
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
      ]
   , testGroup "Variable length (LEB128)"
      [ testProperty "Put/Get reverse (Word8)"           (prop_uleb128_reverse :: Word8 -> Bool)
      , testProperty "Put/Get reverse (Word16)"          (prop_uleb128_reverse :: Word16 -> Bool)
      , testProperty "Put/Get reverse (Word32)"          (prop_uleb128_reverse :: Word32 -> Bool)
      , testProperty "Put/Get reverse (Word64)"          (prop_uleb128_reverse :: Word64 -> Bool)
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
   fromSize :: x -> Int

newtype Size8  = Size8  Int deriving (Show)
newtype Size16 = Size16 Int deriving (Show)
newtype Size32 = Size32 Int deriving (Show)
newtype Size64 = Size64 Int deriving (Show)

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

prop_bits_from_string :: BitString -> Bool
prop_bits_from_string (BitString s) = bitsToString (bitsFromString s :: Word64) == s

prop_bits_to_string :: FiniteBits a => a -> Bool
prop_bits_to_string x = bitsFromString (bitsToString x) == x

prop_reverse_word :: (Integral a, Bits a) => Int -> a -> ArbitraryBitOrder -> Bool
prop_reverse_word n w (ArbitraryBitOrder bo) = maskLeastBits n w == dec
   where
      enc = getBitPutBS $ putBits n w $ newBitPutState bo
      dec = readWord n  $ newBitGetState bo enc

prop_reverse_word_size :: (Integral a, Bits a, Size s) => s -> a -> ArbitraryBitOrder -> Bool
prop_reverse_word_size n w bo = prop_reverse_word (fromSize n) w bo


prop_uleb128_reverse :: (Integral a, Bits a) => a -> Bool
prop_uleb128_reverse w = w == dec
   where
      enc = runPut (putULEB128 w)
      dec = runGet getULEB128 enc
