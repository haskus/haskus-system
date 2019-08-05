module Haskus.Tests.Common
   ( isBijective
   , isEquivalent
   , ArbitraryByteString (..)
   , ArbitraryByteStringNoNul (..)
   , ArbitraryBuffer (..)
   , ArbitraryBufferNoNul (..)
   )
where


import Test.Tasty.QuickCheck as QC

import qualified Data.ByteString as BS

import Haskus.Binary.Buffer

-- | Ensure a function is bijective
isBijective :: Eq a => (a -> a) -> a -> Bool
isBijective f w = w == (f (f w))

-- | Ensure that two functions return the same thing for the same input
isEquivalent :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
isEquivalent f g x = (f x) == (g x)

-- | Arbitrary ByteString (50 chars long max)
newtype ArbitraryByteString
   = ArbitraryByteString BS.ByteString
   deriving (Show)

instance Arbitrary ArbitraryByteString where
   arbitrary                       = ArbitraryByteString . BS.pack <$> resize 50 (listOf arbitrary)
   shrink (ArbitraryByteString bs)
      | BS.null bs = []
      | otherwise  = [ArbitraryByteString $ BS.take (BS.length bs `div` 2) bs]

-- | Arbitrary ByteString (50 chars long max, no Nul)
newtype ArbitraryByteStringNoNul
   = ArbitraryByteStringNoNul BS.ByteString
   deriving (Show)

instance Arbitrary ArbitraryByteStringNoNul where
   arbitrary                       = ArbitraryByteStringNoNul . BS.pack <$> resize 50 (listOf (choose (1,255))) -- we exclude 0
   shrink (ArbitraryByteStringNoNul bs)
      | BS.null bs = []
      | otherwise  = [ArbitraryByteStringNoNul $ BS.take (BS.length bs `div` 2) bs]

-- | Arbitrary Buffer (50 chars long max)
newtype ArbitraryBuffer
   = ArbitraryBuffer Buffer
   deriving (Show)

instance Arbitrary ArbitraryBuffer where
   arbitrary = do
         ArbitraryByteString bs <- arbitrary
         return (ArbitraryBuffer (Buffer bs))

   shrink (ArbitraryBuffer bs)
      | isBufferEmpty bs = []
      | otherwise        = [ArbitraryBuffer $ bufferTake (bufferSize bs `div` 2) bs]

-- | Arbitrary Buffer (50 chars long max, no Nul)
newtype ArbitraryBufferNoNul
   = ArbitraryBufferNoNul Buffer
   deriving (Show)

instance Arbitrary ArbitraryBufferNoNul where
   arbitrary = do
      ArbitraryByteStringNoNul bs <- arbitrary
      return (ArbitraryBufferNoNul (Buffer bs))
   shrink (ArbitraryBufferNoNul bs)
      | isBufferEmpty bs = []
      | otherwise        = [ArbitraryBufferNoNul $ bufferTake (bufferSize bs `div` 2) bs]
