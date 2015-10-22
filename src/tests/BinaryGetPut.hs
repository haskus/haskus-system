module BinaryGetPut
   ( binaryGetPutTests
   )
where

import Distribution.TestSuite (Test,testGroup)
import Distribution.TestSuite.QuickCheck (testProperty)

import qualified Data.ByteString as BS

import Common

import ViperVM.Format.Binary.Get

binaryGetPutTests :: Test
binaryGetPutTests = testGroup "Binary Get/Put" $
   [ testGroup "getByteStringNul"
      [ testProperty "Read two successives strings" getByteStringNul_basic
      ]
   ]


getByteStringNul_basic :: ArbitraryByteStringNoNul -> ArbitraryByteStringNoNul -> Bool
getByteStringNul_basic (ArbitraryByteStringNoNul s1) (ArbitraryByteStringNoNul s2) = runGetOrFail getter str
   where
      str    = (s1 `BS.snoc` 0) `BS.append` s2
      getter = do
         a <- getByteStringNul
         b <- getByteStringNul
         return (a == s1 && b == s2)
