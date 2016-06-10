module BinaryGetPut
   ( binaryGetPutTests
   )
where

import Distribution.TestSuite (Test,testGroup)
import Distribution.TestSuite.QuickCheck (testProperty)

import Common

import ViperVM.Format.Binary.Get
import ViperVM.Format.Binary.Buffer

binaryGetPutTests :: Test
binaryGetPutTests = testGroup "Binary Get/Put" $
   [ testGroup "getBufferNul"
      [ testProperty "Read two successives strings" getBufferNul_basic
      ]
   ]


getBufferNul_basic :: ArbitraryBufferNoNul -> ArbitraryBufferNoNul -> Bool
getBufferNul_basic (ArbitraryBufferNoNul s1) (ArbitraryBufferNoNul s2) = runGetOrFail getter str
   where
      str    = (s1 `bufferSnoc` 0) `bufferAppend` s2
      getter = do
         a <- getBufferNul
         b <- getBufferNul
         return (a == s1 && b == s2)
