module Haskus.Tests.Format.Binary.GetPut
   ( testsGetPut
   )
where

import Distribution.TestSuite (Test,testGroup)
import Distribution.TestSuite.QuickCheck (testProperty)

import Haskus.Tests.Common

import Haskus.Format.Binary.Get
import Haskus.Format.Binary.Buffer

testsGetPut :: Test
testsGetPut = testGroup "Get/Put" $
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
