module Haskus.Tests.Format.Binary where

import Test.Tasty

import Haskus.Tests.Format.Binary.Bits
import Haskus.Tests.Format.Binary.GetPut
import Haskus.Tests.Format.Binary.Vector

testsBinary :: TestTree
testsBinary = testGroup "Binary"
   [ testsBits
   , testsGetPut
   , testsVector
   ]
