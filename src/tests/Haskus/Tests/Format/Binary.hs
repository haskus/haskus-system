module Haskus.Tests.Format.Binary where

import Distribution.TestSuite (Test,testGroup)

import Haskus.Tests.Format.Binary.Bits
import Haskus.Tests.Format.Binary.GetPut
import Haskus.Tests.Format.Binary.Vector

testsBinary :: Test
testsBinary = testGroup "Binary"
   [ testsBits
   , testsGetPut
   , testsVector
   ]
