module ViperVM.Tests.Format.Binary where

import Distribution.TestSuite (Test,testGroup)

import ViperVM.Tests.Format.Binary.Bits
import ViperVM.Tests.Format.Binary.GetPut
import ViperVM.Tests.Format.Binary.Vector

testsBinary :: Test
testsBinary = testGroup "Binary"
   [ testsBits
   , testsGetPut
   , testsVector
   ]
