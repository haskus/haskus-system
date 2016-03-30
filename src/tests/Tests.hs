module Tests where

import Distribution.TestSuite (Test)

import BinaryBits
import BinaryGetPut
import UtilsHArray

tests :: IO [Test]
tests = return
   [ utilsHArrayTests
   , binaryBitsTests
   , binaryGetPutTests
   ]

