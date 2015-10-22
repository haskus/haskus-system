module Tests where

import Distribution.TestSuite (Test)

import BinaryBits
import BinaryGetPut

tests :: IO [Test]
tests = return
   [ binaryBitsTests
   , binaryGetPutTests
   ]

