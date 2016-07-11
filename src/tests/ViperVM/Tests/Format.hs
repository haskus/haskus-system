module ViperVM.Tests.Format where

import Distribution.TestSuite (Test,testGroup)

import ViperVM.Tests.Format.Binary

testsFormat :: Test
testsFormat = testGroup "Format"
   [ testsBinary
   ]
