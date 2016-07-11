module ViperVM.Tests where

import Distribution.TestSuite (Test)

import ViperVM.Tests.Utils
import ViperVM.Tests.Format

tests :: IO [Test]
tests = return $ 
   [ testsUtils
   , testsFormat
   ]

