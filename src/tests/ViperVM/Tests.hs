module ViperVM.Tests where

import Distribution.TestSuite (Test)

import ViperVM.Tests.Utils
import ViperVM.Tests.Format
import ViperVM.Tests.System

tests :: IO [Test]
tests = return $ 
   [ testsUtils
   , testsFormat
   , testsSystem
   ]

