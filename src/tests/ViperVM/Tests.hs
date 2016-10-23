module ViperVM.Tests where

import Distribution.TestSuite (Test)

import ViperVM.Tests.Utils
import ViperVM.Tests.Format
import ViperVM.Tests.System
import ViperVM.Tests.Arch

tests :: IO [Test]
tests = return $ 
   [ testsUtils
   , testsFormat
   , testsSystem
   , testsArch
   ]

