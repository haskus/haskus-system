module Haskus.Tests where

import Distribution.TestSuite (Test)

import Haskus.Tests.Utils
import Haskus.Tests.Format
import Haskus.Tests.System
import Haskus.Tests.Arch

tests :: IO [Test]
tests = return $ 
   [ testsUtils
   , testsFormat
   , testsSystem
   , testsArch
   ]

