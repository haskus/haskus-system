module Haskus.Tests where

import Test.Tasty

import Haskus.Tests.Utils
import Haskus.Tests.Format
import Haskus.Tests.System
import Haskus.Tests.Arch

tests :: TestTree
tests = testGroup "Tests" $ 
   [ testsUtils
   , testsFormat
   , testsSystem
   , testsArch
   ]

