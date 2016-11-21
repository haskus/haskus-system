module ViperVM.Tests.Utils where

import Distribution.TestSuite (Test,testGroup)

import ViperVM.Tests.Utils.HArray
import ViperVM.Tests.Utils.Variant

testsUtils :: Test
testsUtils = testGroup "Utils"
   [ testsHArray
   , testsVariant
   ]


