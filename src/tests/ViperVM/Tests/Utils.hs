module ViperVM.Tests.Utils where

import Distribution.TestSuite (Test,testGroup)

import ViperVM.Tests.Utils.HArray

testsUtils :: Test
testsUtils = testGroup "Utils"
   [ testsHArray
   ]


