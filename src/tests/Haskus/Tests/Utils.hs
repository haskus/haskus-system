module Haskus.Tests.Utils where

import Distribution.TestSuite (Test,testGroup)

import Haskus.Tests.Utils.HArray
import Haskus.Tests.Utils.Variant

testsUtils :: Test
testsUtils = testGroup "Utils"
   [ testsHArray
   , testsVariant
   ]


