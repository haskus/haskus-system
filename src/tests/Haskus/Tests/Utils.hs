module Haskus.Tests.Utils where

import Test.Tasty

import Haskus.Tests.Utils.HArray
import Haskus.Tests.Utils.Variant

testsUtils :: TestTree
testsUtils = testGroup "Utils"
   [ testsHArray
   , testsVariant
   ]


