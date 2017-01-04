module Haskus.Tests.Format where

import Test.Tasty

import Haskus.Tests.Format.Binary

testsFormat :: TestTree
testsFormat = testGroup "Format"
   [ testsBinary
   ]
