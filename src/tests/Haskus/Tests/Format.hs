module Haskus.Tests.Format where

import Distribution.TestSuite (Test,testGroup)

import Haskus.Tests.Format.Binary

testsFormat :: Test
testsFormat = testGroup "Format"
   [ testsBinary
   ]
