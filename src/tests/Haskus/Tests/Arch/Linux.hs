module Haskus.Tests.Arch.Linux where

import Distribution.TestSuite (Test,testGroup)

import Haskus.Tests.Arch.Linux.Input
import Haskus.Tests.Arch.Linux.ErrorCode

testsLinux :: Test
testsLinux = testGroup "Linux"
   [ testsInput
   , testsErrorCode
   ]
