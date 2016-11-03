module ViperVM.Tests.Arch.Linux where

import Distribution.TestSuite (Test,testGroup)

import ViperVM.Tests.Arch.Linux.Input
import ViperVM.Tests.Arch.Linux.ErrorCode

testsLinux :: Test
testsLinux = testGroup "Linux"
   [ testsInput
   , testsErrorCode
   ]
