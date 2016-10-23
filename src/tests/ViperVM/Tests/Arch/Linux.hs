module ViperVM.Tests.Arch.Linux where

import Distribution.TestSuite (Test,testGroup)

import ViperVM.Tests.Arch.Linux.Input

testsLinux :: Test
testsLinux = testGroup "Linux"
   [ testsInput
   ]
