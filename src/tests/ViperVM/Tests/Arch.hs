module ViperVM.Tests.Arch where

import Distribution.TestSuite (Test,testGroup)

import ViperVM.Tests.Arch.Linux

testsArch :: Test
testsArch = testGroup "Arch"
   [ testsLinux
   ]
