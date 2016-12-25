module Haskus.Tests.Arch where

import Distribution.TestSuite (Test,testGroup)

import Haskus.Tests.Arch.Linux

testsArch :: Test
testsArch = testGroup "Arch"
   [ testsLinux
   ]
