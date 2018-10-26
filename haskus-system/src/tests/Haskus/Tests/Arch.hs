module Haskus.Tests.Arch where

import Test.Tasty

import Haskus.Tests.Arch.Linux

testsArch :: TestTree
testsArch = testGroup "Arch"
   [ testsLinux
   ]
