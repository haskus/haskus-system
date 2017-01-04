module Haskus.Tests.Arch.Linux where


import Test.Tasty

import Haskus.Tests.Arch.Linux.Input
import Haskus.Tests.Arch.Linux.ErrorCode

testsLinux :: TestTree
testsLinux = testGroup "Linux"
   [ testsInput
   , testsErrorCode
   ]
