module Haskus.Tests.System where

import Test.Tasty

import Haskus.Tests.System.Devices

testsSystem :: TestTree
testsSystem = testGroup "System"
   [ testsDevices
   ]
