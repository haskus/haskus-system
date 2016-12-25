module Haskus.Tests.System where

import Distribution.TestSuite (Test,testGroup)

import Haskus.Tests.System.Devices

testsSystem :: Test
testsSystem = testGroup "System"
   [ testsDevices
   ]
