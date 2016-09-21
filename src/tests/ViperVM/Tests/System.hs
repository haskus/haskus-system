module ViperVM.Tests.System where

import Distribution.TestSuite (Test,testGroup)

import ViperVM.Tests.System.Devices

testsSystem :: Test
testsSystem = testGroup "System"
   [ testsDevices
   ]
