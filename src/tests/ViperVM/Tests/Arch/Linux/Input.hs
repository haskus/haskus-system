module ViperVM.Tests.Arch.Linux.Input
   ( testsInput
   )
where

import Distribution.TestSuite (Test,testGroup)
import Distribution.TestSuite.QuickCheck (testProperty)

import ViperVM.Arch.Linux.Internals.Input
import ViperVM.Format.Binary.Enum
import ViperVM.Format.Binary.Word

testsInput :: Test
testsInput = testGroup "Device tree"
   [ testProperty "Key's enum" 
      (toCEnum (0x270 :: Word16) == NextFavorite)
   ]
