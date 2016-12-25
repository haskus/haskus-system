module Haskus.Tests.Arch.Linux.Input
   ( testsInput
   )
where

import Distribution.TestSuite (Test,testGroup)
import Distribution.TestSuite.QuickCheck (testProperty)

import Haskus.Arch.Linux.Internals.Input
import Haskus.Format.Binary.Enum
import Haskus.Format.Binary.Word

testsInput :: Test
testsInput = testGroup "Input"
   [ testProperty "Key's enum" 
      (toCEnum (0x270 :: Word16) == NextFavorite)
   ]
