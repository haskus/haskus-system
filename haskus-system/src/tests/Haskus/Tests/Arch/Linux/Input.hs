module Haskus.Tests.Arch.Linux.Input
   ( testsInput
   )
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Haskus.System.Linux.Internals.Input
import Haskus.Format.Binary.Enum
import Haskus.Format.Binary.Word

testsInput :: TestTree
testsInput = testGroup "Input"
   [ testProperty "Key's enum" 
      (toCEnum (0x270 :: Word16) == NextFavorite)
   ]
