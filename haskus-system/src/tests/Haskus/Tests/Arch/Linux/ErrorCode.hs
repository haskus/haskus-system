module Haskus.Tests.Arch.Linux.ErrorCode
   ( testsErrorCode
   )
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Haskus.System.Linux.ErrorCode
import Haskus.Format.Binary.Enum
import Haskus.Format.Binary.Word

testsErrorCode :: TestTree
testsErrorCode = testGroup "Error codes"
   [ testProperty "ErrorCode's enum EBUSY"
      (toCEnum (16  :: Word64) == EBUSY)
   , testProperty "ErrorCode's enum EDOTDOT"
      (toCEnum (73 :: Word64) == EDOTDOT)
   , testProperty "ErrorCode's enum ENETDOWN"
      (toCEnum (100 :: Word64) == ENETDOWN)
   , testProperty "ErrorCode's enum EINPROGRESS"
      (toCEnum (115 :: Word64) == EINPROGRESS)
   , testProperty "ErrorCode's enum EHWPOISON"
      (toCEnum (133 :: Word64) == EHWPOISON)
   , testProperty "ErrorCode's enum EOTHER"
      (toCEnum (150 :: Word64) == EOTHER 150)
   ]

