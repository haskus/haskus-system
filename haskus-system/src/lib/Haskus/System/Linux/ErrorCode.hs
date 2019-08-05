{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Management of returned values from syscalls
module Haskus.System.Linux.ErrorCode 
   ( ErrorCode (..)
   , unhdlErr
   , checkErrorCode
   , checkErrorCode_
   )
where

import Haskus.Number.Int
import Haskus.Binary.Enum
import Haskus.Utils.Flow
import Haskus.System.Linux.Internals.Error

-- | Error to call when a syscall returns an unexpected error value
unhdlErr :: Show err => String -> err -> a
unhdlErr str err =
   error ("Unhandled error "++ show err ++" returned by \""++str++"\". Report this as a haskus-system bug.")

-- | Convert negative values into error codes
checkErrorCode :: Monad m => Int64 -> Excepts '[ErrorCode] m Int64
{-# INLINABLE checkErrorCode #-}
checkErrorCode r
   | r < 0     = failureE (toCEnum (abs r))
   | otherwise = pure r

-- | Convert negative values into error codes, return () otherwise
checkErrorCode_ :: Monad m => Int64 -> Excepts '[ErrorCode] m ()
{-# INLINABLE checkErrorCode_ #-}
checkErrorCode_ r
   | r < 0     = failureE (toCEnum (abs r))
   | otherwise = pure ()
