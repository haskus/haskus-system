{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Management of returned values from syscalls
module Haskus.System.Linux.ErrorCode 
   ( ErrorCode (..)
   , unhdlErr
   , toErrorCode
   , toErrorCodeVoid
   , toErrorCodePure
   )
where

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Enum
import Haskus.Utils.Variant
import Haskus.System.Linux.Internals.Error

-- | Convert negative values into error codes
toErrorCode :: Int64 -> V '[Int64,ErrorCode]
{-# INLINE toErrorCode #-}
toErrorCode r
   | r < 0     = toVariantAt @1 (toCEnum (abs r))
   | otherwise = toVariantAt @0 r

-- | Convert negative values into error codes, return () otherwise
toErrorCodeVoid :: Int64 -> V '[(),ErrorCode]
{-# INLINE toErrorCodeVoid #-}
toErrorCodeVoid r
   | r < 0     = toVariantAt @1 (toCEnum (abs r))
   | otherwise = toVariantAt @0 ()

-- | Convert negative values into error codes, return `f r` otherwise
toErrorCodePure :: (Int64 -> a) -> Int64 -> V '[a,ErrorCode]
{-# INLINE toErrorCodePure #-}
toErrorCodePure f r
   | r < 0     = toVariantAt @1 (toCEnum (abs r))
   | otherwise = toVariantAt @0 (f r)

-- | Error to call when a syscall returns an unexpected error value
unhdlErr :: Show err => String -> err -> a
unhdlErr str err =
   error ("Unhandled error "++ show err ++" returned by \""++str++"\". Report this as a Haskus bug.")
