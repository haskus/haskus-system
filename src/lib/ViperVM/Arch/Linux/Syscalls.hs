{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

-- | Linux syscalls
module ViperVM.Arch.Linux.Syscalls
   ( syscall
   , sysFlow
   , sysOnSuccess
   , sysOnSuccessVoid
   , sysCallAssert
   , sysCallAssert'
   , sysCallAssertQuiet
   , sysCallWarn
   , sysCallWarnQuiet
   , sysLogPrint
   )
where

--TODO: use conditional import here when we will support different
--architectures
import ViperVM.Arch.X86_64.Linux.Syscalls

import Prelude hiding (log)
import Text.Printf

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.System.Sys
import ViperVM.Utils.Flow
import ViperVM.Utils.Variant
import ViperVM.Format.Binary.Word


-- | Convert a syscall into a flow
sysFlow :: IO Int64 -> Flow Sys '[Int64,ErrorCode]
sysFlow f = sysIO (f ||> toErrorCode)

-- | Convert a syscall result into a flow
sysOnSuccess :: IO Int64 -> (Int64 -> a) -> Flow Sys '[a,ErrorCode]
sysOnSuccess a f = sysFlow a >.-.> f

-- | Convert a syscall result into a void flow
sysOnSuccessVoid :: IO Int64 -> Flow Sys '[(),ErrorCode]
sysOnSuccessVoid a = sysFlow a >.-.> const ()

-- | Assert that the given action doesn't fail
sysCallAssert :: String -> IOErr a -> Sys a
sysCallAssert text act = do
   r <- sysIO act
   sysCallAssert' text r

-- | Assert that the given action doesn't fail
sysCallAssert' :: String -> Variant '[a,ErrorCode] -> Sys a
sysCallAssert' text r =
   case toEither r of
      Left err -> do
         let msg = printf "%s (failed with %s)" text (show err)
         sysError msg
      Right v  -> do
         let msg = printf "%s (success)" text
         sysLog LogInfo msg
         return v

-- | Assert that the given action doesn't fail, log only on error
sysCallAssertQuiet :: String -> IOErr a -> Sys a
sysCallAssertQuiet text act = do
   r <- sysIO act
   case toEither r of
      Left err -> do
         let msg = printf "%s (failed with %s)" text (show err)
         sysError msg
      Right v  -> return v

-- | Log a warning if the given action fails, otherwise log success
sysCallWarn :: String -> IOErr a -> Flow Sys '[a,ErrorCode]
sysCallWarn text act = do
   r <- sysIO act
   case toEither r of
      Right _ -> do
         let msg = printf "%s (success)" text
         sysLog LogInfo msg
      Left err -> do
         let msg = printf "%s (failed with %s)" text (show err)
         sysLog LogWarning msg
   return r

-- | Log a warning only if the given action fails
sysCallWarnQuiet :: String -> IOErr a -> Flow Sys '[a,ErrorCode]
sysCallWarnQuiet text act = do
   r <- sysIO act
   case toEither r of
      Right v -> flowSetN @0 v
      Left err -> do
         let msg = printf "%s (failed with %s)" text (show err)
         sysLog LogWarning msg
         flowSetN @1 err
