{-# LANGUAGE LambdaCase #-}

module ViperVM.Arch.Linux.Error
   ( sysOnSuccess
   , sysOnSuccessVoid
   , sysCallAssert
   , sysCallAssert'
   , sysCallAssertQuiet
   , sysCallWarn
   , sysLogPrint
   -- * Errors
   , NotAllowed (..)
   , InvalidRestartCommand (..)
   , MemoryError (..)
   )
where

import Prelude hiding (log)
import Text.Printf
import Data.Int

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.System.Sys

------------------------------------------------
-- Errors
------------------------------------------------
data NotAllowed            = NotAllowed
data InvalidRestartCommand = InvalidRestartCommand
data MemoryError           = MemoryError

------------------------------------------------
-- System calls
------------------------------------------------

sysOnSuccess :: IO Int64 -> (Int64 -> a) -> Sys (Either ErrorCode a)
sysOnSuccess a f = sysIO (onSuccess a f)

sysOnSuccessVoid :: IO Int64 -> Sys (Either ErrorCode ())
sysOnSuccessVoid a = sysIO (onSuccessVoid a)

-- | Assert that the given action doesn't fail
sysCallAssert :: String -> SysRet a -> Sys a
sysCallAssert text act = do
   r <- sysIO act
   sysCallAssert' text r

-- | Assert that the given action doesn't fail
sysCallAssert' :: String -> Either ErrorCode a -> Sys a
sysCallAssert' text r =
   case r of
      Left err -> do
         let msg = printf "%s (failed with %s)" text (show err)
         sysError msg
      Right v  -> do
         let msg = printf "%s (success)" text
         sysLog LogInfo msg
         return v

-- | Assert that the given action doesn't fail, log only on error
sysCallAssertQuiet :: String -> SysRet a -> Sys a
sysCallAssertQuiet text act = do
   r <- sysIO act
   case r of
      Left err -> do
         let msg = printf "%s (failed with %s)" text (show err)
         sysError msg
      Right v  -> return v

-- | Log a warning if the given action fails
sysCallWarn :: String -> SysRet a -> Sys (Either ErrorCode a)
sysCallWarn text act = do
   r <- sysIO act
   case r of
      Left err -> do
         let msg = printf "%s (failed with %s)" text (show err)
         sysLog LogWarning msg
      Right _  -> do
         let msg = printf "%s (success)" text
         sysLog LogInfo msg
   return r
