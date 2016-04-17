{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module ViperVM.Arch.Linux.Error
   ( sysFlow
   , sysOnSuccess
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
   , InvalidParam (..)
   , EntryNotFound (..)
   , InvalidRange (..)
   )
where

import Prelude hiding (log)
import Text.Printf
import Data.Int

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.System.Sys
import ViperVM.Utils.Flow
import ViperVM.Utils.Variant

------------------------------------------------
-- Errors
------------------------------------------------
data NotAllowed            = NotAllowed            deriving (Show,Eq)
data InvalidRestartCommand = InvalidRestartCommand deriving (Show,Eq)
data MemoryError           = MemoryError           deriving (Show,Eq)
data InvalidParam          = InvalidParam          deriving (Show,Eq)
data EntryNotFound         = EntryNotFound         deriving (Show,Eq)
data InvalidRange          = InvalidRange          deriving (Show,Eq)


------------------------------------------------
-- System calls
------------------------------------------------

sysFlow :: IO Int64 -> Flow Sys '[Int64,ErrorCode]
sysFlow f = sysIO (onSuccess f id)

sysOnSuccess :: IO Int64 -> (Int64 -> a) -> Flow Sys '[a,ErrorCode]
sysOnSuccess a f = sysIO (onSuccess a f)

sysOnSuccessVoid :: IO Int64 -> Flow Sys '[(),ErrorCode]
sysOnSuccessVoid a = sysIO (onSuccessVoid a)

-- | Assert that the given action doesn't fail
sysCallAssert :: String -> SysRet a -> Sys a
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
sysCallAssertQuiet :: String -> SysRet a -> Sys a
sysCallAssertQuiet text act = do
   r <- sysIO act
   case toEither r of
      Left err -> do
         let msg = printf "%s (failed with %s)" text (show err)
         sysError msg
      Right v  -> return v

-- | Log a warning if the given action fails
sysCallWarn :: String -> SysRet a -> Flow Sys '[a,ErrorCode]
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
