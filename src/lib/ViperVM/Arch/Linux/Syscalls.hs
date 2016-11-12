{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Linux syscalls
module ViperVM.Arch.Linux.Syscalls
   ( syscall
   , sysFlow
   , sysOnSuccess
   , sysOnSuccessVoid
   , sysLogPrint
   , flowAssertQuiet
   , flowAssert
   , assertShow
   , warningShow
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
sysFlow f = liftIO (f ||> toErrorCode)

-- | Convert a syscall result into a flow
sysOnSuccess :: IO Int64 -> (Int64 -> a) -> Flow Sys '[a,ErrorCode]
sysOnSuccess a f = sysFlow a >.-.> f

-- | Convert a syscall result into a void flow
sysOnSuccessVoid :: IO Int64 -> Flow Sys '[(),ErrorCode]
sysOnSuccessVoid a = sysFlow a >.-.> const ()

-- | Assert a successful result, and log the error otherwise
flowAssertQuiet :: (Show (Variant xs)) => String -> Flow Sys (a ': xs) -> Sys a
flowAssertQuiet text v = 
   v >..~!!> (\a -> sysError (printf "%s (failed with %s)" text (show a)))

-- | Assert a successful result, log on error and on success
flowAssert :: (Show a, Show (Variant xs)) => String -> Flow Sys (a ': xs) -> Sys a
flowAssert text v = 
   v  >.~=>   (\a -> sysLog LogInfo (printf "%s (succeeded with %s)" text (show a)))
      >..~!!> (\xs -> sysError (printf "%s (failed with %s)" text (show xs)))
     
assertShow :: Show a => String -> a -> Sys ()
assertShow text v = do
   let msg = printf "%s (failed with %s)" text (show v)
   sysError msg

warningShow :: Show (Variant xs) => String -> Flow Sys (a ': xs) -> Sys ()
warningShow text f = do
   f >..~!> (\v ->
      sysWarning (printf "%s (failed with %s)" text (show v)))
