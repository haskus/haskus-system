{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Linux signals
module ViperVM.Arch.Linux.Signal
   ( SignalSet(..)
   , ChangeSignals(..)
   , sysPause
   , sysAlarm
   , sysSendSignal
   , sysSendSignalGroup
   , sysSendSignalAll
   , sysCheckProcess
   , sysChangeSignalMask
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Arch.Linux.Process
import ViperVM.Format.Binary.Vector (Vector)
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.Storable
import ViperVM.Utils.Flow
import ViperVM.Utils.Memory

-- | Signal set
newtype SignalSet = SignalSet (Vector 16 Word64) deriving (Storable)

-- | Pause
sysPause :: IOErr ()
sysPause = syscall @"pause" ||> toErrorCodeVoid

-- | Alarm
sysAlarm :: Word-> IOErr Word
sysAlarm seconds = syscall @"alarm" seconds
   ||> toErrorCodePure fromIntegral

-- | Kill syscall
sysSendSignal :: ProcessID -> Int -> IOErr ()
sysSendSignal (ProcessID pid) sig =
   syscall @"kill" (fromIntegral pid) sig
      ||> toErrorCodeVoid

-- | Send a signal to every process in the process group of the calling process
sysSendSignalGroup :: Int -> IOErr ()
sysSendSignalGroup sig =
   syscall @"kill" 0 sig
      ||> toErrorCodeVoid

-- | Send a signal to every process for which the calling process has permission to send signals, except for process 1 (init)
sysSendSignalAll :: Int -> IOErr ()
sysSendSignalAll sig =
   syscall @"kill" (-1) sig
      ||> toErrorCodeVoid

-- | Check if a given process or process group exists
--
-- Send signal "0" the given process
sysCheckProcess :: ProcessID -> IOErr Bool
sysCheckProcess pid = sysSendSignal pid 0
   >.-.> const True
   >%~$> \case
      ESRCH -> flowRet0 False
      e     -> flowRet1 e

-- | Signal actions
data ChangeSignals
   = BlockSignals    -- ^ Block signals in the set
   | UnblockSignals  -- ^ Unblock signals in the set
   | SetSignals      -- ^ Set blocked signals to the set
   deriving (Show,Eq,Enum)

-- | Change signal mask
sysChangeSignalMask :: ChangeSignals -> Maybe SignalSet -> IOErr SignalSet
sysChangeSignalMask act set =
   withMaybeOrNull set $ \x ->
      alloca $ \(ret :: Ptr SignalSet) ->
         syscall @"rt_sigprocmask" (fromEnum act) (castPtr x) (castPtr ret)
            ||>   toErrorCode
            >.~.> (const $ peek ret)
