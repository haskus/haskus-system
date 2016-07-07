{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

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
import ViperVM.Utils.Flow

import Foreign.Storable
import Foreign.Ptr (Ptr,nullPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Alloc (alloca)

-- | Signal set
newtype SignalSet = SignalSet (Vector 16 Word64) deriving (Storable)

-- | Pause
sysPause :: SysRet ()
sysPause = onSuccess syscall_pause (const ())

-- | Alarm
sysAlarm :: Word-> SysRet Word
sysAlarm seconds =
   onSuccess (syscall_alarm seconds) fromIntegral

-- | Kill syscall
sysSendSignal :: ProcessID -> Int -> SysRet ()
sysSendSignal (ProcessID pid) sig =
   onSuccess (syscall_kill (fromIntegral pid) sig) (const ())

-- | Send a signal to every process in the process group of the calling process
sysSendSignalGroup :: Int -> SysRet ()
sysSendSignalGroup sig =
   onSuccess (syscall_kill 0 sig) (const ())

-- | Send a signal to every process for which the calling process has permission to send signals, except for process 1 (init)
sysSendSignalAll :: Int -> SysRet ()
sysSendSignalAll sig =
   onSuccess (syscall_kill (-1) sig) (const ())

-- | Check if a given process or process group exists
--
-- Send signal "0" the given process
sysCheckProcess :: ProcessID -> SysRet Bool
sysCheckProcess pid = sysSendSignal pid 0
   >.-.> const True
   >%~-> \case
      ESRCH -> flowRet False
      e     -> flowRet1 e

-- | Signal actions
data ChangeSignals
   = BlockSignals    -- ^ Block signals in the set
   | UnblockSignals  -- ^ Unblock signals in the set
   | SetSignals      -- ^ Set blocked signals to the set
   deriving (Show,Eq,Enum)

-- | Change signal mask
sysChangeSignalMask :: ChangeSignals -> Maybe SignalSet -> SysRet SignalSet
sysChangeSignalMask act set =
   let f x = alloca $ \(ret :: Ptr SignalSet) ->
               onSuccessIO (syscall_sigprocmask (fromEnum act) x ret) (const $ peek ret)
   in
   case set of
      Just s -> with s f
      Nothing -> f nullPtr
