{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Linux signals
module Haskus.System.Linux.Signal
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

import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Syscalls
import Haskus.System.Linux.Process
import Haskus.Format.Binary.Vector (Vector)
import Haskus.Format.Binary.Word
import Foreign.Ptr
import Haskus.Format.Binary.Storable
import Haskus.Utils.Flow
import Haskus.Memory.Utils

-- | Signal set
newtype SignalSet = SignalSet (Vector 16 Word64) deriving (Storable)

-- | Pause
sysPause :: MonadIO m => Flow '[ErrorCode] m ()
sysPause = checkErrorCode_ =<< liftIO (syscall_pause)

-- | Alarm
sysAlarm :: MonadIO m => Word-> Flow '[ErrorCode] m Word
sysAlarm seconds = fromIntegral <$> (checkErrorCode =<< liftIO (syscall_alarm seconds))

-- | Kill syscall
sysSendSignal :: MonadIO m => ProcessID -> Int -> Flow '[ErrorCode] m ()
sysSendSignal (ProcessID pid) sig =
   checkErrorCode_ =<< liftIO (syscall_kill (fromIntegral pid) sig)

-- | Send a signal to every process in the process group of the calling process
sysSendSignalGroup :: MonadIO m => Int -> Flow '[ErrorCode] m ()
sysSendSignalGroup sig = checkErrorCode_ =<< liftIO (syscall_kill 0 sig)

-- | Send a signal to every process for which the calling process has permission to send signals, except for process 1 (init)
sysSendSignalAll :: MonadIO m => Int -> Flow '[ErrorCode] m ()
sysSendSignalAll sig = checkErrorCode_ =<< liftIO (syscall_kill (-1) sig)

-- | Check if a given process or process group exists
--
-- Send signal "0" to the given process/process group
sysCheckProcess :: MonadIO m => ProcessID -> Flow '[ErrorCode] m Bool
sysCheckProcess pid = 
   (sysSendSignal pid 0 >> return True)
      -- ESRCH indicates that the process wasn't found
      -- Other errors are left unchanged
      `catchE` (\ESRCH -> success False)

-- | Signal actions
data ChangeSignals
   = BlockSignals    -- ^ Block signals in the set
   | UnblockSignals  -- ^ Unblock signals in the set
   | SetSignals      -- ^ Set blocked signals to the set
   deriving (Show,Eq,Enum)

-- | Change signal mask
sysChangeSignalMask :: MonadInIO m => ChangeSignals -> Maybe SignalSet -> Flow '[ErrorCode] m SignalSet
sysChangeSignalMask act set =
   withMaybeOrNull set $ \x ->
      alloca $ \(ret :: Ptr SignalSet) -> do
         r <- liftIO (syscall_rt_sigprocmask (fromEnum act) (castPtr x) (castPtr ret))
         checkErrorCode_ r
         liftIO (peek ret)
