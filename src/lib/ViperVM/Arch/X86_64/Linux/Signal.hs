{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module ViperVM.Arch.X86_64.Linux.Signal
   ( SignalSet(..)
   , sysPause
   , sysAlarm
   , sysSendSignal
   , sysSendSignalGroup
   , sysSendSignalAll
   , sysCheckProcess
   , sysChangeBlockedSignals
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.X86_64.Linux.Syscall
import ViperVM.Arch.X86_64.Linux.Process

import Data.Int
import Data.Word
import Foreign.Storable
import Foreign.Ptr (Ptr,nullPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Alloc (alloca)
import Data.Vector.Fixed.Cont (S,Z)
import Data.Vector.Fixed.Storable (Vec)

type N16 = --16
   S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z
   )))))))))))))))

newtype SignalSet = SignalSet (Vec N16 Word64) deriving (Storable)

sysPause :: SysRet ()
sysPause = onSuccess (syscall0 34) (const ())

sysAlarm :: Word-> SysRet Word
sysAlarm seconds =
   onSuccess (syscall1 37 seconds) fromIntegral

-- | Kill syscall
sysSendSignal :: ProcessID -> Int -> SysRet ()
sysSendSignal (ProcessID pid) sig =
   onSuccess (syscall2 62 pid sig) (const ())

-- | Send a signal to every process in the process group of the calling process
sysSendSignalGroup :: Int -> SysRet ()
sysSendSignalGroup sig =
   onSuccess (syscall2 62 (0 :: Int64) sig) (const ())

-- | Send a signal to every process for which the calling process has permission to send signals, except for process 1 (init)
sysSendSignalAll :: Int -> SysRet ()
sysSendSignalAll sig =
   onSuccess (syscall2 62 (-1 :: Int64) sig) (const ())

-- | Check if a given process or process group exists
--
-- Send signal "0" the given process
sysCheckProcess :: ProcessID -> SysRet Bool
sysCheckProcess pid = do
   err <- sysSendSignal pid 0
   return $ case err of
      Right _ -> Right True
      Left ESRCH -> Right False
      Left e -> Left e

data ChangeSignals
   = BlockSignals    -- ^ Block signals in the set
   | UnblockSignals  -- ^ Unblock signals in the set
   | SetSignals      -- ^ Set blocked signals to the set
   deriving (Show,Eq,Enum)

sysChangeBlockedSignals :: ChangeSignals -> Maybe SignalSet -> SysRet SignalSet
sysChangeBlockedSignals act set =
   let f x = alloca $ \(ret :: Ptr SignalSet) ->
               onSuccessIO (syscall3 14 (fromEnum act) x ret) (const $ peek ret)
   in
   case set of
      Just s -> with s f
      Nothing -> f nullPtr
