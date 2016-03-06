{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ViperVM.Arch.Linux.Process
   ( ProcessID(..)
   , ThreadID(..)
   , UserID(..)
   , GroupID(..)
   , sysExit
   , sysGetCPU
   , sysGetProcessID
   , sysGetParentProcessID
   , sysGetRealUserID
   , sysGetEffectiveUserID
   , sysSetEffectiveUserID
   , sysGetRealGroupID
   , sysGetEffectiveGroupID
   , sysSetEffectiveGroupID
   , sysGetThreadID
   , sysFork
   , sysVFork
   , sysSchedulerYield
   )
where

import Control.Monad (void)
import Data.Int (Int64)
import Data.Word (Word32)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable
import Foreign.CStorable

import ViperVM.Arch.Linux.Syscalls
import ViperVM.Arch.Linux.ErrorCode

newtype ProcessID = ProcessID Word32 deriving (Show,Eq,Ord,Storable,CStorable)
newtype ThreadID = ThreadID Word32 deriving (Show,Eq,Ord,Storable,CStorable)
newtype UserID = UserID Word32 deriving (Show,Eq,Ord,Storable,CStorable)
newtype GroupID = GroupID Word32 deriving (Show,Eq,Ord,Storable,CStorable)

-- | Exit the current process with the given return value
-- This syscall does not return.
sysExit :: Int64 -> IO ()
sysExit n = void (syscall_exit n)

-- | Get CPU and NUMA node executing the current process
sysGetCPU :: SysRet (Word,Word)
sysGetCPU =
   alloca $ \cpu ->
      alloca $ \node ->
         onSuccessIO (syscall_getcpu (cpu :: Ptr Word) (node :: Ptr Word) nullPtr)
            (const ((,) <$> peek cpu <*> peek node))

-- | Return process ID
sysGetProcessID :: IO ProcessID
sysGetProcessID = ProcessID . fromIntegral <$> syscall_getpid

-- | Return thread ID
sysGetThreadID :: IO ThreadID
sysGetThreadID = ThreadID . fromIntegral <$> syscall_gettid

-- | Return parent process ID
sysGetParentProcessID :: IO ProcessID
sysGetParentProcessID = ProcessID . fromIntegral <$> syscall_getppid

-- | Get real user ID of the calling process
sysGetRealUserID :: IO UserID
sysGetRealUserID = UserID . fromIntegral <$> syscall_getuid

-- | Get effective user ID of the calling process
sysGetEffectiveUserID :: IO UserID
sysGetEffectiveUserID = UserID . fromIntegral <$> syscall_geteuid

-- | Set effective user ID of the calling process
sysSetEffectiveUserID :: UserID -> SysRet ()
sysSetEffectiveUserID (UserID uid) =
   onSuccess (syscall_setuid uid) (const ())

-- | Get real group ID of the calling process
sysGetRealGroupID :: IO GroupID
sysGetRealGroupID = GroupID . fromIntegral <$> syscall_getgid

-- | Get effective group ID of the calling process
sysGetEffectiveGroupID :: IO GroupID
sysGetEffectiveGroupID = GroupID . fromIntegral <$> syscall_getegid

-- | Set effective group ID of the calling process
sysSetEffectiveGroupID :: GroupID -> SysRet ()
sysSetEffectiveGroupID (GroupID gid) =
   onSuccess (syscall_setgid gid) (const ())

-- | Create a child process
sysFork :: SysRet ProcessID
sysFork = onSuccess syscall_fork (ProcessID . fromIntegral)

-- | Create a child process and block parent
sysVFork :: SysRet ProcessID
sysVFork = onSuccess syscall_vfork (ProcessID . fromIntegral)

-- | Yield the processor
sysSchedulerYield :: SysRet ()
sysSchedulerYield = onSuccess syscall_sched_yield (const ())

