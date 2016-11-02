{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Process management
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

import ViperVM.Format.Binary.Ptr (Ptr, nullPtr)
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Storable
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Utils.Flow

-- | Process ID
newtype ProcessID = ProcessID Word32 deriving (Show,Eq,Ord,Storable)

-- | Thread ID
newtype ThreadID = ThreadID Word32 deriving (Show,Eq,Ord,Storable)

-- | User ID
newtype UserID = UserID Word32 deriving (Show,Eq,Ord,Storable)

-- | Group ID
newtype GroupID = GroupID Word32 deriving (Show,Eq,Ord,Storable)

-- | Exit the current process with the given return value
-- This syscall does not return.
sysExit :: Int64 -> IO ()
sysExit n = void (syscall @"exit" n)

-- | Get CPU and NUMA node executing the current process
sysGetCPU :: IOErr (Word,Word)
sysGetCPU =
   alloca $ \cpu ->
      alloca $ \node ->
         syscall @"getcpu" (cpu :: Ptr Word) (node :: Ptr Word) nullPtr
            ||>   toErrorCode
            >.~.> (const ((,) <$> peek cpu <*> peek node))

-- | Return process ID
sysGetProcessID :: IO ProcessID
sysGetProcessID = ProcessID . fromIntegral <$> syscall @"getpid"

-- | Return thread ID
sysGetThreadID :: IO ThreadID
sysGetThreadID = ThreadID . fromIntegral <$> syscall @"gettid"

-- | Return parent process ID
sysGetParentProcessID :: IO ProcessID
sysGetParentProcessID = ProcessID . fromIntegral <$> syscall @"getppid"

-- | Get real user ID of the calling process
sysGetRealUserID :: IO UserID
sysGetRealUserID = UserID . fromIntegral <$> syscall @"getuid"

-- | Get effective user ID of the calling process
sysGetEffectiveUserID :: IO UserID
sysGetEffectiveUserID = UserID . fromIntegral <$> syscall @"geteuid"

-- | Set effective user ID of the calling process
sysSetEffectiveUserID :: UserID -> IOErr ()
sysSetEffectiveUserID (UserID uid) = syscall @"setuid" uid
   ||> toErrorCodeVoid

-- | Get real group ID of the calling process
sysGetRealGroupID :: IO GroupID
sysGetRealGroupID = GroupID . fromIntegral <$> syscall @"getgid"

-- | Get effective group ID of the calling process
sysGetEffectiveGroupID :: IO GroupID
sysGetEffectiveGroupID = GroupID . fromIntegral <$> syscall @"getegid"

-- | Set effective group ID of the calling process
sysSetEffectiveGroupID :: GroupID -> IOErr ()
sysSetEffectiveGroupID (GroupID gid) = syscall @"setgid" gid
   ||> toErrorCodeVoid

-- | Create a child process
sysFork :: IOErr ProcessID
sysFork = syscall @"fork"
   ||> toErrorCodePure (ProcessID . fromIntegral)

-- | Create a child process and block parent
sysVFork :: IOErr ProcessID
sysVFork = syscall @"vfork"
   ||> toErrorCodePure (ProcessID . fromIntegral)

-- | Yield the processor
sysSchedulerYield :: IOErr ()
sysSchedulerYield = syscall @"sched_yield" ||> toErrorCodeVoid

