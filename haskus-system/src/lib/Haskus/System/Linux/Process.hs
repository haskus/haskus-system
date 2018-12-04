{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Process management
module Haskus.System.Linux.Process
   ( ProcessID(..)
   , ThreadID(..)
   , UserID(..)
   , GroupID(..)
   , SessionID(..)
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

import Haskus.Format.Binary.Ptr (Ptr, nullPtr)
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Storable
import Haskus.System.Linux.Syscalls
import Haskus.System.Linux.ErrorCode
import Haskus.Utils.Flow

-- | Process ID
newtype ProcessID = ProcessID Word32 deriving (Show,Eq,Ord,Storable)

-- | Thread ID
newtype ThreadID = ThreadID Word32 deriving (Show,Eq,Ord,Storable)

-- | User ID
newtype UserID = UserID Word32 deriving (Show,Eq,Ord,Storable)

-- | Group ID
newtype GroupID = GroupID Word32 deriving (Show,Eq,Ord,Storable)

-- | Session ID
newtype SessionID = SessionID Word32 deriving (Show,Eq,Ord,Storable)

-- | Exit the current process with the given return value
-- This syscall does not return.
sysExit :: Int64 -> IO ()
sysExit n = void (syscall_exit n)

-- | Get CPU and NUMA node executing the current process
sysGetCPU :: MonadInIO m => FlowT '[ErrorCode] m (Word,Word)
sysGetCPU =
   alloca $ \cpu ->
      alloca $ \node -> do
         r <- liftIO (syscall_getcpu (cpu :: Ptr Word) (node :: Ptr Word) nullPtr)
         checkErrorCode_ r
         (,) <$> peek cpu <*> peek node

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
sysSetEffectiveUserID :: MonadIO m => UserID -> FlowT '[ErrorCode] m ()
sysSetEffectiveUserID (UserID uid) = checkErrorCode_ =<< liftIO (syscall_setuid uid)

-- | Get real group ID of the calling process
sysGetRealGroupID :: IO GroupID
sysGetRealGroupID = GroupID . fromIntegral <$> syscall_getgid

-- | Get effective group ID of the calling process
sysGetEffectiveGroupID :: IO GroupID
sysGetEffectiveGroupID = GroupID . fromIntegral <$> syscall_getegid

-- | Set effective group ID of the calling process
sysSetEffectiveGroupID :: MonadIO m => GroupID -> FlowT '[ErrorCode] m ()
sysSetEffectiveGroupID (GroupID gid) = checkErrorCode_ =<< liftIO (syscall_setgid gid)

-- | Create a child process
sysFork :: MonadIO m => FlowT '[ErrorCode] m ProcessID
sysFork = do
   v <- checkErrorCode =<< liftIO (syscall_fork)
   return (ProcessID (fromIntegral v))

-- | Create a child process and block parent
sysVFork :: MonadIO m => FlowT '[ErrorCode] m ProcessID
sysVFork = do
   v <- checkErrorCode =<< liftIO (syscall_vfork)
   return (ProcessID (fromIntegral v))

-- | Yield the processor
sysSchedulerYield :: MonadIO m => FlowT '[ErrorCode] m ()
sysSchedulerYield = checkErrorCode_ =<< liftIO (syscall_sched_yield)

