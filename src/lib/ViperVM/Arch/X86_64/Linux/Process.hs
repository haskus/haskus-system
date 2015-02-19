module ViperVM.Arch.X86_64.Linux.Process
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
import Data.Word (Word, Word32)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Control.Applicative ((<$>), (<*>))

import ViperVM.Arch.X86_64.Linux.Syscall
import ViperVM.Arch.Linux.ErrorCode

newtype ProcessID = ProcessID Word32 deriving (Show,Eq,Ord)
newtype ThreadID = ThreadID Word32 deriving (Show,Eq,Ord)
newtype UserID = UserID Word32 deriving (Show,Eq,Ord)
newtype GroupID = GroupID Word32 deriving (Show,Eq,Ord)

-- | Exit the current process with the given return value
-- This syscall does not return.
sysExit :: Int64 -> IO ()
sysExit n = void (syscall1 60 n)

-- | Get CPU and NUMA node executing the current process
sysGetCPU :: SysRet (Word,Word)
sysGetCPU =
   alloca $ \cpu ->
      alloca $ \node ->
         onSuccessIO (syscall3 309 (cpu :: Ptr Word) (node :: Ptr Word) nullPtr)
            (const ((,) <$> peek cpu <*> peek node))

-- | Return process ID
sysGetProcessID :: IO ProcessID
sysGetProcessID = ProcessID . fromIntegral <$> syscall0 39

-- | Return thread ID
sysGetThreadID :: IO ThreadID
sysGetThreadID = ThreadID . fromIntegral <$> syscall0 186

-- | Return parent process ID
sysGetParentProcessID :: IO ProcessID
sysGetParentProcessID = ProcessID . fromIntegral <$> syscall0 110

-- | Get real user ID of the calling process
sysGetRealUserID :: IO UserID
sysGetRealUserID = UserID . fromIntegral <$> syscall0 102

-- | Get effective user ID of the calling process
sysGetEffectiveUserID :: IO UserID
sysGetEffectiveUserID = UserID . fromIntegral <$> syscall0 107

-- | Set effective user ID of the calling process
sysSetEffectiveUserID :: UserID -> SysRet ()
sysSetEffectiveUserID (UserID uid) =
   onSuccess (syscall1 105 uid) (const ())

-- | Get real group ID of the calling process
sysGetRealGroupID :: IO GroupID
sysGetRealGroupID = GroupID . fromIntegral <$> syscall0 104

-- | Get effective group ID of the calling process
sysGetEffectiveGroupID :: IO GroupID
sysGetEffectiveGroupID = GroupID . fromIntegral <$> syscall0 108

-- | Set effective group ID of the calling process
sysSetEffectiveGroupID :: GroupID -> SysRet ()
sysSetEffectiveGroupID (GroupID gid) =
   onSuccess (syscall1 106 gid) (const ())

-- | Create a child process
sysFork :: SysRet ProcessID
sysFork = onSuccess (syscall0 57) (ProcessID . fromIntegral)

-- | Create a child process and block parent
sysVFork :: SysRet ProcessID
sysVFork = onSuccess (syscall0 58) (ProcessID . fromIntegral)

-- | Yield the processor
sysSchedulerYield :: SysRet ()
sysSchedulerYield = onSuccess (syscall0 24) (const ())

