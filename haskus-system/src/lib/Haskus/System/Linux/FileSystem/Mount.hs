{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Filesystem mount
module Haskus.System.Linux.FileSystem.Mount
   ( MountFlag(..)
   , MountFlags
   , UnmountFlag(..)
   , UnmountFlags
   , sysMount
   , sysUnmount
   , mountSysFS
   , mountDevFS
   , mountProcFS
   , mountTmpFS
   )
where

import Foreign.Ptr (Ptr,nullPtr)
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.BitSet
import Haskus.Format.String (withCString)
import qualified Haskus.Format.Binary.BitSet as BitSet
import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Syscalls
import Haskus.System.Linux.Internals.FileSystem
import Haskus.Utils.Flow

-- | Unmount flag
data UnmountFlag
   = UnmountForce       -- ^ Force unmounting
   | UnmountDetach      -- ^ Just detach from the tree
   | UnmountExpire      -- ^ Mark for expiry
   | UnmountDontFollow  -- ^ Don't follow symlink on unmount
   deriving (Show,Eq,Enum,BitOffset)

-- | Unmount flags
type UnmountFlags = BitSet Word64 UnmountFlag

-- | Mount a file system
sysMount :: MonadInIO m => String -> String -> String -> MountFlags -> Ptr () -> Excepts '[ErrorCode] m ()
sysMount source target fstype flags dat =
   withCString source $ \source' ->
      withCString target $ \target' ->
         withCString fstype $ \fstype' ->
            liftIO (syscall_mount source' target' fstype' (BitSet.toBits flags) dat)
               >>= checkErrorCode_


-- | Unmount a file system
sysUnmount :: MonadInIO m => String -> UnmountFlags -> Excepts '[ErrorCode] m ()
sysUnmount target flags =
   withCString target $ \target' ->
      liftIO (syscall_umount2 target' (BitSet.toBits flags))
         >>= checkErrorCode_

-- | Type of the low-level Linux "mount" function
type MountCall m = String -> String -> String -> MountFlags -> Ptr () -> Excepts '[ErrorCode] m ()

-- | Mount SysFS at the given location
mountSysFS :: MonadIO m => MountCall m -> FilePath -> Excepts '[ErrorCode] m ()
mountSysFS mount path = mount "none" path "sysfs" BitSet.empty nullPtr

-- | Mount DevFS at the given location
mountDevFS :: MonadIO m => MountCall m -> FilePath -> Excepts '[ErrorCode] m ()
mountDevFS mount path = mount "none" path "devtmpfs" BitSet.empty nullPtr

-- | Mount ProcFS at the given location
mountProcFS :: MonadIO m => MountCall m -> FilePath -> Excepts '[ErrorCode] m ()
mountProcFS mount path = mount "none" path "proc" BitSet.empty nullPtr

-- | Mount TmpFS at the given location
mountTmpFS :: MonadIO m => MountCall m -> FilePath -> Excepts '[ErrorCode] m ()
mountTmpFS mount path = mount "none" path "tmpfs" BitSet.empty nullPtr
