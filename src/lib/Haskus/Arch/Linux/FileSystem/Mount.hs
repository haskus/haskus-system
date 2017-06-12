{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Filesystem mount
module Haskus.Arch.Linux.FileSystem.Mount
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

import Haskus.Format.Binary.Ptr (Ptr,nullPtr)
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.BitSet
import Haskus.Format.String (withCString)
import qualified Haskus.Format.Binary.BitSet as BitSet
import Haskus.Arch.Linux.ErrorCode
import Haskus.Arch.Linux.Syscalls
import Haskus.Arch.Linux.Internals.FileSystem
import Haskus.Utils.Flow

-- | Unmount flag
data UnmountFlag
   = UnmountForce       -- ^ Force unmounting
   | UnmountDetach      -- ^ Just detach from the tree
   | UnmountExpire      -- ^ Mark for expiry
   | UnmountDontFollow  -- ^ Don't follow symlink on unmount
   deriving (Show,Eq,Enum,CBitSet)

-- | Unmount flags
type UnmountFlags = BitSet Word64 UnmountFlag

-- | Mount a file system
sysMount :: MonadInIO m => String -> String -> String -> MountFlags -> Ptr () -> Flow m '[(),ErrorCode]
sysMount source target fstype flags dat =
   withCString source $ \source' ->
      withCString target $ \target' ->
         withCString fstype $ \fstype' ->
            liftIO (syscall_mount source' target' fstype' (BitSet.toBits flags) dat)
               ||> toErrorCodeVoid


-- | Unmount a file system
sysUnmount :: MonadInIO m => String -> UnmountFlags -> Flow m '[(),ErrorCode]
sysUnmount target flags =
   withCString target $ \target' ->
      liftIO (syscall_umount2 target' (BitSet.toBits flags))
         ||> toErrorCodeVoid

-- | Type of the low-level Linux "mount" function
type MountCall m = String -> String -> String -> MountFlags -> Ptr () -> Flow m '[(),ErrorCode]

-- | Mount SysFS at the given location
mountSysFS :: MonadIO m => MountCall m -> FilePath -> Flow m '[(),ErrorCode]
mountSysFS mount path = mount "none" path "sysfs" BitSet.empty nullPtr

-- | Mount DevFS at the given location
mountDevFS :: MonadIO m => MountCall m -> FilePath -> Flow m '[(),ErrorCode]
mountDevFS mount path = mount "none" path "devtmpfs" BitSet.empty nullPtr

-- | Mount ProcFS at the given location
mountProcFS :: MonadIO m => MountCall m -> FilePath -> Flow m '[(),ErrorCode]
mountProcFS mount path = mount "none" path "proc" BitSet.empty nullPtr

-- | Mount TmpFS at the given location
mountTmpFS :: MonadIO m => MountCall m -> FilePath -> Flow m '[(),ErrorCode]
mountTmpFS mount path = mount "none" path "tmpfs" BitSet.empty nullPtr
