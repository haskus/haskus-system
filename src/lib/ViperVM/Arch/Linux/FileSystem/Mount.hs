{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Filesystem mount
module ViperVM.Arch.Linux.FileSystem.Mount
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

import ViperVM.Format.Binary.Ptr (Ptr,nullPtr)
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.BitSet
import ViperVM.Format.String (withCString)
import qualified ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Arch.Linux.Internals.FileSystem
import ViperVM.Utils.Flow

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
sysMount :: String -> String -> String -> MountFlags -> Ptr () -> IOErr ()
sysMount source target fstype flags dat =
   withCString source $ \source' ->
      withCString target $ \target' ->
         withCString fstype $ \fstype' ->
            syscall @"mount" source' target' fstype' (BitSet.toBits flags) dat
               ||> toErrorCodeVoid


-- | Unmount a file system
sysUnmount :: String -> UnmountFlags -> IOErr ()
sysUnmount target flags =
   withCString target $ \target' ->
      syscall @"umount2" target' (BitSet.toBits flags)
         ||> toErrorCodeVoid

-- | Type of the low-level Linux "mount" function
type MountCall = String -> String -> String -> MountFlags -> Ptr () -> IOErr ()

-- | Mount SysFS at the given location
mountSysFS :: MountCall -> FilePath -> IOErr ()
mountSysFS mount path = mount "none" path "sysfs" BitSet.empty nullPtr

-- | Mount DevFS at the given location
mountDevFS :: MountCall -> FilePath -> IOErr ()
mountDevFS mount path = mount "none" path "devtmpfs" BitSet.empty nullPtr

-- | Mount ProcFS at the given location
mountProcFS :: MountCall -> FilePath -> IOErr ()
mountProcFS mount path = mount "none" path "proc" BitSet.empty nullPtr

-- | Mount TmpFS at the given location
mountTmpFS :: MountCall -> FilePath -> IOErr ()
mountTmpFS mount path = mount "none" path "tmpfs" BitSet.empty nullPtr
