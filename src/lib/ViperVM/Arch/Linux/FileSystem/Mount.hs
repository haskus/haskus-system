{-# LANGUAGE DeriveAnyClass #-}

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

import Foreign.C.String (withCString)
import Foreign.Ptr (Ptr,nullPtr)

import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.BitSet
import qualified ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Arch.Linux.Internals.FileSystem

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
sysMount :: String -> String -> String -> MountFlags -> Ptr () -> SysRet ()
sysMount source target fstype flags dat =
   withCString source $ \source' ->
      withCString target $ \target' ->
         withCString fstype $ \fstype' ->
            onSuccess (syscall_mount source' target' fstype' (BitSet.toBits flags) dat) (const ())


-- | Unmount a file system
sysUnmount :: String -> UnmountFlags -> SysRet ()
sysUnmount target flags =
   withCString target $ \target' ->
      onSuccess (syscall_umount2 target' (BitSet.toBits flags)) (const ())

-- | Type of the low-level Linux "mount" function
type MountCall = String -> String -> String -> MountFlags -> Ptr () -> SysRet ()

-- | Mount SysFS at the given location
mountSysFS :: MountCall -> FilePath -> SysRet ()
mountSysFS mount path = mount "none" path "sysfs" BitSet.empty nullPtr

-- | Mount DevFS at the given location
mountDevFS :: MountCall -> FilePath -> SysRet ()
mountDevFS mount path = mount "none" path "devtmpfs" BitSet.empty nullPtr

-- | Mount ProcFS at the given location
mountProcFS :: MountCall -> FilePath -> SysRet ()
mountProcFS mount path = mount "none" path "proc" BitSet.empty nullPtr

-- | Mount TmpFS at the given location
mountTmpFS :: MountCall -> FilePath -> SysRet ()
mountTmpFS mount path = mount "none" path "tmpfs" BitSet.empty nullPtr
