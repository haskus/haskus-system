{-# LANGUAGE DeriveAnyClass #-}

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
import Data.Word

import ViperVM.Format.Binary.BitSet
import qualified ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Syscalls

data MountFlag
   = MountReadOnly               -- ^ Mount read-only
   | MountNoSuid                 -- ^ Ignore suid and sgid bits
   | MountNoDevice               -- ^ Disallow access to device special files
   | MountNoExec                 -- ^ Disallow program execution
   | MountSynchronous            -- ^ Writes are synced at once
   | MountRemount                -- ^ Alter flags of a mounted FS
   | MountMandatoryLock          -- ^ Allow mandatory locks on an FS
   | MountSynchronousDirectory   -- ^ Directory modifications are synchronous
   | MountNoAccessTime           -- ^ Do not update access times
   | MountNoDirectoryAccessTime  -- ^ Do not update directory access times
   | MountBind                   -- ^ Bind directory at different place
   | MountMove                   -- ^ Move a subtree (without unmounting)
   | MountRecursive              -- ^ Recursive (loop-back) mount
   | MountSilent                 -- ^ Disable some warnings in the kernel log
   | MountPosixACL               -- ^ VFS does not apply the umask
   | MountUnbindable             -- ^ Change to unbindable
   | MountPrivate                -- ^ Change to private
   | MountSlave                  -- ^ Change to slave
   | MountShared                 -- ^ Change to shared
   | MountRelativeAccessTime     -- ^ Update atime relative to mtime/ctime
   | MountKernelMount            -- ^ This is a kern_mount call
   | MountUpdateInodeVersion     -- ^ Update inode I_version field
   | MountStrictAccessTime       -- ^ Always perform atime updates
   | MountActive
   | MountNoUser
   deriving (Show,Eq,EnumBitSet)

instance Enum MountFlag where
   fromEnum x = case x of
      MountReadOnly               -> 0
      MountNoSuid                 -> 1
      MountNoDevice               -> 2
      MountNoExec                 -> 3
      MountSynchronous            -> 4
      MountRemount                -> 5
      MountMandatoryLock          -> 6
      MountSynchronousDirectory   -> 7
      MountNoAccessTime           -> 10
      MountNoDirectoryAccessTime  -> 11
      MountBind                   -> 12
      MountMove                   -> 13
      MountRecursive              -> 14
      MountSilent                 -> 15
      MountPosixACL               -> 16
      MountUnbindable             -> 17
      MountPrivate                -> 18
      MountSlave                  -> 19
      MountShared                 -> 20
      MountRelativeAccessTime     -> 21
      MountKernelMount            -> 22
      MountUpdateInodeVersion     -> 23
      MountStrictAccessTime       -> 24
      MountActive                 -> 30
      MountNoUser                 -> 31

   toEnum x = case x of
      0  -> MountReadOnly
      1  -> MountNoSuid
      2  -> MountNoDevice
      3  -> MountNoExec
      4  -> MountSynchronous
      5  -> MountRemount
      6  -> MountMandatoryLock
      7  -> MountSynchronousDirectory
      10 -> MountNoAccessTime
      11 -> MountNoDirectoryAccessTime
      12 -> MountBind
      13 -> MountMove
      14 -> MountRecursive
      15 -> MountSilent
      16 -> MountPosixACL
      17 -> MountUnbindable
      18 -> MountPrivate
      19 -> MountSlave
      20 -> MountShared
      21 -> MountRelativeAccessTime
      22 -> MountKernelMount
      23 -> MountUpdateInodeVersion
      24 -> MountStrictAccessTime
      30 -> MountActive
      31 -> MountNoUser
      _  -> error "Unknown mount flag"

type MountFlags = BitSet Word64 MountFlag

data UnmountFlag
   = UnmountForce       -- ^ Force unmounting
   | UnmountDetach      -- ^ Just detach from the tree
   | UnmountExpire      -- ^ Mark for expiry
   | UnmountDontFollow  -- ^ Don't follow symlink on unmount
   deriving (Show,Eq,Enum,EnumBitSet)

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
