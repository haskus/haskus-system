{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

-- | Linux FS internals
module ViperVM.Arch.Linux.Internals.FileSystem
   ( SeekWhence (..)
   , RenameFlag (..)
   , FileCloneRange (..)
   , TrimRange (..)
   , FileDedupeRangeInfo (..)
   , FileDedupeRangeHeader (..)
   , FilesStatStruct (..)
   , InodesStat (..)
   , MountFlag (..)
   , MountFlags
   , remountFlagMask
   , FsxAttr (..)
   , XFlag (..)
   , ioctlSetReadOnlyStatus
   , ioctlGetReadOnlyStatus
   , ioctlReReadPartitionTable
   , ioctlFlushBuferCache
   , ioctlSetReadAhead
   , ioctlGetReadAhead
   , ioctlSetMaxSectors
   , ioctlGetMaxSectors
   , ioctlGetLogicalBlockSize
   , ioctlGetSoftBlockSize
   , ioctlSetSoftBlockSize
   , ioctlGetSize
   , ioctlTraceStart
   , ioctlTraceStop
   , ioctlTraceTearDown
   , ioctlDiscard
   , ioctlGetIOMin
   , ioctlGetIOOpt
   , ioctlGetAlignmentOffset
   , ioctlGetPhysicalBlockSize
   , ioctlDiscardZeroes
   , ioctlDiscardSecure
   , ioctlGetRotational
   , ioctlZeroOut
   , ioctlGetDAX
   )
where

import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Format.Binary.Vector as Vector
import ViperVM.Format.Binary.Enum

import Foreign.Storable
import Foreign.CStorable
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Data.Word
import Data.Int
import Control.Monad
import GHC.Generics (Generic)

-- | Helper to convert a Ptr into a Int64
ptrToArg :: Ptr a -> Int64
ptrToArg = fromIntegral . ptrToIntPtr


-- =============================================================
--    From linux/include/uapi/linux/fs.h
-- =============================================================

data SeekWhence
   = SeekSet      -- ^ seek relative to beginning of file 
   | SeekCurrent  -- ^ seek relative to current file position 
   | SeekEnd      -- ^ seek relative to end of file 
   | SeekData     -- ^ seek to the next data 
   | SeekHole     -- ^ seek to the next hole 
   deriving (Show,Eq,Enum,CEnum)

data RenameFlag
   = RenameNoReplace
   | RenameExchange
   | RenameWhiteout
   deriving (Show,Eq,Enum,CBitSet)


-- | struct file_clone_range
data FileCloneRange = FileCloneRange
   { fcrSrcFD      :: Int64
   , fcrSrcOffset  :: Word64
   , fcrSrcLength  :: Word64
   , fcrDestOffset :: Word64
   }
   deriving (Show,Generic,CStorable)


-- | struct fstrim_range
data TrimRange = TrimRange
   { trStart     :: Word64
   , trLength    :: Word64
   , trMinLength :: Word64
   }
   deriving (Show,Generic,CStorable)


-- | extent-same (dedupe) ioctls; these MUST match the btrfs ioctl definitions
data DedupeRangeFlag
   = DedupeRangeSame
   | DedupeRangeDiffers
   deriving (Show,Eq,Enum)

-- | from struct btrfs_ioctl_file_extent_same_info
-- struct file_dedupe_range_info

data FileDedupeRangeInfo = FileDedupeRangeInfo
   { fdriDestFD       :: Int64  -- ^ (in) destination file
   , fdriDestOffset   :: Word64 -- ^ (in) - start of extent in destination
   , fdriBytesDeduped :: Word64 -- ^ (out) - total # of bytes we were able to dedupe from this file.
   , fdriStatus       :: Int32  -- ^ (out) status of this dedupe operation:
                                -- < 0 for error
                                -- == FILE_DEDUPE_RANGE_SAME if dedupe succeeds
                                -- == FILE_DEDUPE_RANGE_DIFFERS if data differs
   , fdriReserved     :: Word32 -- ^ must be zero 
   }
   deriving (Show,Eq,Generic,CStorable)



-- | from struct btrfs_ioctl_file_extent_same_args
-- struct file_dedupe_range
data FileDedupeRangeHeader = FileDedupeRangeHeader
   { fdrSrcOffset :: Word64 -- ^ (in) start of extent in source
   , fdrSrcLengtj :: Word64 -- ^ (in) length of extent
   , fdrDestCount :: Word16 -- ^ (in) total elements in info array
   , fdrReserved1 :: Word16 -- ^ must be zero
   , fdrReserved2 :: Word32 -- ^ must be zero
   }
   deriving (Show,Eq,Generic,CStorable)


-----------------------------------------------------------------------------
-- Dynamically-tunable limits and defaults
-----------------------------------------------------------------------------

-- | struct files_stat_struct
data FilesStatStruct = FilesStatStruct
   { fssNrFiles     :: CULong -- ^ Read-only
   , fssNrFreeFiles :: CULong -- ^ Read-only
   , fssMaxFiles    :: CULong -- ^ Tunable
   }
   deriving (Show,Eq,Generic,CStorable)

-- | struct inodes_stat_t
data InodesStat = InodesStat
   { isNrInodes :: CLong
   , isNrUnused :: CLong
   , isDummy    :: Vector 5 CLong -- padding for sysctl ABI compatibility
   }
   deriving (Show,Generic,CStorable)

-- | These are the fs-independent mount-flags: up to 32 flags are supported
data MountFlag
   = MountReadOnly               -- ^ Mount read-only
   | MountNoSuid                 -- ^ Ignore suid and sgid bits
   | MountNoDevice               -- ^ Disallow access to device special files
   | MountNoExec                 -- ^ Disallow program execution
   | MountSynchronous            -- ^ Writes are synced at once
   | MountRemount                -- ^ Alter flags of a mounted FS
   | MountMandatoryLock          -- ^ Allow mandatory locks on an FS
   | MountSynchronousDirectory   -- ^ Directory modifications are synchronous
   | MountReserved1
   | MountReserved2
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
   | MountLazyTime               -- ^ Update the on-disk [acm]times lazily
   deriving (Show,Eq,Enum,CBitSet)

type MountFlags = BitSet Word64 MountFlag



-- | Superblock flags that can be altered by MS_REMOUNT
remountFlagMask :: MountFlags
remountFlagMask = BitSet.fromList
   [ MountReadOnly
   , MountSynchronous
   , MountMandatoryLock
   , MountUpdateInodeVersion
   , MountLazyTime
   ]


-- | Structure for FS_IOC_FSGETXATTR[A] and FS_IOC_FSSETXATTR
-- struct fsxattr

data FsxAttr = FsxAttr
   { fsxFlags     :: BitSet Word32 XFlag -- ^ xflags field value (get/set)
   , fsxExtSize   :: Word32              -- ^ extsize field value (get/set)
   , fsxNoExtents :: Word32              -- ^ nextents field value (get)
   , fsxProjectID :: Word32              -- ^ project identifier (get/set)
   , fsxPadding   :: Vector 12 Word8
   }
   deriving (Show,Generic,CStorable)


data XFlag
   = XFlagRealTime         -- ^ data in realtime volume 
   | XFlagPrealloc         -- ^ preallocated file extents 
   | XFlagImmutable        -- ^ file cannot be modified 
   | XFlagAppend           -- ^ all writes append 
   | XFlagSync             -- ^ all writes synchronous 
   | XFlagNoAtime          -- ^ do not update access time 
   | XFlagNoDump           -- ^ do not include in backups 
   | XFlagRTInherit        -- ^ create with rt bit set 
   | XFlagProjecInherit    -- ^ create with parents projid 
   | XFlagNoSymLinks       -- ^ disallow symlink creation 
   | XFlagExtSize          -- ^ extent size allocator hint 
   | XFlagExtSizeInherit   -- ^ inherit inode extent size 
   | XFlagNoDefrag         -- ^ do not defragment 
   | XFlagFileStream       -- ^ use filestream allocator 
   | XFlagDAX              -- ^ use DAX for IO 
   | XFlagHasAttr          -- ^ no DIFLAG for this   
   deriving (Show,Eq,Enum)

instance CBitSet XFlag where
   toBitOffset x = case x of
      XFlagRealTime       -> 0
      XFlagPrealloc       -> 1
      XFlagImmutable      -> 3
      XFlagAppend         -> 4
      XFlagSync           -> 5
      XFlagNoAtime        -> 6
      XFlagNoDump         -> 7
      XFlagRTInherit      -> 8
      XFlagProjecInherit  -> 9
      XFlagNoSymLinks     -> 10
      XFlagExtSize        -> 11
      XFlagExtSizeInherit -> 12
      XFlagNoDefrag       -> 13
      XFlagFileStream     -> 14
      XFlagDAX            -> 15
      XFlagHasAttr        -> 31
   fromBitOffset x = case x of
      0   -> XFlagRealTime
      1   -> XFlagPrealloc
      3   -> XFlagImmutable
      4   -> XFlagAppend
      5   -> XFlagSync
      6   -> XFlagNoAtime
      7   -> XFlagNoDump
      8   -> XFlagRTInherit
      9   -> XFlagProjecInherit
      10  -> XFlagNoSymLinks
      11  -> XFlagExtSize
      12  -> XFlagExtSizeInherit
      13  -> XFlagNoDefrag
      14  -> XFlagFileStream
      15  -> XFlagDAX
      31  -> XFlagHasAttr
      _   -> error "Unknown extended flag"



blkIoctl :: Word8 -> Int64 -> Handle -> SysRet Int64
blkIoctl n = ioctlSignalValue sysIoctl 0x12 n defaultCheck

blkIoctlS' :: Word8 -> Int64 -> Handle -> SysRet ()
blkIoctlS' n b fd = do
   r <- ioctlSignalValue sysIoctl 0x12 n defaultCheck b fd
   return $ case r of
      Left err -> Left err
      Right _  -> Right ()

blkIoctlS :: Word8 -> Handle -> SysRet ()
blkIoctlS n = ioctlSignal sysIoctl 0x12 n defaultCheck

blkIoctlR :: Storable a => Word8 -> Handle -> SysRet a
blkIoctlR n = ioctlRead sysIoctl 0x12 n defaultCheck

blkIoctlW :: Storable a => Word8 -> Handle -> a -> SysRet ()
blkIoctlW n = ioctlWrite sysIoctl 0x12 n defaultCheck


onSuccessIO' :: SysRet a -> (a -> IO b) -> SysRet b
onSuccessIO' s f = do
   r <- s
   case r of
      Left err -> return (Left err)
      Right u  -> Right <$> f u

-- | BLKROSET set device read-only (0 = read-write)
ioctlSetReadOnlyStatus :: Bool -> Handle -> SysRet Int64
ioctlSetReadOnlyStatus b = blkIoctl 93 (if b then 1 else 0)

-- | BLKROGET get read-only status (0 = read_write)
ioctlGetReadOnlyStatus :: Handle -> SysRet Bool
ioctlGetReadOnlyStatus fd = fmap (==1) <$> blkIoctl 94 0 fd

-- | BLKRRPART re-read partition table
ioctlReReadPartitionTable :: Handle -> SysRet ()
ioctlReReadPartitionTable = blkIoctlS 95

-- | BLKGETSIZE return device size /512 (long *arg)
-- Use the 64-bit version instead
--ioctlGetDeviceSize32 :: Handle -> SysRet CLong
--ioctlGetDeviceSize32 fd =
--   with 0 $ \(p :: Ptr CLong) ->
--      onSuccessIO (blkIoctl 96 0 fd) (const $ peek p)

-- | BLKFLSBUF  flush buffer cache
ioctlFlushBuferCache :: Handle -> SysRet ()
ioctlFlushBuferCache = blkIoctlS 97

-- | BLKRASET set read ahead for block device
ioctlSetReadAhead :: Int64 -> Handle -> SysRet ()
ioctlSetReadAhead = blkIoctlS' 98

-- | BLKRAGET get current read ahead setting
ioctlGetReadAhead :: Handle -> SysRet CLong
ioctlGetReadAhead fd =
   with 0 $ \p -> do
      r <- blkIoctl 99 (ptrToArg p) fd
      forM r (const $ peek p)

-- Linux aliases BLKRAGET/BLKRASET and BLKFRAGET/BLKFRASET
-- BLKFRASET  _IO(0x12,100)/* set filesystem (mm/filemap.c) read-ahead */
-- BLKFRAGET  _IO(0x12,101)/* get filesystem (mm/filemap.c) read-ahead */


-- | BLKSECTSET set max sectors per request
ioctlSetMaxSectors :: CUShort -> Handle -> SysRet ()
ioctlSetMaxSectors b = blkIoctlS' 102 (fromIntegral b)

-- | BLKSECTGET get max sectors per request
ioctlGetMaxSectors :: Handle -> SysRet CUShort
ioctlGetMaxSectors fd =
   with 0 $ \p ->
      onSuccessIO' (blkIoctl 103 (ptrToArg p) fd) (const $ peek p)

-- | BLKSSZGET get block device logical block size
ioctlGetLogicalBlockSize :: Handle -> SysRet CUInt
ioctlGetLogicalBlockSize fd =
   with 0 $ \p ->
      onSuccessIO' (blkIoctl 104 (ptrToArg p) fd) (const $ peek p)

-- | BLKBSZGET get block device soft block size
ioctlGetSoftBlockSize :: Handle -> SysRet CSize
ioctlGetSoftBlockSize = blkIoctlR 112

-- | BLKBSZSET set block device soft block size
ioctlSetSoftBlockSize :: Handle -> CSize -> SysRet ()
ioctlSetSoftBlockSize = blkIoctlW 113

-- | BLKGETSIZE64 return device size in bytes
ioctlGetSize :: Handle -> SysRet Word64
ioctlGetSize fd =
   -- we have to encode the command with a sizeOf CSize, but the command returns
   -- unconditionally a Word64...
   fmap snd <$> ioctlReadBogus sysIoctl 0x12 114 defaultCheck sz fd
   where
      sz = fromIntegral (sizeOf (undefined :: CSize)) 

-- We haven't defined struct user_trace_setup yet
-- #define BLKTRACESETUP _IOWR(0x12,115,struct blk_user_trace_setup)

-- | BLKTRACESTART
ioctlTraceStart :: Handle -> SysRet ()
ioctlTraceStart = blkIoctlS 116

-- | BLKTRACESTOP
ioctlTraceStop :: Handle -> SysRet ()
ioctlTraceStop = blkIoctlS 117

-- | BLKTRACETEARDOWN
ioctlTraceTearDown :: Handle -> SysRet ()
ioctlTraceTearDown = blkIoctlS 118

ioctlDiscard :: Handle -> SysRet ()
ioctlDiscard = blkIoctlS 119

-- | BLKIOMIN
ioctlGetIOMin :: Handle -> SysRet CUInt
ioctlGetIOMin fd =
   alloca $ \p ->
      onSuccessIO' (blkIoctl 120 (ptrToArg p) fd) (const $ peek p)

-- | BLKIOOPT
ioctlGetIOOpt :: Handle -> SysRet CUInt
ioctlGetIOOpt fd =
   alloca $ \p ->
      onSuccessIO' (blkIoctl 121 (ptrToArg p) fd) (const $ peek p)

-- | BLKALIGNOFF
ioctlGetAlignmentOffset :: Handle -> SysRet Int
ioctlGetAlignmentOffset fd =
   alloca $ \p ->
      onSuccessIO' (blkIoctl 122 (ptrToArg p) fd) (const $ peek p)

-- | BLKPBSZGET get block device physical block size
ioctlGetPhysicalBlockSize :: Handle -> SysRet CUInt
ioctlGetPhysicalBlockSize fd =
   alloca $ \p ->
      onSuccessIO' (blkIoctl 123 (ptrToArg p) fd) (const $ peek p)

-- | BLKDISCARDZEROES
ioctlDiscardZeroes :: Handle -> SysRet CUInt
ioctlDiscardZeroes fd =
   alloca $ \p ->
      onSuccessIO' (blkIoctl 124 (ptrToArg p) fd) (const $ peek p)

-- | BLKSECDISCARD
ioctlDiscardSecure :: Handle -> SysRet ()
ioctlDiscardSecure = blkIoctlS 125

-- | BLKROTATIONAL
ioctlGetRotational :: Handle -> SysRet CUShort
ioctlGetRotational fd =
   alloca $ \p ->
      onSuccessIO' (blkIoctl 126 (ptrToArg p) fd) (const $ peek p)

-- | BLKZEROOUT
ioctlZeroOut :: Handle -> SysRet()
ioctlZeroOut = blkIoctlS 127

-- | BLKDAXGET
ioctlGetDAX :: Handle -> SysRet Int
ioctlGetDAX fd =
   alloca $ \p ->
      onSuccessIO' (blkIoctl 129 (ptrToArg p) fd) (const $ peek p)


-- #define FIBMAP     _IO(0x00,1)  /* bmap access */
-- #define FIGETBSZ   _IO(0x00,2)  /* get the block size used for bmap */
-- #define FIFREEZE        _IOWR('X', 119, int)    /* Freeze */
-- #define FITHAW          _IOWR('X', 120, int)    /* Thaw */
-- #define FITRIM          _IOWR('X', 121, struct fstrim_range)    /* Trim */
-- #define FICLONE         _IOW(0x94, 9, int)
-- #define FICLONERANGE    _IOW(0x94, 13, struct file_clone_range)
-- #define FIDEDUPERANGE   _IOWR(0x94, 54, struct file_dedupe_range)
-- 
-- #define FS_IOC_GETFLAGS                 _IOR('f', 1, long)
-- #define FS_IOC_SETFLAGS                 _IOW('f', 2, long)
-- #define FS_IOC_GETVERSION               _IOR('v', 1, long)
-- #define FS_IOC_SETVERSION               _IOW('v', 2, long)
-- #define FS_IOC_FIEMAP                   _IOWR('f', 11, struct fiemap)
-- #define FS_IOC32_GETFLAGS               _IOR('f', 1, int)
-- #define FS_IOC32_SETFLAGS               _IOW('f', 2, int)
-- #define FS_IOC32_GETVERSION             _IOR('v', 1, int)
-- #define FS_IOC32_SETVERSION             _IOW('v', 2, int)
-- #define FS_IOC_FSGETXATTR               _IOR ('X', 31, struct fsxattr)
-- #define FS_IOC_FSSETXATTR               _IOW ('X', 32, struct fsxattr)
-- 
-- /*
--  * Inode flags (FS_IOC_GETFLAGS / FS_IOC_SETFLAGS)
--  *
--  * Note: for historical reasons, these flags were originally used and
--  * defined for use by ext2/ext3, and then other file systems started
--  * using these flags so they wouldn't need to write their own version
--  * of chattr/lsattr (which was shipped as part of e2fsprogs).  You
--  * should think twice before trying to use these flags in new
--  * contexts, or trying to assign these flags, since they are used both
--  * as the UAPI and the on-disk encoding for ext2/3/4.  Also, we are
--  * almost out of 32-bit flags.  :-)
--  *
--  * We have recently hoisted FS_IOC_FSGETXATTR / FS_IOC_FSSETXATTR from
--  * XFS to the generic FS level interface.  This uses a structure that
--  * has padding and hence has more room to grow, so it may be more
--  * appropriate for many new use cases.
--  *
--  * Please do not change these flags or interfaces before checking with
--  * linux-fsdevel@vger.kernel.org and linux-api@vger.kernel.org.
--  */
-- #define FS_SECRM_FL                     0x00000001 /* Secure deletion */
-- #define FS_UNRM_FL                      0x00000002 /* Undelete */
-- #define FS_COMPR_FL                     0x00000004 /* Compress file */
-- #define FS_SYNC_FL                      0x00000008 /* Synchronous updates */
-- #define FS_IMMUTABLE_FL                 0x00000010 /* Immutable file */
-- #define FS_APPEND_FL                    0x00000020 /* writes to file may only append */
-- #define FS_NODUMP_FL                    0x00000040 /* do not dump file */
-- #define FS_NOATIME_FL                   0x00000080 /* do not update atime */
-- /* Reserved for compression usage... */
-- #define FS_DIRTY_FL                     0x00000100
-- #define FS_COMPRBLK_FL                  0x00000200 /* One or more compressed clusters */
-- #define FS_NOCOMP_FL                    0x00000400 /* Don't compress */
-- /* End compression flags --- maybe not all used */
-- #define FS_ENCRYPT_FL                   0x00000800 /* Encrypted file */
-- #define FS_BTREE_FL                     0x00001000 /* btree format dir */
-- #define FS_INDEX_FL                     0x00001000 /* hash-indexed directory */
-- #define FS_IMAGIC_FL                    0x00002000 /* AFS directory */
-- #define FS_JOURNAL_DATA_FL              0x00004000 /* Reserved for ext3 */
-- #define FS_NOTAIL_FL                    0x00008000 /* file tail should not be merged */
-- #define FS_DIRSYNC_FL                   0x00010000 /* dirsync behaviour (directories only) */
-- #define FS_TOPDIR_FL                    0x00020000 /* Top of directory hierarchies*/
-- #define FS_HUGE_FILE_FL                 0x00040000 /* Reserved for ext4 */
-- #define FS_EXTENT_FL                    0x00080000 /* Extents */
-- #define FS_EA_INODE_FL                  0x00200000 /* Inode used for large EA */
-- #define FS_EOFBLOCKS_FL                 0x00400000 /* Reserved for ext4 */
-- #define FS_NOCOW_FL                     0x00800000 /* Do not cow file */
-- #define FS_INLINE_DATA_FL               0x10000000 /* Reserved for ext4 */
-- #define FS_PROJINHERIT_FL               0x20000000 /* Create with parents projid */
-- #define FS_RESERVED_FL                  0x80000000 /* reserved for ext2 lib */
-- 
-- #define FS_FL_USER_VISIBLE              0x0003DFFF /* User visible flags */
-- #define FS_FL_USER_MODIFIABLE           0x000380FF /* User modifiable flags */
-- 
-- 

data SyncFileRangeFlag
   = SyncFileRangeWaitBefore
   | SyncFileRangeWrite
   | SyncFileRangeWaitAfter
   deriving (Show,Eq,Enum,CBitSet)
