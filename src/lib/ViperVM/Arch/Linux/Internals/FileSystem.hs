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
   , ioctlFreeze
   , ioctlThaw
   , ioctlTrim
   , ioctlClone
   , ioctlCloneRange
   , ioctlGetFlags
   , ioctlSetFlags
   , ioctlGetVersion
   , ioctlSetVersion
   , ioctlGetFlags32
   , ioctlSetFlags32
   , ioctlGetVersion32
   , ioctlSetVersion32
   , ioctlGetXAttr
   , ioctlSetXAttr
   , InodeFlag (..)
   , inodeUserVisibleFlags
   , inodeUserModifiableFlags
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

instance Storable FileCloneRange where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

-- | struct fstrim_range
data TrimRange = TrimRange
   { trStart     :: Word64
   , trLength    :: Word64
   , trMinLength :: Word64
   }
   deriving (Show,Generic,CStorable)

instance Storable TrimRange where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek


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

instance Storable FileDedupeRangeInfo where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek


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

instance Storable FileDedupeRangeHeader where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek


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

instance Storable FilesStatStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

-- | struct inodes_stat_t
data InodesStat = InodesStat
   { isNrInodes :: CLong
   , isNrUnused :: CLong
   , isDummy    :: Vector 5 CLong -- padding for sysctl ABI compatibility
   }
   deriving (Show,Generic,CStorable)

instance Storable InodesStat where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

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

instance Storable FsxAttr where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek


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
-- #define FS_IOC_FIEMAP                   _IOWR('f', 11, struct fiemap)

-- | Freeze (FIFREEZE)
ioctlFreeze :: Handle -> Int -> SysRet Int
ioctlFreeze = ioctlReadWrite sysIoctl 88 119 defaultCheck

-- | Thaw (FITHAW)
ioctlThaw :: Handle -> Int -> SysRet Int
ioctlThaw = ioctlReadWrite sysIoctl 88 120 defaultCheck

-- | Trim (FITRIM)
ioctlTrim :: Handle -> TrimRange -> SysRet TrimRange
ioctlTrim = ioctlReadWrite sysIoctl 88 121 defaultCheck

-- | Clone (FICLONE)
ioctlClone :: Handle -> Int -> SysRet ()
ioctlClone = ioctlWrite sysIoctl 0x94 9 defaultCheck

-- | Clone range (FICLONERANGE)
ioctlCloneRange :: Handle -> FileCloneRange -> SysRet ()
ioctlCloneRange = ioctlWrite sysIoctl 0x94 13 defaultCheck

-- | FIDEDUPERANGE
-- The size parameter must be sizeOf FileDedupeRangeHeader
-- but the actual object is a FileDedupeRangeHeader followed by an array of
-- FileDedupeRangeInfo structures...
--
--ioctlDedupeRange :: Handle -> FileDe -> SysRet ()
--ioctlDedupeRange = ioctlWrite sysIoctl 0x94 54 defaultCheck


-- | FS_IOC_GETFLAGS
ioctlGetFlags :: Handle -> SysRet CLong
ioctlGetFlags = ioctlRead sysIoctl 102 1 defaultCheck

-- | FS_IOC_SETFLAGS
ioctlSetFlags :: Handle -> CLong -> SysRet ()
ioctlSetFlags = ioctlWrite sysIoctl 102 2 defaultCheck

-- | FS_IOC_GETVERSION
ioctlGetVersion :: Handle -> SysRet CLong
ioctlGetVersion = ioctlRead sysIoctl 118 1 defaultCheck

-- | FS_IOC_SETVERSION
ioctlSetVersion :: Handle -> CLong -> SysRet ()
ioctlSetVersion = ioctlWrite sysIoctl 118 2 defaultCheck


-- | FS_IOC32_GETFLAGS
ioctlGetFlags32 :: Handle -> SysRet Int
ioctlGetFlags32 = ioctlRead sysIoctl 102 1 defaultCheck

-- | FS_IOC32_SETFLAGS
ioctlSetFlags32 :: Handle -> Int -> SysRet ()
ioctlSetFlags32 = ioctlWrite sysIoctl 102 2 defaultCheck

-- | FS_IOC32_GETVERSION
ioctlGetVersion32 :: Handle -> SysRet Int
ioctlGetVersion32 = ioctlRead sysIoctl 118 1 defaultCheck

-- | FS_IOC32_SETVERSION
ioctlSetVersion32 :: Handle -> Int -> SysRet ()
ioctlSetVersion32 = ioctlWrite sysIoctl 118 2 defaultCheck

-- | FS_IOC_FSGETXATTR
ioctlGetXAttr :: Handle -> SysRet FsxAttr
ioctlGetXAttr = ioctlRead sysIoctl 88 31 defaultCheck

-- | FS_IOC_FSSETXATTR
ioctlSetXAttr :: Handle -> FsxAttr -> SysRet ()
ioctlSetXAttr = ioctlWrite sysIoctl 88 32 defaultCheck



-- | Inode flags (FS_IOC_GETFLAGS / FS_IOC_SETFLAGS)
data InodeFlag
   = InodeFlagSecureDeletion        -- ^ Secure deletion 
   | InodeFlagUndelete              -- ^ Undelete 
   | InodeFlagCompress              -- ^ Compress file 
   | InodeFlagSynchronous           -- ^ Synchronous updates 
   | InodeFlagImmutable             -- ^ Immutable file 
   | InodeFlagAppend                -- ^ writes to file may only append 
   | InodeFlagNoDump                -- ^ do not dump file 
   | InodeFlagNoAccessTime          -- ^ do not update atime 
   | InodeFlagDirty                 
   | InodeFlagCompressedClusters    -- ^ One or more compressed clusters 
   | InodeFlagNoCompression         -- ^ Don't compress 
   | InodeFlagEncrypt               -- ^ Encrypted file 
   | InodeFlagBTree_HashIndexed     -- ^ btree format dir / hash-indexed directory 
   | InodeFlagAFS                   -- ^ AFS directory 
   | InodeFlagJournalData           -- ^ Reserved for ext3 
   | InodeFlagNoTail                -- ^ file tail should not be merged 
   | InodeFlagDirSync               -- ^ dirsync behaviour (directories only) 
   | InodeFlagTopDir                -- ^ Top of directory hierarchies
   | InodeFlagHugeFile              -- ^ Reserved for ext4 
   | InodeFlagExtent                -- ^ Extents 
   | InodeFlagEA                    -- ^ Inode used for large EA 
   | InodeFlagEOFBlocks             -- ^ Reserved for ext4 
   | InodeFlagNoCow                 -- ^ Do not cow file 
   | InodeFlagInlineData            -- ^ Reserved for ext4 
   | InodeFlagProjectInherit        -- ^ Create with parents projid 
   | InodeFlagReserved              -- ^ reserved for ext2 lib 
   deriving (Show,Eq,Enum)

instance CBitSet InodeFlag where
   toBitOffset x = case x of
      InodeFlagSecureDeletion        -> 0
      InodeFlagUndelete              -> 1
      InodeFlagCompress              -> 2
      InodeFlagSynchronous           -> 3
      InodeFlagImmutable             -> 4
      InodeFlagAppend                -> 5
      InodeFlagNoDump                -> 6
      InodeFlagNoAccessTime          -> 7
      InodeFlagDirty                 -> 8
      InodeFlagCompressedClusters    -> 9
      InodeFlagNoCompression         -> 10
      InodeFlagEncrypt               -> 11
      InodeFlagBTree_HashIndexed     -> 12
      InodeFlagAFS                   -> 13
      InodeFlagJournalData           -> 14
      InodeFlagNoTail                -> 15
      InodeFlagDirSync               -> 16
      InodeFlagTopDir                -> 17
      InodeFlagHugeFile              -> 18
      InodeFlagExtent                -> 19
      InodeFlagEA                    -> 21
      InodeFlagEOFBlocks             -> 22
      InodeFlagNoCow                 -> 23
      InodeFlagInlineData            -> 28
      InodeFlagProjectInherit        -> 29
      InodeFlagReserved              -> 31

   fromBitOffset x = case x of
    0   -> InodeFlagSecureDeletion
    1   -> InodeFlagUndelete
    2   -> InodeFlagCompress
    3   -> InodeFlagSynchronous
    4   -> InodeFlagImmutable
    5   -> InodeFlagAppend
    6   -> InodeFlagNoDump
    7   -> InodeFlagNoAccessTime
    8   -> InodeFlagDirty
    9   -> InodeFlagCompressedClusters
    10  -> InodeFlagNoCompression
    11  -> InodeFlagEncrypt
    12  -> InodeFlagBTree_HashIndexed
    13  -> InodeFlagAFS
    14  -> InodeFlagJournalData
    15  -> InodeFlagNoTail
    16  -> InodeFlagDirSync
    17  -> InodeFlagTopDir
    18  -> InodeFlagHugeFile
    19  -> InodeFlagExtent
    21  -> InodeFlagEA
    22  -> InodeFlagEOFBlocks
    23  -> InodeFlagNoCow
    28  -> InodeFlagInlineData
    29  -> InodeFlagProjectInherit
    31  -> InodeFlagReserved
    _   -> error "Unknown Inode flag"


-- | User visible flags
inodeUserVisibleFlags :: BitSet Word32 InodeFlag
inodeUserVisibleFlags = BitSet.fromBits 0x0003DFFF

-- | User modifiable flags
inodeUserModifiableFlags :: BitSet Word32 InodeFlag
inodeUserModifiableFlags = BitSet.fromBits 0x000380FF

data SyncFileRangeFlag
   = SyncFileRangeWaitBefore
   | SyncFileRangeWrite
   | SyncFileRangeWaitAfter
   deriving (Show,Eq,Enum,CBitSet)
