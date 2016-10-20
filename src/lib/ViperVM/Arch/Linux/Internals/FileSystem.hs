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
   , Range (..)
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
import ViperVM.Arch.Linux.Handle
import ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Format.Binary.Vector as Vector
import ViperVM.Format.Binary.Enum
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Storable
import ViperVM.Utils.Flow
import ViperVM.Utils.Types.Generics (Generic)

import Foreign.Marshal.Utils (toBool)

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


csize :: Int
csize = sizeOf (undefined :: CSize)

-- blkIoctl :: Word8 -> Int64 -> Handle -> IOErr Int64
-- blkIoctl n = ioctlSignalValue 0x12 n
-- 
-- blkIoctlS' :: Word8 -> Int64 -> Handle -> IOErr ()
-- blkIoctlS' n b fd = do
--    r <- ioctlSignalValue 0x12 n b fd
--    return $ case r of
--       Left err -> Left err
--       Right _  -> Right ()
-- 
-- blkIoctlS :: Word8 -> Handle -> IOErr ()
-- blkIoctlS n = ioctlSignal 0x12 n
-- 
-- blkIoctlR :: Storable a => Word8 -> Handle -> IOErr a
-- blkIoctlR n = ioctlRead 0x12 n
-- 
-- blkIoctlW :: Storable a => Word8 -> Handle -> a -> IOErr ()
-- blkIoctlW n = ioctlWrite 0x12 n
-- 
-- 
-- onSuccessIO' :: IOErr a -> (a -> IO b) -> IOErr b
-- onSuccessIO' s f = do
--    r <- s
--    case r of
--       Left err -> return (Left err)
--       Right u  -> Right <$> f u

-- | BLKROSET set device read-only (0 = read-write)
ioctlSetReadOnlyStatus :: Bool -> Handle -> IOErr ()
ioctlSetReadOnlyStatus b =
   ioctlWriteCmd (ioctlCommand None 0x12 93 0) (if b then 1 else 0 :: Int)

-- | BLKROGET get read-only status (0 = read_write)
ioctlGetReadOnlyStatus :: Handle -> IOErr Bool
ioctlGetReadOnlyStatus fd =
   ioctlReadCmd (ioctlCommand None 0x12 94 0) fd
   >.-.> (toBool :: Int -> Bool)

-- | BLKRRPART re-read partition table
ioctlReReadPartitionTable :: Handle -> IOErr ()
ioctlReReadPartitionTable = ioctlSignal 0x12 95 (0 :: Int)

-- | BLKGETSIZE return device size /512 (long *arg)
-- Use the 64-bit version instead
--ioctlGetDeviceSize32 :: Handle -> IOErr CLong
--ioctlGetDeviceSize32 fd =
--   with 0 $ \(p :: Ptr CLong) ->
--      onSuccessIO (blkIoctl 96 0 fd) (const $ peek p)

-- | BLKFLSBUF  flush buffer cache
ioctlFlushBuferCache :: Handle -> IOErr ()
ioctlFlushBuferCache = ioctlSignal 0x12 97 (0 :: Int)

-- | BLKRASET set read ahead for block device
ioctlSetReadAhead :: Int64 -> Handle -> IOErr ()
ioctlSetReadAhead = ioctlSignal 0x12 98

-- | BLKRAGET get current read ahead setting
ioctlGetReadAhead :: Handle -> IOErr CLong
ioctlGetReadAhead = ioctlReadCmd (ioctlCommand None 0x12 99 0)

-- Linux aliases BLKRAGET/BLKRASET and BLKFRAGET/BLKFRASET
-- BLKFRASET  _IO(0x12,100)/* set filesystem (mm/filemap.c) read-ahead */
-- BLKFRAGET  _IO(0x12,101)/* get filesystem (mm/filemap.c) read-ahead */


-- | BLKSECTSET set max sectors per request
--
-- Doesn't seem to be supported by Linux (cf block/ioctl.c)
ioctlSetMaxSectors :: CUShort -> Handle -> IOErr ()
ioctlSetMaxSectors = ioctlSignal 0x12 102

-- | BLKSECTGET get max sectors per request
ioctlGetMaxSectors :: Handle -> IOErr CUShort
ioctlGetMaxSectors = ioctlReadCmd (ioctlCommand None 0x12 103 0)

-- | BLKSSZGET get block device logical block size
ioctlGetLogicalBlockSize :: Handle -> IOErr Int
ioctlGetLogicalBlockSize = ioctlReadCmd (ioctlCommand None 0x12 104 0)

-- | BLKBSZGET get block device soft block size
ioctlGetSoftBlockSize :: Handle -> IOErr Int
ioctlGetSoftBlockSize = ioctlReadCmd (ioctlCommand Read 0x12 112 csize)

-- | BLKBSZSET set block device soft block size
ioctlSetSoftBlockSize :: Int -> Handle -> IOErr ()
ioctlSetSoftBlockSize = ioctlWriteCmd (ioctlCommand Write 0x12 113 csize)

-- | BLKGETSIZE64 return device size in bytes
ioctlGetSize :: Handle -> IOErr Word64
ioctlGetSize = ioctlReadCmd (ioctlCommand Read 0x12 114 csize)

-- We haven't defined struct user_trace_setup yet
-- #define BLKTRACESETUP _IOWR(0x12,115,struct blk_user_trace_setup)

-- | BLKTRACESTART
ioctlTraceStart :: Handle -> IOErr ()
ioctlTraceStart = ioctlSignal 0x12 116 (0 :: Int)

-- | BLKTRACESTOP
ioctlTraceStop :: Handle -> IOErr ()
ioctlTraceStop = ioctlSignal 0x12 117 (0 :: Int)

-- | BLKTRACETEARDOWN
ioctlTraceTearDown :: Handle -> IOErr ()
ioctlTraceTearDown = ioctlSignal 0x12 118 (0 :: Int)

data Range = Range
   { rangeStart  :: Word64
   , rangeLength :: Word64
   } deriving (Generic,CStorable)

instance Storable Range where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke

-- | BLKDISCARD
ioctlDiscard :: Range -> Handle -> IOErr ()
ioctlDiscard = ioctlWriteCmd (ioctlCommand None 0x12 119 0)

-- | BLKIOMIN
ioctlGetIOMin :: Handle -> IOErr CUInt
ioctlGetIOMin = ioctlReadCmd (ioctlCommand None 0x12 120 0)

-- | BLKIOOPT
ioctlGetIOOpt :: Handle -> IOErr CUInt
ioctlGetIOOpt = ioctlReadCmd (ioctlCommand None 0x12 121 0)

-- | BLKALIGNOFF
ioctlGetAlignmentOffset :: Handle -> IOErr Int
ioctlGetAlignmentOffset = ioctlReadCmd (ioctlCommand None 0x12 122 0)

-- | BLKPBSZGET get block device physical block size
ioctlGetPhysicalBlockSize :: Handle -> IOErr CUInt
ioctlGetPhysicalBlockSize = ioctlReadCmd (ioctlCommand None 0x12 123 0)

-- | BLKDISCARDZEROES
ioctlDiscardZeroes :: Handle -> IOErr CUInt
ioctlDiscardZeroes = ioctlReadCmd (ioctlCommand None 0x12 124 0)

-- | BLKSECDISCARD
ioctlDiscardSecure :: Range -> Handle -> IOErr ()
ioctlDiscardSecure = ioctlWriteCmd (ioctlCommand None 0x12 125 0)

-- | BLKROTATIONAL
ioctlGetRotational :: Handle -> IOErr CUShort
ioctlGetRotational = ioctlReadCmd (ioctlCommand None 0x12 126 0)

-- | BLKZEROOUT
ioctlZeroOut :: Range -> Handle -> IOErr()
ioctlZeroOut = ioctlWriteCmd (ioctlCommand None 0x12 127 0)

-- | BLKDAXGET
ioctlGetDAX :: Handle -> IOErr Int
ioctlGetDAX  = ioctlReadCmd (ioctlCommand None 0x12 129 0)

-- #define FIBMAP     _IO(0x00,1)  /* bmap access */
-- #define FIGETBSZ   _IO(0x00,2)  /* get the block size used for bmap */
-- #define FS_IOC_FIEMAP                   _IOWR('f', 11, struct fiemap)

-- | Freeze (FIFREEZE)
ioctlFreeze :: Int -> Handle -> IOErr Int
ioctlFreeze = ioctlWriteRead 88 119

-- | Thaw (FITHAW)
ioctlThaw :: Int -> Handle -> IOErr Int
ioctlThaw = ioctlWriteRead 88 120

-- | Trim (FITRIM)
ioctlTrim :: TrimRange -> Handle -> IOErr TrimRange
ioctlTrim = ioctlWriteRead 88 121

-- | Clone (FICLONE)
ioctlClone :: Int -> Handle -> IOErr ()
ioctlClone = ioctlWrite 0x94 9

-- | Clone range (FICLONERANGE)
ioctlCloneRange :: FileCloneRange -> Handle -> IOErr ()
ioctlCloneRange = ioctlWrite 0x94 13

-- | FIDEDUPERANGE
-- The size parameter must be sizeOf FileDedupeRangeHeader
-- but the actual object is a FileDedupeRangeHeader followed by an array of
-- FileDedupeRangeInfo structures...
--
--ioctlDedupeRange :: Handle -> FileDe -> IOErr ()
--ioctlDedupeRange = ioctlWrite 0x94 54


-- | FS_IOC_GETFLAGS
ioctlGetFlags :: Handle -> IOErr CLong
ioctlGetFlags = ioctlRead 102 1

-- | FS_IOC_SETFLAGS
ioctlSetFlags :: CLong -> Handle -> IOErr ()
ioctlSetFlags = ioctlWrite 102 2

-- | FS_IOC_GETVERSION
ioctlGetVersion :: Handle -> IOErr CLong
ioctlGetVersion = ioctlRead 118 1

-- | FS_IOC_SETVERSION
ioctlSetVersion :: CLong -> Handle -> IOErr ()
ioctlSetVersion = ioctlWrite 118 2


-- | FS_IOC32_GETFLAGS
ioctlGetFlags32 :: Handle -> IOErr Int
ioctlGetFlags32 = ioctlRead 102 1

-- | FS_IOC32_SETFLAGS
ioctlSetFlags32 :: Int -> Handle -> IOErr ()
ioctlSetFlags32 = ioctlWrite 102 2

-- | FS_IOC32_GETVERSION
ioctlGetVersion32 :: Handle -> IOErr Int
ioctlGetVersion32 = ioctlRead 118 1

-- | FS_IOC32_SETVERSION
ioctlSetVersion32 :: Int -> Handle -> IOErr ()
ioctlSetVersion32 = ioctlWrite 118 2

-- | FS_IOC_FSGETXATTR
ioctlGetXAttr :: Handle -> IOErr FsxAttr
ioctlGetXAttr = ioctlRead 88 31

-- | FS_IOC_FSSETXATTR
ioctlSetXAttr :: FsxAttr -> Handle -> IOErr ()
ioctlSetXAttr = ioctlWrite 88 32



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
