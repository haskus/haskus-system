{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module ViperVM.Arch.Linux.FileSystem
   ( FilePermission(..)
   , FilePermissions
   , FileType(..)
   , FileOption(..)
   , FileOptions
   , makeMode
   , OpenFlag(..)
   , SeekWhence(..)
   , AccessMode(..)
   , AccessModes
   , FileLock(..)
   , Device(..)
   , withDevice
   , Stat(..)
   , sysOpen
   , sysOpenAt
   , sysCreate
   , sysClose
   , sysSeek
   , sysSeek'
   , sysIoctl
   , sysAccess
   , sysDup
   , sysDup2
   , sysSetCurrentDirectory
   , sysSetCurrentDirectoryPath
   , sysGetCurrentDirectory
   , sysRename
   , sysFileLock
   , sysFileSync
   , sysFileDataSync
   , sysTruncate
   , sysTruncatePath
   , sysLink
   , sysSymlink
   , sysUnlink
   , sysUnlinkAt
   , sysChangePermission
   , sysChangePermissionPath
   , sysChangeOwnership
   , sysChangeOwnershipPath
   , sysChangeLinkOwnershipPath
   , sysSetProcessUMask
   , sysFileStat
   , sysFileDescriptorStat
   , sysSync
   , sysSyncFS
   , sysCreateSpecialFile
   , sysCreateSpecialFileAt
   , DeviceType(..)
   , createDeviceFile
   )
where

import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (Storable, peek, poke, sizeOf, alignment)
import Foreign.CStorable
import Data.Word (Word64, Word32)
import Foreign.C.String (CString, withCString, peekCString)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Bits (FiniteBits, Bits, (.|.), (.&.), shiftR, shiftL, complement)

import GHC.Generics (Generic)

import ViperVM.Format.Binary.BitSet
import qualified ViperVM.Format.Binary.BitSet as BitSet

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Arch.Linux.Utils (toSet)
import ViperVM.Arch.Linux.Time (TimeSpec)
import ViperVM.Arch.Linux.Process (UserID(..), GroupID(..))

-- | Open a file
sysOpen :: FilePath -> [OpenFlag] -> FilePermissions -> SysRet FileDescriptor
sysOpen path flags mode = 
   withCString path $ \path' -> 
      onSuccess (syscall_open path' (toSet flags :: Int) (BitSet.toBits mode))
         (FileDescriptor . fromIntegral)

-- | Open a file
sysOpenAt :: FileDescriptor -> FilePath -> [OpenFlag] -> FilePermissions -> SysRet FileDescriptor
sysOpenAt (FileDescriptor fd) path flags mode = 
   withCString path $ \path' -> 
      onSuccess (syscall_openat fd path' (toSet flags :: Int) (BitSet.toBits mode))
         (FileDescriptor . fromIntegral)

sysCreateCString :: CString -> FilePermissions -> SysRet FileDescriptor
sysCreateCString path mode = 
   onSuccess (syscall_creat path (BitSet.toBits mode)) (FileDescriptor . fromIntegral)

sysCreate :: String -> FilePermissions -> SysRet FileDescriptor
sysCreate path mode = withCString path $ \path' -> sysCreateCString path' mode

-- | Close a file descriptor
sysClose :: FileDescriptor -> SysRet ()
sysClose (FileDescriptor fd) =
   onSuccess (syscall_close fd) (const ())


-- | Flags for "open" syscall
data OpenFlag =
     OpenReadOnly
   | OpenWriteOnly
   | OpenReadWrite
   | CloseOnExec
   | OpenAppend
   | OpenAsync
   | OpenCreate
   | OpenDirect
   | OpenDirectory
   | OpenExclusive
   | OpenLargeFile
   | OpenWithoutAccessTime
   | OpenNoTTYControl
   | OpenDontFollowSymLinks
   | OpenNonBlocking
   | OpenPath
   | OpenSynchronous
   | OpenTmpFile
   | OpenTruncate
   deriving (Show,Eq)

instance Enum OpenFlag where
   fromEnum x = case x of
      OpenReadOnly            -> 0o00000000
      OpenWriteOnly           -> 0o00000001
      OpenReadWrite           -> 0o00000002
      OpenCreate              -> 0o00000100
      OpenExclusive           -> 0o00000200
      OpenNoTTYControl        -> 0o00000400
      OpenTruncate            -> 0o00001000
      OpenAppend              -> 0o00002000
      OpenNonBlocking         -> 0o00004000
      OpenSynchronous         -> 0o00010000
      OpenAsync               -> 0o00020000
      OpenDirect              -> 0o00040000
      OpenLargeFile           -> 0o00100000
      OpenDirectory           -> 0o00200000
      OpenDontFollowSymLinks  -> 0o00400000
      OpenWithoutAccessTime   -> 0o01000000
      CloseOnExec             -> 0o02000000
      OpenPath                -> 0o10000000
      OpenTmpFile             -> 0o20000000

   toEnum x = case x of
      0o00000000 -> OpenReadOnly
      0o00000001 -> OpenWriteOnly
      0o00000002 -> OpenReadWrite
      0o00000100 -> OpenCreate
      0o00000200 -> OpenExclusive
      0o00000400 -> OpenNoTTYControl
      0o00001000 -> OpenTruncate
      0o00002000 -> OpenAppend
      0o00004000 -> OpenNonBlocking
      0o00010000 -> OpenSynchronous
      0o00020000 -> OpenAsync
      0o00040000 -> OpenDirect
      0o00100000 -> OpenLargeFile
      0o00200000 -> OpenDirectory
      0o00400000 -> OpenDontFollowSymLinks
      0o01000000 -> OpenWithoutAccessTime
      0o02000000 -> CloseOnExec
      0o10000000 -> OpenPath
      0o20000000 -> OpenTmpFile
      _ -> error "Unknown Open flag value"


-- | File permissions
data FilePermission
   = PermOtherExecute
   | PermOtherWrite
   | PermOtherRead
   | PermGroupExecute
   | PermGroupWrite
   | PermGroupRead
   | PermUserExecute
   | PermUserWrite
   | PermUserRead
   deriving (Eq,Show,Enum)

instance EnumBitSet FilePermission

type FilePermissions = BitSet Word FilePermission

data SeekWhence = 
     SeekSet 
   | SeekCurrent 
   | SeekEnd 
   | SeekData
   | SeekHole
   deriving (Enum,Eq,Show)

-- | Reposition read/write file offset, return the new position
sysSeek :: FileDescriptor -> Int64 -> SeekWhence -> SysRet Int64
sysSeek (FileDescriptor fd) off whence =
   onSuccess (syscall_lseek fd off (fromEnum whence)) id

-- | Reposition read/write file offset
sysSeek' :: FileDescriptor -> Int64 -> SeekWhence -> SysRet ()
sysSeek' fd off whence = fmap (const ()) <$> (sysSeek fd off whence)

-- | Send a custom command to a device
sysIoctl :: FileDescriptor -> Int64 -> Int64 -> IO Int64
sysIoctl (FileDescriptor fd) cmd arg =
   syscall_ioctl fd cmd arg


-- | Access mode
--
-- To test if a file exists, use no flag
data AccessMode
   = AccessExecute  -- bit 0
   | AccessWrite    -- bit 1
   | AccessRead     -- bit 2
   deriving (Eq,Show,Enum)

instance EnumBitSet AccessMode

type AccessModes = BitSet Word64 AccessMode

sysAccess :: FilePath -> AccessModes -> SysRet ()
sysAccess path mode = withCString path $ \path' ->
   onSuccess (syscall_access path' (BitSet.toBits mode)) (const ())


sysDup :: FileDescriptor -> SysRet FileDescriptor
sysDup (FileDescriptor oldfd) = 
   onSuccess (syscall_dup oldfd) (FileDescriptor . fromIntegral)

sysDup2 :: FileDescriptor -> FileDescriptor -> SysRet FileDescriptor
sysDup2 (FileDescriptor oldfd) (FileDescriptor newfd) = 
   onSuccess (syscall_dup2 oldfd newfd) (FileDescriptor . fromIntegral)

sysSetCurrentDirectoryPath :: FilePath -> SysRet ()
sysSetCurrentDirectoryPath path = withCString path $ \path' ->
   onSuccess (syscall_chdir path') (const ())

sysSetCurrentDirectory :: FileDescriptor -> SysRet ()
sysSetCurrentDirectory (FileDescriptor fd) = 
   onSuccess (syscall_fchdir fd) (const ())

sysGetCurrentDirectory :: SysRet FilePath
sysGetCurrentDirectory = go 128
   where
      go n = allocaArray n $ \ptr -> do
         ret <- onSuccessIO (syscall_getcwd ptr (fromIntegral n)) (const (peekCString ptr))
         case ret of
            Left ERANGE -> go (2 * n)
            _ -> return ret

sysRename :: FilePath -> FilePath -> SysRet ()
sysRename oldPath newPath =
   withCString oldPath $ \old' ->
      withCString newPath $ \new' ->
         onSuccess (syscall_rename old' new') (const ())

data FileLock =
     SharedLock
   | ExclusiveLock
   | RemoveLock

sysFileLock :: FileDescriptor -> FileLock -> Bool -> SysRet ()
sysFileLock (FileDescriptor fd) mode nonBlocking = do
   let
      mode' = case mode of
         SharedLock     -> 1
         ExclusiveLock  -> 2
         RemoveLock     -> 8

      nb = if nonBlocking then 4 else 0

   onSuccess (syscall_flock fd (mode' .|. nb :: Int64)) (const ())


sysFileSync :: FileDescriptor -> SysRet ()
sysFileSync (FileDescriptor fd) = onSuccess (syscall_fsync fd) (const ())

sysFileDataSync :: FileDescriptor -> SysRet ()
sysFileDataSync (FileDescriptor fd) = onSuccess (syscall_fdatasync fd) (const ())

sysTruncatePath :: FilePath -> Word64 -> SysRet ()
sysTruncatePath path size = withCString path $ \path' ->
   onSuccess (syscall_truncate path' size) (const ())

sysTruncate :: FileDescriptor -> Word64 -> SysRet ()
sysTruncate (FileDescriptor fd) size =
   onSuccess (syscall_ftruncate fd size) (const ())

sysLink :: FilePath -> FilePath -> SysRet ()
sysLink src dest =
   withCString src $ \src' ->
      withCString dest $ \dest' ->
         onSuccess (syscall_link src' dest') (const ())

sysSymlink :: FilePath -> FilePath -> SysRet ()
sysSymlink src dest =
   withCString src $ \src' ->
      withCString dest $ \dest' ->
         onSuccess (syscall_symlink src' dest') (const ())

sysUnlink :: FilePath -> SysRet ()
sysUnlink path = withCString path $ \path' ->
   onSuccess (syscall_unlink path') (const ())

sysUnlinkAt :: FileDescriptor -> FilePath -> Bool -> SysRet ()
sysUnlinkAt (FileDescriptor fd) path rmdir = withCString path $ \path' ->
   onSuccess (syscall_unlinkat fd path' (if rmdir then 0x200 else 0)) (const ())

sysChangePermissionPath :: FilePath -> FilePermissions -> SysRet ()
sysChangePermissionPath path mode = withCString path $ \path' ->
   onSuccess (syscall_chmod path' (BitSet.toBits mode)) (const ())

sysChangePermission :: FileDescriptor -> FilePermissions -> SysRet ()
sysChangePermission (FileDescriptor fd) mode = 
   onSuccess (syscall_fchmod fd (BitSet.toBits mode)) (const ())


-- | Avoid duplication in *chown syscalls
chownEx :: (x -> Word32 -> Word32 -> IO Int64) -> x -> Maybe UserID -> Maybe GroupID -> SysRet ()
chownEx sc a uid gid = do
   let
      fuid (UserID x) = x
      fgid (GroupID x) = x
      uid' = maybe (-1) fuid uid
      gid' = maybe (-1) fgid gid
   onSuccess (sc a uid' gid') (const ())


-- | chown
sysChangeOwnershipPath :: FilePath -> Maybe UserID -> Maybe GroupID -> SysRet ()
sysChangeOwnershipPath path uid gid = withCString path (\p -> chownEx syscall_chown p uid gid)

-- | lchown
sysChangeLinkOwnershipPath :: FilePath -> Maybe UserID -> Maybe GroupID -> SysRet ()
sysChangeLinkOwnershipPath path uid gid = withCString path (\p -> chownEx syscall_lchown p uid gid)

-- | fchown
sysChangeOwnership :: FileDescriptor -> Maybe UserID -> Maybe GroupID -> SysRet ()
sysChangeOwnership (FileDescriptor fd) = chownEx syscall_fchown fd

-- | umask
sysSetProcessUMask :: FilePermissions -> SysRet FilePermissions
sysSetProcessUMask mode =
   onSuccess (syscall_umask (BitSet.toBits mode)) (fromBits . fromIntegral)

-- | File type
data FileType
   = FileTypeSocket
   | FileTypeLink
   | FileTypeFile
   | FileTypeBlockDevice
   | FileTypeCharDevice
   | FileTypeFIFO
   | FileTypeDirectory
   deriving (Show,Eq)

instance Enum FileType where
   fromEnum x = case x of
      FileTypeSocket       -> 12
      FileTypeLink         -> 10
      FileTypeFile         -> 8
      FileTypeBlockDevice  -> 6
      FileTypeCharDevice   -> 2
      FileTypeFIFO         -> 1
      FileTypeDirectory    -> 4
   toEnum x = case x of
      12 -> FileTypeSocket
      10 -> FileTypeLink
      8  -> FileTypeFile
      6  -> FileTypeBlockDevice
      2  -> FileTypeCharDevice
      1  -> FileTypeFIFO
      4  -> FileTypeDirectory
      _  -> error $ "Invalid file type: " ++ show x

-- | Read file type from Stat "mode" field 
modeFileType :: (Num a, Bits a, Integral a) => a -> FileType
modeFileType x = toEnum (fromIntegral ((x `shiftR` 12) .&. 0x0F))

-- | Create file type value for mode
fromFileType :: (Bits a, Num a) => FileType -> a
fromFileType x = fromIntegral (fromEnum x) `shiftL` 12

-- | File options
data FileOption
   = FileOptSticky
   | FileOptSetGID
   | FileOptSetUID
   deriving (Show,Eq,Enum)

instance EnumBitSet FileOption

type FileOptions = BitSet Word64 FileOption

-- | Read file options from Stat "mode" field 
modeFileOptions :: (Integral a, FiniteBits a) => a -> FileOptions
modeFileOptions x = BitSet.fromBits ((fromIntegral x `shiftR` 9) .&. 0x07)

makeMode :: FilePermissions -> FileOptions -> Word64
makeMode perm opt = fromIntegral (BitSet.toBits perm) 
                .|. (fromIntegral (BitSet.toBits opt)  `shiftL` 9)

-- | Read file permission from Stat "mode" field 
modeFilePermission :: (Integral a, FiniteBits a) => a -> FilePermissions
modeFilePermission x = fromBits (fromIntegral x .&. 0x01FF)

-- | Device identifier
data Device = Device
   { deviceMajor :: Word32
   , deviceMinor :: Word32
   } deriving (Show,Eq)

instance Storable Device where
   sizeOf _    = 8
   alignment _ = alignment (undefined :: Word64)
   peek x      = f <$> peek (castPtr x :: Ptr Word64)
      where
         f y = Device
            { deviceMajor = fromIntegral $
                              ((y `shiftR` 8) .&. 0xFFF) .|.
                              ((y `shiftR` 32) .&. complement 0xFFF)
            , deviceMinor = fromIntegral $
                              (y .&. 0xFF) .|.
                              ((y `shiftR` 12) .&. complement 0xFF)
            }
   poke ptr x = poke (castPtr ptr :: Ptr Word64) (toKernelDevice x)

-- | Convert a Device into a Word64 suitable for the kernel
toKernelDevice :: Device -> Word64
toKernelDevice dev =
      (minor .&. 0xFF) 
        .|. ((major .&. 0xfff) `shiftL` 8)
        .|. ((minor .&. complement 0xff) `shiftL` 12)
        .|. ((major .&. complement 0xfff) `shiftL` 32)
   where
      minor = fromIntegral (deviceMinor dev) :: Word64
      major = fromIntegral (deviceMajor dev) :: Word64

-- | Convert a Device into a Word64 suitable for the kernel
withDevice :: Device -> (Word64 -> a) -> a
withDevice dev f = f (toKernelDevice dev)

-- | File stat
--
-- Warning: the original structure is not portable between different
-- architectures (a lot of ifdefs for field sizes and field order...)
-- This one is for x86-64
data StatStruct = StatStruct
   { statDevice'           :: StorableWrap Device
   , statInode'            :: Word64
   , statLinkCount'        :: Word64
   , statMode'             :: Word32
   , statUID'              :: Word32
   , statGID'              :: Word32
   , statPad0'             :: Word32
   , statDevNum'           :: StorableWrap Device
   , statSize'             :: Int64
   , statBlockSize'        :: Int64
   , statBlockCount'       :: Int64
   , statLastAccess'       :: TimeSpec
   , statLastModif'        :: TimeSpec
   , statLastStatusChange' :: TimeSpec
   } deriving (Generic)

instance CStorable StatStruct
instance Storable StatStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

data Stat = Stat
   { statDevice            :: Device
   , statInode             :: Word64
   , statLinkCount         :: Word64
   , statMode              :: Word32
   , statFileType          :: FileType
   , statFileOptions       :: FileOptions
   , statFilePermissions   :: FilePermissions
   , statUID               :: Word32
   , statGID               :: Word32
   , statDevNum            :: Device
   , statSize              :: Int64
   , statBlockSize         :: Int64
   , statBlockCount        :: Int64
   , statLastAccess        :: TimeSpec
   , statLastModif         :: TimeSpec
   , statLastStatusChange  :: TimeSpec
   } deriving (Show)

toStat :: StatStruct -> Stat
toStat (StatStruct {..}) = 
   let
      Storable statDevice'' = statDevice'
      Storable statDevNum'' = statDevNum'
   in Stat
      { statDevice            = statDevice''
      , statInode             = statInode'
      , statMode              = statMode'
      , statFileType          = modeFileType statMode'
      , statFileOptions       = modeFileOptions statMode'
      , statFilePermissions   = modeFilePermission statMode'
      , statLinkCount         = statLinkCount'
      , statUID               = statUID'
      , statGID               = statGID'
      , statDevNum            = statDevNum''
      , statSize              = statSize'
      , statBlockSize         = statBlockSize'
      , statBlockCount        = statBlockCount'
      , statLastAccess        = statLastAccess'
      , statLastModif         = statLastModif'
      , statLastStatusChange  = statLastStatusChange'
      }

-- | Stat on a path
--
-- If the path targets a symbolic link and followLink is false, then returned
-- information are about the link itself
sysFileStat :: FilePath -> Bool -> SysRet Stat
sysFileStat path followLink = do
   withCString path $ \path' ->
      allocaBytes (sizeOf (undefined :: StatStruct)) $ \s ->
         let
            -- select between stat and lstat syscalls
            sc = if followLink then syscall_stat else syscall_lstat
         in
         onSuccessIO (sc path' s) (const (toStat <$> peek s))

-- | Stat on file descriptor
sysFileDescriptorStat :: FileDescriptor -> SysRet Stat
sysFileDescriptorStat (FileDescriptor fd) =
   allocaBytes (sizeOf (undefined :: StatStruct)) $ \s ->
      onSuccessIO (syscall_fstat fd s) (const (toStat <$> peek s))


sysSync :: SysRet ()
sysSync = onSuccess syscall_sync (const ())

sysSyncFS :: FileDescriptor -> SysRet ()
sysSyncFS (FileDescriptor fd) = onSuccess (syscall_syncfs fd) (const ())

-- | Create a special file
--
-- mknod syscall
sysCreateSpecialFile :: FilePath -> FileType -> FilePermissions -> Maybe Device -> SysRet ()
sysCreateSpecialFile path typ perm dev = do
   let 
      mode = fromIntegral (toBits perm) .|. fromFileType typ :: Word64
      dev' = fromMaybe (Device 0 0) dev

   withCString path $ \path' ->
      withDevice dev' $ \dev'' ->
         onSuccess (syscall_mknod path' mode dev'') (const ())

-- | Create a special file
--
-- mknodat syscall
sysCreateSpecialFileAt :: FileDescriptor -> FilePath -> FileType -> FilePermissions -> Maybe Device -> SysRet ()
sysCreateSpecialFileAt (FileDescriptor fd) path typ perm dev = do
   let 
      mode = fromIntegral (toBits perm) .|. fromFileType typ :: Word64
      dev' = fromMaybe (Device 0 0) dev

   withCString path $ \path' ->
      withDevice dev' $ \dev'' ->
         onSuccess (syscall_mknodat fd path' mode dev'') (const ())

data DeviceType = CharDevice | BlockDevice
   deriving (Show,Eq,Ord)

-- | Create a device special file
createDeviceFile :: FileDescriptor -> FilePath -> DeviceType -> FilePermissions -> Device -> SysRet ()
createDeviceFile fd path typ perm dev = sysCreateSpecialFileAt fd path typ' perm (Just dev)
   where
      typ' = case typ of
         CharDevice  -> FileTypeCharDevice
         BlockDevice -> FileTypeBlockDevice
