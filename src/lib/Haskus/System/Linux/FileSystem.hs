{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Haskus.System.Linux.FileSystem
   ( FilePermission(..)
   , FilePermissions
   , FileType(..)
   , FileOption(..)
   , FileOptions
   , makeMode
   , SeekWhence(..)
   , AccessMode(..)
   , AccessModes
   , FileLock(..)
   , Stat(..)
   , sysSeek
   , sysSeek'
   , sysAccess
   , sysDup
   , sysDup2
   , sysSetCurrentDirectory
   , sysSetCurrentDirectoryPath
   , sysGetCurrentDirectory
   , sysFileLock
   , sysTruncate
   , sysTruncatePath
   , sysLink
   , sysUnlink
   , sysUnlinkAt
   , sysChangePermission
   , sysChangePermissionPath
   , sysChangeOwnership
   , sysChangeOwnershipPath
   , sysChangeLinkOwnershipPath
   , sysSetProcessUMask
   , sysFileStat
   , sysHandleStat
   , sysCreateSpecialFile
   -- ** Open/Close
   , OpenErrors
   , open
   , close
   -- ** Synchronization
   , syncAll
   , syncAllByHandle
   , syncHandle
   -- ** Rename
   , RenameErrors
   , rename
   -- * Device
   , DeviceID (..)
   , withDeviceID
   )
where

import Haskus.Format.Binary.Bits
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Ptr (Ptr, castPtr)
import Haskus.Format.Binary.BitSet
import Haskus.Format.String
import qualified Haskus.Format.Binary.BitSet as BitSet
import Haskus.Utils.Flow
import Haskus.Utils.Maybe
import Haskus.Utils.Types.Generics (Generic)

import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Error
import Haskus.System.Linux.Syscalls
import Haskus.System.Linux.Time (TimeSpec)
import Haskus.System.Linux.Process (UserID(..), GroupID(..))
import Haskus.System.Linux.Internals.FileSystem

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
   deriving (Eq,Show,Enum,CBitSet)

type FilePermissions = BitSet Word FilePermission

-- | Reposition read/write file offset, return the new position
sysSeek :: MonadIO m => Handle -> Int64 -> SeekWhence -> Flow m '[Int64,ErrorCode]
sysSeek (Handle fd) off whence =
   liftIO (syscall_lseek fd off (fromEnum whence))
      ||> toErrorCode

-- | Reposition read/write file offset
sysSeek' :: MonadIO m => Handle -> Int64 -> SeekWhence -> Flow m '[(),ErrorCode]
sysSeek' fd off whence = sysSeek fd off whence >.-.> const ()


-- | Access mode
--
-- To test if a file exists, use no flag
data AccessMode
   = AccessExecute
   | AccessWrite
   | AccessRead
   deriving (Eq,Show,Enum,CBitSet)

type AccessModes = BitSet Word64 AccessMode

sysAccess :: MonadInIO m => FilePath -> AccessModes -> Flow m '[(),ErrorCode]
sysAccess path mode = withCString path $ \path' ->
   liftIO (syscall_access path' (BitSet.toBits mode))
      ||> toErrorCodeVoid


sysDup :: MonadIO m => Handle -> Flow m '[Handle,ErrorCode]
sysDup (Handle oldfd) = 
   liftIO (syscall_dup oldfd)
      ||> toErrorCodePure (Handle . fromIntegral)

sysDup2 :: MonadIO m => Handle -> Handle -> Flow m '[Handle,ErrorCode]
sysDup2 (Handle oldfd) (Handle newfd) = 
   liftIO (syscall_dup2 oldfd newfd)
      ||> toErrorCodePure (Handle . fromIntegral)

sysSetCurrentDirectoryPath :: MonadInIO m => FilePath -> Flow m '[(),ErrorCode]
sysSetCurrentDirectoryPath path = withCString path $ \path' ->
   liftIO (syscall_chdir path')
      ||> toErrorCodeVoid

sysSetCurrentDirectory :: MonadIO m => Handle -> Flow m '[(),ErrorCode]
sysSetCurrentDirectory (Handle fd) = 
   liftIO (syscall_fchdir fd)
      ||> toErrorCodeVoid

sysGetCurrentDirectory :: MonadInIO m => Flow m '[FilePath,ErrorCode]
sysGetCurrentDirectory = go 128
   where
      go n = allocaArray n $ \ptr -> do
         liftIO (syscall_getcwd ptr (fromIntegral n))
            ||>   toErrorCode
            >.~.> const (peekCString ptr)
            >%~^> \case
               ERANGE -> go (2 * n)
               e      -> flowSet e

data FileLock =
     SharedLock
   | ExclusiveLock
   | RemoveLock

sysFileLock :: MonadIO m => Handle -> FileLock -> Bool -> Flow m '[(),ErrorCode]
sysFileLock (Handle fd) mode nonBlocking =
   liftIO (syscall_flock fd (mode' .|. nb :: Int64))
      ||> toErrorCodeVoid
   where
      mode' = case mode of
         SharedLock     -> 1
         ExclusiveLock  -> 2
         RemoveLock     -> 8
      nb = if nonBlocking then 4 else 0

sysTruncatePath :: MonadInIO m => FilePath -> Word64 -> Flow m '[(),ErrorCode]
sysTruncatePath path size = withCString path $ \path' ->
   liftIO (syscall_truncate path' size)
      ||> toErrorCodeVoid

sysTruncate :: MonadIO m => Handle -> Word64 -> Flow m '[(),ErrorCode]
sysTruncate (Handle fd) size =
   liftIO (syscall_ftruncate fd size)
      ||> toErrorCodeVoid

sysLink :: MonadInIO m => FilePath -> FilePath -> Flow m '[(),ErrorCode]
sysLink src dest =
   withCString src $ \src' ->
      withCString dest $ \dest' ->
         liftIO (syscall_link src' dest')
            ||> toErrorCodeVoid

sysUnlink :: MonadInIO m => FilePath -> Flow m '[(),ErrorCode]
sysUnlink path = withCString path $ \path' ->
   liftIO (syscall_unlink path')
      ||> toErrorCodeVoid

sysUnlinkAt :: MonadInIO m => Handle -> FilePath -> Bool -> Flow m '[(),ErrorCode]
sysUnlinkAt (Handle fd) path rmdir = withCString path $ \path' ->
   liftIO (syscall_unlinkat fd path' (if rmdir then 0x200 else 0))
      ||> toErrorCodeVoid


sysChangePermissionPath :: MonadInIO m => FilePath -> FilePermissions -> Flow m '[(),ErrorCode]
sysChangePermissionPath path mode = withCString path $ \path' ->
   liftIO (syscall_chmod path' (BitSet.toBits mode))
      ||> toErrorCodeVoid

sysChangePermission :: MonadIO m => Handle -> FilePermissions -> Flow m '[(),ErrorCode]
sysChangePermission (Handle fd) mode = 
   liftIO (syscall_fchmod fd (BitSet.toBits mode))
      ||> toErrorCodeVoid


-- | Avoid duplication in *chown syscalls
chownEx :: MonadInIO m => (x -> Word32 -> Word32 -> IO Int64) -> x -> Maybe UserID -> Maybe GroupID -> Flow m '[(),ErrorCode]
chownEx sc a uid gid = liftIO (sc a uid' gid') ||> toErrorCodeVoid
   where
      fuid (UserID x) = x
      fgid (GroupID x) = x
      uid' = maybe maxBound fuid uid
      gid' = maybe maxBound fgid gid


-- | chown
sysChangeOwnershipPath :: MonadInIO m => FilePath -> Maybe UserID -> Maybe GroupID -> Flow m '[(),ErrorCode]
sysChangeOwnershipPath path uid gid = withCString path (\p -> chownEx (syscall_chown) p uid gid)

-- | lchown
sysChangeLinkOwnershipPath :: MonadInIO m => FilePath -> Maybe UserID -> Maybe GroupID -> Flow m '[(),ErrorCode]
sysChangeLinkOwnershipPath path uid gid = withCString path (\p -> chownEx (syscall_lchown) p uid gid)

-- | fchown
sysChangeOwnership :: MonadInIO m => Handle -> Maybe UserID -> Maybe GroupID -> Flow m '[(),ErrorCode]
sysChangeOwnership (Handle fd) = chownEx (syscall_fchown) fd

-- | umask
sysSetProcessUMask :: MonadIO m => FilePermissions -> Flow m '[FilePermissions,ErrorCode]
sysSetProcessUMask mode =
   liftIO (syscall_umask (BitSet.toBits mode))
      ||> toErrorCodePure (fromBits . fromIntegral)

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
   deriving (Show,Eq,Enum,CBitSet)

type FileOptions = BitSet Word64 FileOption

-- | Read file options from Stat "mode" field 
modeFileOptions :: (Integral a, FiniteBits a) => a -> FileOptions
modeFileOptions x = BitSet.fromBits ((fromIntegral x `shiftR` 9) .&. 0x07)

makeMode :: FileType -> FilePermissions -> FileOptions -> Word64
makeMode typ perm opt =
   fromIntegral (BitSet.toBits perm) 
   .|. (fromIntegral (BitSet.toBits opt)  `shiftL` 9)
   .|. fromFileType typ

-- | Read file permission from Stat "mode" field 
modeFilePermission :: (Integral a, FiniteBits a) => a -> FilePermissions
modeFilePermission x = fromBits (fromIntegral x .&. 0x01FF)


-- | File stat
--
-- Warning: the original structure is not portable between different
-- architectures (a lot of ifdefs for field sizes and field order...)
-- This one is for x86-64
data StatStruct = StatStruct
   { statDevice'           :: DeviceID
   , statInode'            :: Word64
   , statLinkCount'        :: Word64
   , statMode'             :: Word32
   , statUID'              :: Word32
   , statGID'              :: Word32
   , statPad0'             :: Word32
   , statDevNum'           :: DeviceID
   , statSize'             :: Int64
   , statBlockSize'        :: Int64
   , statBlockCount'       :: Int64
   , statLastAccess'       :: TimeSpec
   , statLastModif'        :: TimeSpec
   , statLastStatusChange' :: TimeSpec
   } deriving (Generic,Storable)

data Stat = Stat
   { statDevice            :: DeviceID
   , statInode             :: Word64
   , statLinkCount         :: Word64
   , statMode              :: Word32
   , statFileType          :: FileType
   , statFileOptions       :: FileOptions
   , statFilePermissions   :: FilePermissions
   , statUID               :: Word32
   , statGID               :: Word32
   , statDevNum            :: DeviceID
   , statSize              :: Int64
   , statBlockSize         :: Int64
   , statBlockCount        :: Int64
   , statLastAccess        :: TimeSpec
   , statLastModif         :: TimeSpec
   , statLastStatusChange  :: TimeSpec
   } deriving (Show)

toStat :: StatStruct -> Stat
toStat (StatStruct {..}) = Stat
   { statDevice            = statDevice'
   , statInode             = statInode'
   , statMode              = statMode'
   , statFileType          = modeFileType statMode'
   , statFileOptions       = modeFileOptions statMode'
   , statFilePermissions   = modeFilePermission statMode'
   , statLinkCount         = statLinkCount'
   , statUID               = statUID'
   , statGID               = statGID'
   , statDevNum            = statDevNum'
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
sysFileStat :: MonadInIO m => FilePath -> Bool -> Flow m '[Stat,ErrorCode]
sysFileStat path followLink = do
   withCString path $ \path' ->
      allocaBytes (sizeOfT' @StatStruct) $ \s ->
         let
            -- select between stat and lstat syscalls
            sc = if followLink then syscall_stat else syscall_lstat
         in
         liftIO (sc path' (castPtr s))
            ||>   toErrorCode
            >.~.> (const (toStat <$> peek s))

-- | Stat on file descriptor
sysHandleStat :: MonadInIO m => Handle -> Flow m '[Stat,ErrorCode]
sysHandleStat (Handle fd) =
   allocaBytes (sizeOfT' @StatStruct) $ \s ->
      liftIO (syscall_fstat fd (castPtr s))
         ||>   toErrorCode
         >.~.> (const (toStat <$> peek s))


-- | Create a special file
--
-- mknodat syscall. 
sysCreateSpecialFile :: MonadInIO m => Maybe Handle -> FilePath -> FileType -> FilePermissions -> Maybe DeviceID -> Flow m '[(),ErrorCode]
sysCreateSpecialFile hdl path typ perm dev = do
   let 
      mode = fromIntegral (toBits perm) .|. fromFileType typ :: Word64
      dev' = fromMaybe (DeviceID 0 0) dev
      -- We pass a dummy file descriptor if the handle is not required
      fd   = case hdl of
                  Just (Handle x) -> x
                  Nothing         -> maxBound
   withCString path $ \path' ->
      withDeviceID dev' $ \dev'' ->
         liftIO (syscall_mknodat fd path' mode dev'')
            ||> toErrorCodeVoid

-----------------------------------------------------------------------
-- Open / Close
-----------------------------------------------------------------------

type OpenErrors
   = '[ NotAllowed
      , ExhaustedQuota
      , FileAlreadyExists
      , MemoryError
      , Overflow
      , Interrupted
      , InvalidParam
      , InvalidIsDirectory
      , SymbolicLinkLoop
      , TooManyProcessHandles
      , TooManySystemHandles
      , TooLongPathName
      , DeviceNotFound
      , OutOfKernelMemory
      , OutOfSpace
      , NotADirectory
      , FileSystemIOError
      , TempFileNotSupported
      , ReadOnlyFileSystem
      , CannotWriteExecutedImage
      , RetryLater
      , InvalidHandle
      , InvalidPathComponent
      ]

-- | Open and possibly create a file
open :: MonadInIO m => Maybe Handle -> FilePath -> HandleFlags -> FilePermissions -> Flow m (Handle ': OpenErrors)
open mhdl path flags mode = do
   let call = case mhdl of
                  Nothing          -> syscall_open
                  Just (Handle fd) -> syscall_openat fd
   withCString path
      (\path' -> liftIO (call path' (BitSet.toBits flags) (BitSet.toBits mode)))
      ||> toErrorCodePure (Handle . fromIntegral)
      >..%~^> \case
         EACCES       -> flowSet NotAllowed
         EDQUOT       -> flowSet ExhaustedQuota
         EEXIST       -> flowSet FileAlreadyExists
         EFAULT       -> flowSet MemoryError
         EFBIG        -> flowSet Overflow
         EINTR        -> flowSet Interrupted
         EINVAL       -> flowSet InvalidParam
         EISDIR       -> flowSet InvalidIsDirectory
         ELOOP        -> flowSet SymbolicLinkLoop
         EMFILE       -> flowSet TooManyProcessHandles
         ENAMETOOLONG -> flowSet TooLongPathName
         ENFILE       -> flowSet TooManySystemHandles
         ENODEV       -> flowSet DeviceNotFound
         ENOENT       -> flowSet InvalidPathComponent
         ENOMEM       -> flowSet OutOfKernelMemory
         ENOSPC       -> flowSet OutOfSpace
         ENOTDIR      -> flowSet NotADirectory
         ENXIO        -> flowSet FileSystemIOError
         EOPNOTSUPP   -> flowSet TempFileNotSupported
         EPERM        -> flowSet NotAllowed
         EROFS        -> flowSet ReadOnlyFileSystem
         ETXTBSY      -> flowSet CannotWriteExecutedImage
         EAGAIN       -> flowSet RetryLater
         EBADF        -> flowSet InvalidHandle
         err          -> unhdlErr "open" err


-- | Close a file descriptor
close :: MonadIO m => Handle -> Flow m '[(),InvalidHandle,Interrupted,FileSystemIOError]
close (Handle fd) =
   liftIO (syscall_close fd)
      ||> toErrorCodeVoid
      >..%~^> \case
         EBADF -> flowSet InvalidHandle
         EINTR -> flowSet Interrupted
         EIO   -> flowSet FileSystemIOError
         err   -> unhdlErr "close" err


-----------------------------------------------------------------------
-- Synchronization
-----------------------------------------------------------------------

-- | Causes all pending modifications to file system metadata and cached file
-- data to be written to the underlying filesystems
syncAll :: MonadIO m => m ()
syncAll = liftIO (syscall_sync)
   ||> toErrorCodeVoid
   >..~!!> unhdlErr "syncAll"

-- | Causes all pending modifications to file system metadata and cached file
-- data to be written to the underlying filesystem containg the open handle `fd`
syncAllByHandle :: MonadIO m => Handle -> Flow m '[(),InvalidHandle]
syncAllByHandle (Handle fd) = liftIO (syscall_syncfs fd)
   ||> toErrorCodeVoid
   >..%~^> \case
      EBADF -> flowSet InvalidHandle
      err   -> unhdlErr "syncAllByHandle" err

-- | Flushes all modified in-core of the file referred by the handle to the disk
-- device.
--
-- It does not necessarily ensure that the entry in the directory containing the
-- file has also reached disk. For that an explicit `syncHandle` on a handle for
-- the directory is also needed.
--
-- If the `flushMetadata` is False, only the contents of the file and the
-- metadata required to retrieve it (e.g., the file size) are flushed on disk.
-- Otherwise, all the metadata are flushed.
syncHandle :: MonadIO m => Bool -> Handle -> Flow m '[(),InvalidHandle,FileSystemIOError, InvalidParam]
syncHandle flushMetadata (Handle fd) =
      call
         ||> toErrorCodeVoid
         >..%~^> \case
            EBADF  -> flowSet InvalidHandle
            EIO    -> flowSet FileSystemIOError
            EROFS  -> flowSet InvalidParam
            EINVAL -> flowSet InvalidParam
            err    -> unhdlErr "syncHandle" err
   where
      call = if flushMetadata
               then liftIO (syscall_fsync fd)
               else liftIO (syscall_fdatasync fd)

-----------------------------------------------------------------------
-- Rename/move
-----------------------------------------------------------------------

type RenameErrors
   = '[ NotAllowed
      , BusyDirectory
      , ExhaustedQuota
      , InvalidParam
      , InvalidIsDirectory
      , SymbolicLinkLoop
      , TooManyLinks
      , TooLongPathName
      , InvalidPathComponent
      , OutOfKernelMemory
      , OutOfSpace
      , NotADirectory
      , NotEmptyDirectory
      , ReadOnlyFileSystem
      , NotTheSameFileSystem
      , InvalidHandle
      , FileAlreadyExists
      ]


-- | Change or exchange the name or location of a file
rename :: MonadInIO m => Maybe Handle -> FilePath -> Maybe Handle -> FilePath -> [RenameFlag] -> Flow m (() ': RenameErrors)
rename mohdl oldPath mnhdl newPath flags = do
   let
      flags'              = BitSet.fromList flags
      fromHdl (Handle fd) = fd
      mohdl'              = fromMaybe 0xFFFFFFFF (fmap fromHdl mohdl)
      mnhdl'              = fromMaybe 0xFFFFFFFF (fmap fromHdl mnhdl)
      noreplace           = BitSet.member flags' RenameNoReplace
   withCString oldPath $ \old' ->
      withCString newPath $ \new' ->
         liftIO (syscall_renameat2 mohdl' old' mnhdl' new'
                                      (BitSet.toBits flags'))
            ||> toErrorCodeVoid
            >..%~^> \case
               EACCES                   -> flowSet NotAllowed
               EBUSY                    -> flowSet BusyDirectory
               EDQUOT                   -> flowSet ExhaustedQuota
               EINVAL                   -> flowSet InvalidParam
               EISDIR                   -> flowSet InvalidIsDirectory
               ELOOP                    -> flowSet SymbolicLinkLoop
               EMLINK                   -> flowSet TooManyLinks
               ENAMETOOLONG             -> flowSet TooLongPathName
               ENOENT                   -> flowSet InvalidPathComponent
               ENOMEM                   -> flowSet OutOfKernelMemory
               ENOSPC                   -> flowSet OutOfSpace
               ENOTDIR                  -> flowSet NotADirectory
               ENOTEMPTY                -> flowSet NotEmptyDirectory
               EEXIST | noreplace       -> flowSet FileAlreadyExists
                      | otherwise       -> flowSet NotEmptyDirectory
               EPERM                    -> flowSet NotAllowed
               EROFS                    -> flowSet ReadOnlyFileSystem
               EXDEV                    -> flowSet NotTheSameFileSystem
               EBADF                    -> flowSet InvalidHandle
               err                      -> unhdlErr "rename" err




-----------------------------------------------------------------------
-- Device
-----------------------------------------------------------------------

-- | Device identifier
data DeviceID = DeviceID
   { deviceMajor :: !Word32 -- ^ Major
   , deviceMinor :: !Word32 -- ^ Minor
   } deriving (Show,Eq,Ord)

instance Storable DeviceID where
   sizeOf _     = 8
   alignment _  = alignmentT @Word64
   peekIO x     = fromKernelDevice <$> peek (castPtr x :: Ptr Word64)
   pokeIO ptr x = poke (castPtr ptr :: Ptr Word64) (toKernelDevice x)

-- | Convert a DeviceID into a Word64 suitable for the kernel
toKernelDevice :: DeviceID -> Word64
toKernelDevice dev =
      (minor .&. 0xFF) 
        .|. ((major .&. 0xfff) `shiftL` 8)
        .|. ((minor .&. complement 0xff) `shiftL` 12)
        .|. ((major .&. complement 0xfff) `shiftL` 32)
   where
      minor = fromIntegral (deviceMinor dev) :: Word64
      major = fromIntegral (deviceMajor dev) :: Word64

fromKernelDevice :: Word64 -> DeviceID
fromKernelDevice y = DeviceID
   { deviceMajor = fromIntegral $
                     ((y `shiftR` 8) .&. 0xFFF) .|.
                     ((y `shiftR` 32) .&. complement 0xFFF)
   , deviceMinor = fromIntegral $
                     (y .&. 0xFF) .|.
                     ((y `shiftR` 12) .&. complement 0xFF)
   }


-- | Use a DeviceID as a Word64 suitable for the kernel
withDeviceID :: DeviceID -> (Word64 -> a) -> a
withDeviceID dev f = f (toKernelDevice dev)
