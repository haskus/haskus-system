{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ViperVM.Arch.Linux.FileSystem
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
   , sysOpen
   , sysOpenAt
   , sysCreate
   , sysClose
   , sysSeek
   , sysSeek'
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
   , sysSync
   , sysSyncFS
   , sysCreateSpecialFile
   -- * Device
   , DeviceID (..)
   , withDeviceID
   )
where

import Foreign.Marshal.Array (allocaArray)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (Storable, peek, poke, sizeOf, alignment)
import Foreign.CStorable
import GHC.Generics (Generic)

import ViperVM.Format.Binary.Bits
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Ptr (Ptr, castPtr)
import ViperVM.Format.Binary.BitSet
import ViperVM.Format.String
import qualified ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Utils.Flow
import ViperVM.Utils.Maybe

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Arch.Linux.Time (TimeSpec)
import ViperVM.Arch.Linux.Process (UserID(..), GroupID(..))
import ViperVM.Arch.Linux.Internals.FileSystem

-- | Open a file
sysOpen :: FilePath -> HandleFlags -> FilePermissions -> SysRet Handle
sysOpen path flags mode = 
   withCString path $ \path' -> 
      onSuccess (syscall_open path' (BitSet.toBits flags) (BitSet.toBits mode))
         (Handle . fromIntegral)

-- | Open a file
sysOpenAt :: Handle -> FilePath -> HandleFlags -> FilePermissions -> SysRet Handle
sysOpenAt (Handle fd) path flags mode = 
   withCString path $ \path' -> 
      onSuccess (syscall_openat fd path' (BitSet.toBits flags) (BitSet.toBits mode))
         (Handle . fromIntegral)

sysCreateCString :: CString -> FilePermissions -> SysRet Handle
sysCreateCString path mode = 
   onSuccess (syscall_creat path (BitSet.toBits mode)) (Handle . fromIntegral)

sysCreate :: String -> FilePermissions -> SysRet Handle
sysCreate path mode = withCString path $ \path' -> sysCreateCString path' mode

-- | Close a file descriptor
sysClose :: Handle -> SysRet ()
sysClose (Handle fd) =
   onSuccess (syscall_close fd) (const ())


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
sysSeek :: Handle -> Int64 -> SeekWhence -> SysRet Int64
sysSeek (Handle fd) off whence =
   onSuccess (syscall_lseek fd off (fromEnum whence)) id

-- | Reposition read/write file offset
sysSeek' :: Handle -> Int64 -> SeekWhence -> SysRet ()
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

sysAccess :: FilePath -> AccessModes -> SysRet ()
sysAccess path mode = withCString path $ \path' ->
   onSuccess (syscall_access path' (BitSet.toBits mode)) (const ())


sysDup :: Handle -> SysRet Handle
sysDup (Handle oldfd) = 
   onSuccess (syscall_dup oldfd) (Handle . fromIntegral)

sysDup2 :: Handle -> Handle -> SysRet Handle
sysDup2 (Handle oldfd) (Handle newfd) = 
   onSuccess (syscall_dup2 oldfd newfd) (Handle . fromIntegral)

sysSetCurrentDirectoryPath :: FilePath -> SysRet ()
sysSetCurrentDirectoryPath path = withCString path $ \path' ->
   onSuccess (syscall_chdir path') (const ())

sysSetCurrentDirectory :: Handle -> SysRet ()
sysSetCurrentDirectory (Handle fd) = 
   onSuccess (syscall_fchdir fd) (const ())

sysGetCurrentDirectory :: SysRet FilePath
sysGetCurrentDirectory = go 128
   where
      go n = allocaArray n $ \ptr -> do
         onSuccessId (syscall_getcwd ptr (fromIntegral n))
            >.~.> const (peekCString ptr)
            >%~^> \case
               ERANGE -> go (2 * n)
               e      -> flowSet e

sysRename :: FilePath -> FilePath -> SysRet ()
sysRename oldPath newPath =
   withCString oldPath $ \old' ->
      withCString newPath $ \new' ->
         onSuccess (syscall_rename old' new') (const ())

data FileLock =
     SharedLock
   | ExclusiveLock
   | RemoveLock

sysFileLock :: Handle -> FileLock -> Bool -> SysRet ()
sysFileLock (Handle fd) mode nonBlocking = do
   let
      mode' = case mode of
         SharedLock     -> 1
         ExclusiveLock  -> 2
         RemoveLock     -> 8

      nb = if nonBlocking then 4 else 0

   onSuccess (syscall_flock fd (mode' .|. nb :: Int64)) (const ())


sysFileSync :: Handle -> SysRet ()
sysFileSync (Handle fd) = onSuccess (syscall_fsync fd) (const ())

sysFileDataSync :: Handle -> SysRet ()
sysFileDataSync (Handle fd) = onSuccess (syscall_fdatasync fd) (const ())

sysTruncatePath :: FilePath -> Word64 -> SysRet ()
sysTruncatePath path size = withCString path $ \path' ->
   onSuccess (syscall_truncate path' size) (const ())

sysTruncate :: Handle -> Word64 -> SysRet ()
sysTruncate (Handle fd) size =
   onSuccess (syscall_ftruncate fd size) (const ())

sysLink :: FilePath -> FilePath -> SysRet ()
sysLink src dest =
   withCString src $ \src' ->
      withCString dest $ \dest' ->
         onSuccess (syscall_link src' dest') (const ())

sysUnlink :: FilePath -> SysRet ()
sysUnlink path = withCString path $ \path' ->
   onSuccess (syscall_unlink path') (const ())

sysUnlinkAt :: Handle -> FilePath -> Bool -> SysRet ()
sysUnlinkAt (Handle fd) path rmdir = withCString path $ \path' ->
   onSuccess (syscall_unlinkat fd path' (if rmdir then 0x200 else 0)) (const ())



sysChangePermissionPath :: FilePath -> FilePermissions -> SysRet ()
sysChangePermissionPath path mode = withCString path $ \path' ->
   onSuccess (syscall_chmod path' (BitSet.toBits mode)) (const ())

sysChangePermission :: Handle -> FilePermissions -> SysRet ()
sysChangePermission (Handle fd) mode = 
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
sysChangeOwnership :: Handle -> Maybe UserID -> Maybe GroupID -> SysRet ()
sysChangeOwnership (Handle fd) = chownEx syscall_fchown fd

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
   { statDevice'           :: StorableWrap DeviceID
   , statInode'            :: Word64
   , statLinkCount'        :: Word64
   , statMode'             :: Word32
   , statUID'              :: Word32
   , statGID'              :: Word32
   , statPad0'             :: Word32
   , statDevNum'           :: StorableWrap DeviceID
   , statSize'             :: Int64
   , statBlockSize'        :: Int64
   , statBlockCount'       :: Int64
   , statLastAccess'       :: TimeSpec
   , statLastModif'        :: TimeSpec
   , statLastStatusChange' :: TimeSpec
   } deriving (Generic,CStorable)

instance Storable StatStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

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
sysHandleStat :: Handle -> SysRet Stat
sysHandleStat (Handle fd) =
   allocaBytes (sizeOf (undefined :: StatStruct)) $ \s ->
      onSuccessIO (syscall_fstat fd s) (const (toStat <$> peek s))


sysSync :: SysRet ()
sysSync = onSuccess syscall_sync (const ())

sysSyncFS :: Handle -> SysRet ()
sysSyncFS (Handle fd) = onSuccess (syscall_syncfs fd) (const ())

-- | Create a special file
--
-- mknodat syscall. 
sysCreateSpecialFile :: Maybe Handle -> FilePath -> FileType -> FilePermissions -> Maybe DeviceID -> SysRet ()
sysCreateSpecialFile hdl path typ perm dev = do
   let 
      mode = fromIntegral (toBits perm) .|. fromFileType typ :: Word64
      dev' = fromMaybe (DeviceID 0 0) dev
      -- We pass a dummy file descriptor if the handle is not required
      fd   = case hdl of
                  Just (Handle x) -> x
                  Nothing         -> (-1)
   withCString path $ \path' ->
      withDeviceID dev' $ \dev'' ->
         onSuccessVoid (syscall_mknodat fd path' mode dev'')

-- | Device identifier
data DeviceID = DeviceID
   { deviceMajor :: !Word32 -- ^ Major
   , deviceMinor :: !Word32 -- ^ Minor
   } deriving (Show,Eq,Ord)

instance Storable DeviceID where
   sizeOf _    = 8
   alignment _ = alignment (undefined :: Word64)
   peek x      = fromKernelDevice <$> peek (castPtr x :: Ptr Word64)
   poke ptr x  = poke (castPtr ptr :: Ptr Word64) (toKernelDevice x)

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

