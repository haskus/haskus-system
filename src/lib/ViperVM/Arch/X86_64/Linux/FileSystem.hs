{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module ViperVM.Arch.X86_64.Linux.FileSystem
   ( FilePermission(..)
   , FileType(..)
   , FileOption(..)
   , OpenFlag(..)
   , SeekWhence(..)
   , AccessMode(..)
   , FileLock(..)
   , Device(..)
   , Stat(..)
   , sysOpen
   , sysCreate
   , sysClose
   , sysSeek
   , sysIoctl
   , sysAccess
   , sysDup
   , sysDup2
   , sysSetCurrentDirectory
   , sysSetCurrentDirectoryPath
   , sysGetCurrentDirectory
   , sysRename
   , sysRemoveDirectory
   , sysFileLock
   , sysFileSync
   , sysFileDataSync
   , sysTruncate
   , sysTruncatePath
   , sysLink
   , sysSymlink
   , sysUnlink
   , sysChangePermission
   , sysChangePermissionPath
   , sysChangeOwnership
   , sysChangeOwnershipPath
   , sysChangeLinkOwnershipPath
   , sysSetProcessUMask
   , sysFileStat
   , sysFileDescriptorStat
   , sysCreateDirectory
   , sysSync
   , sysSyncFS
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
import Data.Bits (Bits, (.|.), (.&.), shiftR, shiftL, complement)
import Control.Applicative ((<$>))

import GHC.Generics (Generic)

import ViperVM.Utils.EnumSet
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.X86_64.Linux.Syscall
import ViperVM.Arch.X86_64.Linux.Utils (toSet)
import ViperVM.Arch.X86_64.Linux.Time (TimeSpec)
import ViperVM.Arch.X86_64.Linux.Process (UserID(..), GroupID(..))


sysOpenCString :: CString -> [OpenFlag] -> [FilePermission] -> SysRet FileDescriptor
sysOpenCString path flags mode =
   onSuccess (syscall3 2 path (toSet flags :: Int) (toBitSet mode :: Int))
      (FileDescriptor . fromIntegral)

-- | Open a file
sysOpen :: String -> [OpenFlag] -> [FilePermission] -> SysRet FileDescriptor
sysOpen path flags mode = withCString path (\path' -> sysOpenCString path' flags mode)

sysCreateCString :: CString -> [FilePermission] -> SysRet FileDescriptor
sysCreateCString path mode = 
   onSuccess (syscall2 85 path (toBitSet mode :: Int)) (FileDescriptor . fromIntegral)

sysCreate :: String -> [FilePermission] -> SysRet FileDescriptor
sysCreate path mode = withCString path $ \path' -> sysCreateCString path' mode

-- | Close a file descriptor
sysClose :: FileDescriptor -> SysRet ()
sysClose (FileDescriptor fd) =
   onSuccess (syscall1 3 fd) (const ())


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

data SeekWhence = 
     SeekSet 
   | SeekCurrent 
   | SeekEnd 
   | SeekData
   | SeekHole
   deriving (Enum,Eq,Show)

-- | Reposition read/write file offset
sysSeek :: FileDescriptor -> Int64 -> SeekWhence -> SysRet Int64
sysSeek (FileDescriptor fd) off whence =
   onSuccess (syscall3 8 fd off (fromEnum whence)) id


-- | Send a custom command to a device
sysIoctl :: FileDescriptor -> Int64 -> Int64 -> IO Int64
sysIoctl (FileDescriptor fd) cmd arg =
   syscall3 16 fd cmd arg


-- | Access mode
--
-- To test if a file exists, use no flag
data AccessMode
   = AccessExecute  -- bit 0
   | AccessWrite    -- bit 1
   | AccessRead     -- bit 2
   deriving (Eq,Show,Enum)

instance EnumBitSet AccessMode

sysAccess :: FilePath -> [AccessMode] -> SysRet ()
sysAccess path mode = withCString path $ \path' ->
   onSuccess (syscall2 21 path' (toBitSet mode :: Int64)) (const ())


sysDup :: FileDescriptor -> SysRet FileDescriptor
sysDup (FileDescriptor oldfd) = 
   onSuccess (syscall1 32 oldfd) (FileDescriptor . fromIntegral)

sysDup2 :: FileDescriptor -> FileDescriptor -> SysRet FileDescriptor
sysDup2 (FileDescriptor oldfd) (FileDescriptor newfd) = 
   onSuccess (syscall2 33 oldfd newfd) (FileDescriptor . fromIntegral)

sysSetCurrentDirectoryPath :: FilePath -> SysRet ()
sysSetCurrentDirectoryPath path = withCString path $ \path' ->
   onSuccess (syscall1 80 path') (const ())

sysSetCurrentDirectory :: FileDescriptor -> SysRet ()
sysSetCurrentDirectory (FileDescriptor fd) = 
   onSuccess (syscall1 81 fd) (const ())

sysGetCurrentDirectory :: SysRet FilePath
sysGetCurrentDirectory = go 128
   where
      go n = allocaArray n $ \ptr -> do
         ret <- onSuccessIO (syscall2 79 (ptr :: CString) n) (const (peekCString ptr))
         case ret of
            Left ERANGE -> go (2 * n)
            _ -> return ret

sysRename :: FilePath -> FilePath -> SysRet ()
sysRename oldPath newPath =
   withCString oldPath $ \old' ->
      withCString newPath $ \new' ->
         onSuccess (syscall2 82 old' new') (const ())

sysRemoveDirectory :: FilePath -> SysRet ()
sysRemoveDirectory path = withCString path $ \path' ->
   onSuccess (syscall1 84 path') (const ())

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

   onSuccess (syscall2 73 fd (mode' .|. nb :: Int64)) (const ())


sysFileSync :: FileDescriptor -> SysRet ()
sysFileSync (FileDescriptor fd) = onSuccess (syscall1 74 fd) (const ())

sysFileDataSync :: FileDescriptor -> SysRet ()
sysFileDataSync (FileDescriptor fd) = onSuccess (syscall1 75 fd) (const ())

sysTruncatePath :: FilePath -> Word64 -> SysRet ()
sysTruncatePath path size = withCString path $ \path' ->
   onSuccess (syscall2 76 path' size) (const ())

sysTruncate :: FileDescriptor -> Word64 -> SysRet ()
sysTruncate (FileDescriptor fd) size =
   onSuccess (syscall2 77 fd size) (const ())

sysLink :: FilePath -> FilePath -> SysRet ()
sysLink src dest =
   withCString src $ \src' ->
      withCString dest $ \dest' ->
         onSuccess (syscall2 86 src' dest') (const ())

sysSymlink :: FilePath -> FilePath -> SysRet ()
sysSymlink src dest =
   withCString src $ \src' ->
      withCString dest $ \dest' ->
         onSuccess (syscall2 88 src' dest') (const ())

sysUnlink :: FilePath -> SysRet ()
sysUnlink path = withCString path $ \path' ->
   onSuccess (syscall1 87 path') (const ())

sysChangePermissionPath :: FilePath -> [FilePermission] -> SysRet ()
sysChangePermissionPath path mode = withCString path $ \path' ->
   onSuccess (syscall2 90 path' (toBitSet mode :: Int)) (const ())

sysChangePermission :: FileDescriptor -> [FilePermission] -> SysRet ()
sysChangePermission (FileDescriptor fd) mode = 
   onSuccess (syscall2 91 fd (toBitSet mode :: Int)) (const ())


-- | Avoid duplication in *chown syscalls
chownEx :: Arg x => Int64 -> x -> Maybe UserID -> Maybe GroupID -> SysRet ()
chownEx n a uid gid = do
   let
      fuid (UserID x) = x
      fgid (GroupID x) = x
      uid' = maybe (-1) fuid uid
      gid' = maybe (-1) fgid gid
   onSuccess (syscall3 n a uid' gid') (const ())


-- | chown
sysChangeOwnershipPath :: FilePath -> Maybe UserID -> Maybe GroupID -> SysRet ()
sysChangeOwnershipPath path uid gid = withCString path (\p -> chownEx 92 p uid gid)

-- | lchown
sysChangeLinkOwnershipPath :: FilePath -> Maybe UserID -> Maybe GroupID -> SysRet ()
sysChangeLinkOwnershipPath path uid gid = withCString path (\p -> chownEx 94 p uid gid)

-- | fchown
sysChangeOwnership :: FileDescriptor -> Maybe UserID -> Maybe GroupID -> SysRet ()
sysChangeOwnership (FileDescriptor fd) = chownEx 93 fd

-- | umask
sysSetProcessUMask :: [FilePermission] -> SysRet [FilePermission]
sysSetProcessUMask mode =
   onSuccess (syscall1 95 (toBitSet mode :: Int)) fromBitSet

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
toFileType :: (Num a, Bits a, Integral a) => a -> FileType
toFileType x = toEnum (fromIntegral ((x `shiftR` 12) .&. 0x0F))

-- | File options
data FileOption
   = FileOptSticky
   | FileOptSetGID
   | FileOptSetUID
   deriving (Show,Eq,Enum)

instance EnumBitSet FileOption

-- | Read file options from Stat "mode" field 
toFileOptions :: (Num a, Bits a) => a -> [FileOption]
toFileOptions x = fromBitSet ((x `shiftR` 9) .&. 0x07)

fromFileOptions :: (Num a, Bits a) => [FileOption] -> a
fromFileOptions x = toBitSet x `shiftL` 9

-- | Read file permission from Stat "mode" field 
toFilePermission :: (Num a, Bits a) => a -> [FilePermission]
toFilePermission x = fromBitSet (x .&. 0x01FF)

fromFilePermission :: (Num a, Bits a) => [FilePermission] -> a
fromFilePermission x = toBitSet x

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
   poke ptr x = poke (castPtr ptr :: Ptr Word64) (f x)
      where
         f (Device major' minor') =
            let
               minor = fromIntegral minor' :: Word64
               major = fromIntegral major' :: Word64
            in
            (minor .&. 0xFF) 
              .|. ((major .&. 0xfff) `shiftL` 8)
              .|. ((minor .&. complement 0xff) `shiftL` 12)
              .|. ((major .&. complement 0xfff) `shiftL` 32)

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
   , statFileOptions       :: [FileOption]
   , statFilePermissions   :: [FilePermission]
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
      , statFileType          = toFileType statMode'
      , statFileOptions       = toFileOptions statMode'
      , statFilePermissions   = toFilePermission statMode'
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
sysFileStat :: String -> Bool -> SysRet Stat
sysFileStat path followLink = do
   putStrLn $ "Size of stat: " ++ show (cSizeOf (undefined :: StatStruct))
   withCString path $ \path' ->
      allocaBytes (sizeOf (undefined :: StatStruct)) $ \s ->
         let
            -- select between stat and lstat syscalls
            code = if followLink then 4 else 6
         in
         onSuccessIO (syscall2 code path' s) (const (toStat <$> peek s))

-- | Stat on file descriptor
sysFileDescriptorStat :: FileDescriptor -> SysRet Stat
sysFileDescriptorStat (FileDescriptor fd) =
   allocaBytes (sizeOf (undefined :: StatStruct)) $ \s ->
      onSuccessIO (syscall2 5 fd s) (const (toStat <$> peek s))


sysCreateDirectory :: Maybe FileDescriptor -> String -> [FilePermission] -> Bool -> SysRet ()
sysCreateDirectory fd path perm sticky = do
   let
      opt = if sticky then [FileOptSticky] else []
      mode = fromFilePermission perm .|. fromFileOptions opt :: Word64

   withCString path $ \path' ->
      case fd of
         Nothing -> onSuccess (syscall2 83 path' mode) (const ())
         Just (FileDescriptor fd') -> onSuccess (syscall3 258 fd' path' mode) (const ())


sysSync :: SysRet ()
sysSync = onSuccess (syscall0 162) (const ())

sysSyncFS :: FileDescriptor -> SysRet ()
sysSyncFS (FileDescriptor fd) = onSuccess (syscall1 306 fd) (const ())
