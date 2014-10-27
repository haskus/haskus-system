module ViperVM.Arch.X86_64.Linux.FileSystem (
   FileDescriptor(..), FilePermission(..), OpenFlag(..), 
   SeekWhence(..), AccessMode(..), FileLock(..),
   sysRead, sysWrite,
   sysOpen, sysCreate, sysClose,
   sysSeek, sysReadAt, sysWriteAt,
   sysAccess, sysDup, sysDup2,
   sysSetCurrentDirectory, sysSetCurrentDirectoryPath,
   sysGetCurrentDirectory, sysRename, sysRemoveDirectory,
   sysFileLock, sysFileSync, sysFileDataSync,
   sysTruncate, sysTruncatePath, 
   sysLink, sysSymlink, sysUnlink,
   sysChangePermission, sysChangePermissionPath,
   sysChangeOwnership, sysChangeOwnershipPath, sysChangeLinkOwnershipPath,
   sysSetProcessUMask
) where

import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (allocaArray)
import Data.Word (Word,Word64)
import Foreign.C.String (CString, withCString, peekCString)
import Data.Int (Int64)
import Data.Bits (Bits, (.|.), (.&.))
import Data.Maybe (catMaybes)

import ViperVM.Arch.X86_64.Linux.Syscall
import ViperVM.Arch.X86_64.Linux.ErrorCode
import ViperVM.Arch.X86_64.Linux.Utils (toSet)
import ViperVM.Arch.X86_64.Linux.Process (UserID(..), GroupID(..))

-- | File descriptor
newtype FileDescriptor = FileDescriptor Word


-- | Read cound bytes from the given file descriptor and put them in "buf"
-- Returns the number of bytes read or 0 if end of file
sysRead :: FileDescriptor -> Ptr a -> Word64 -> SysRet Word64
sysRead (FileDescriptor fd) buf count =
   onSuccess (syscall3 0 fd buf count) fromIntegral


-- | Write cound bytes into the given file descriptor from "buf"
-- Returns the number of bytes written (0 indicates that nothing was written)
sysWrite :: FileDescriptor -> Ptr a -> Word64 -> SysRet Word64
sysWrite (FileDescriptor fd) buf count =
   onSuccess (syscall3 1 fd buf count) fromIntegral


sysOpenCString :: CString -> [OpenFlag] -> [FilePermission] -> SysRet FileDescriptor
sysOpenCString path flags mode =
   onSuccess (syscall3 2 path (toSet flags :: Int) (toSet mode :: Int))
      (FileDescriptor . fromIntegral)

-- | Open a file
sysOpen :: String -> [OpenFlag] -> [FilePermission] -> SysRet FileDescriptor
sysOpen path flags mode = withCString path (\path' -> sysOpenCString path' flags mode)

sysCreateCString :: CString -> [FilePermission] -> SysRet FileDescriptor
sysCreateCString path mode = 
   onSuccess (syscall2 85 path (toSet mode :: Int)) (FileDescriptor . fromIntegral)

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
data FilePermission =
     PermUserRead
   | PermUserWrite
   | PermUserExecute
   | PermGroupRead
   | PermGroupWrite
   | PermGroupExecute
   | PermOtherRead
   | PermOtherWrite
   | PermOtherExecute
   deriving (Show)

instance Enum FilePermission where
   fromEnum x = case x of
      PermUserRead      -> 0o400
      PermUserWrite     -> 0o200
      PermUserExecute   -> 0o100
      PermGroupRead     -> 0o040
      PermGroupWrite    -> 0o020
      PermGroupExecute  -> 0o010
      PermOtherRead     -> 0o004
      PermOtherWrite    -> 0o002
      PermOtherExecute  -> 0o001

   toEnum x = case x of
      0o400 -> PermUserRead
      0o200 -> PermUserWrite
      0o100 -> PermUserExecute
      0o040 -> PermGroupRead
      0o020 -> PermGroupWrite
      0o010 -> PermGroupExecute
      0o004 -> PermOtherRead
      0o002 -> PermOtherWrite
      0o001 -> PermOtherExecute
      _ -> error "Unrecognized file permission"

toPermission :: (Num a, Bits a) => a -> [FilePermission]
toPermission n = catMaybes (fmap f vs)
   where
      f (v,e) = if n .&. v /= 0 then Just e else Nothing
      vs = [
         (0o400, PermUserRead),
         (0o200, PermUserWrite),
         (0o100, PermUserExecute),
         (0o040, PermGroupRead),
         (0o020, PermGroupWrite),
         (0o010, PermGroupExecute),
         (0o004, PermOtherRead),
         (0o002, PermOtherWrite),
         (0o001, PermOtherExecute)]

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

-- | Read a file descriptor at a given position
sysReadAt :: FileDescriptor -> Ptr () -> Word64 -> Word64 -> SysRet Word64
sysReadAt (FileDescriptor fd) buf count offset =
   onSuccess (syscall4 17 fd buf count offset) fromIntegral

-- | Write a file descriptor at a given position
sysWriteAt :: FileDescriptor -> Ptr () -> Word64 -> Word64 -> SysRet Word64
sysWriteAt (FileDescriptor fd) buf count offset =
   onSuccess (syscall4 18 fd buf count offset) fromIntegral

data AccessMode = AccessExist | AccessRead | AccessWrite | AccessExecute

instance Enum AccessMode where
   fromEnum x = case x of
      AccessExist    -> 0
      AccessRead     -> 4
      AccessWrite    -> 2
      AccessExecute  -> 1

   toEnum = undefined

sysAccess :: FilePath -> [AccessMode] -> SysRet ()
sysAccess path mode = withCString path $ \path' ->
   onSuccess (syscall2 21 path' (toSet mode :: Int64)) (const ())


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
   onSuccess (syscall2 90 path' (toSet mode :: Int)) (const ())

sysChangePermission :: FileDescriptor -> [FilePermission] -> SysRet ()
sysChangePermission (FileDescriptor fd) mode = 
   onSuccess (syscall2 91 fd (toSet mode :: Int)) (const ())


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
   onSuccess (syscall1 95 (toSet mode :: Int)) toPermission
