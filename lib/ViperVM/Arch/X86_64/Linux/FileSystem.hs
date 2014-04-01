module ViperVM.Arch.X86_64.Linux.FileSystem (
   FileDescriptor(..), FilePermission(..), OpenFlag(..), 
   sysRead, sysWrite,
   sysOpen, sysClose,
   SeekWhence(..), sysSeek, sysReadAt, sysWriteAt
) where

import Foreign.Ptr (Ptr)
import Data.Word (Word,Word64)
import Foreign.C.String (CString, withCString)
import Data.Int (Int64)

import ViperVM.Arch.X86_64.Linux.Syscall
import ViperVM.Arch.X86_64.Linux.ErrorCode
import ViperVM.Arch.X86_64.Linux.Utils (toSet)

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
