{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Directory
module ViperVM.Arch.Linux.FileSystem.Directory
   ( sysGetDirectoryEntries
   , sysCreateDirectory
   , sysRemoveDirectory
   , DirectoryEntry(..)
   , DirectoryEntryHeader(..)
   , listDirectory
   )
where

import Control.Monad.Trans.Either
import Foreign.Storable
import Foreign.CStorable
import GHC.Generics (Generic)
import Data.Word
import Data.Int
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.C.String

import qualified ViperVM.Utils.BitSet as BitSet

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Arch.Linux.FileSystem

sysCreateDirectory :: Maybe FileDescriptor -> FilePath -> FilePermissions -> Bool -> SysRet ()
sysCreateDirectory fd path perm sticky = do
   let
      opt = if sticky then BitSet.fromList [FileOptSticky] else BitSet.empty
      mode = makeMode perm opt

   withCString path $ \path' ->
      case fd of
         Nothing -> onSuccess (syscall_mkdir path' mode) (const ())
         Just (FileDescriptor fd') -> onSuccess (syscall_mkdirat fd' path' mode) (const ())


sysRemoveDirectory :: FilePath -> SysRet ()
sysRemoveDirectory path = withCString path $ \path' ->
   onSuccess (syscall_rmdir path') (const ())


data DirectoryEntryHeader = DirectoryEntryHeader
   { dirInod      :: Word64   -- ^ Inode number
   , dirOffset    :: Int64    -- ^ Offset of the next entry
   , dirLength    :: Word16   -- ^ Length of the entry
   , dirFileTyp   :: Word8    -- ^ Type of file
   } deriving (Generic)

instance CStorable DirectoryEntryHeader

instance Storable DirectoryEntryHeader where
   peek      = cPeek
   poke      = cPoke
   sizeOf    = cSizeOf
   alignment = cAlignment

data DirectoryEntry = DirectoryEntry
   { entryInode :: Word64
   , entryType  :: Word8
   , entryName  :: FilePath
   } deriving (Show)

-- | getdents64 syscall
--
-- Linux doesn't provide a stateless API: the offset in the file (i.e. the
-- iterator in the directory contents) is shared by everyone using the file
-- descriptor...
--
-- TODO: propose a "pgetdents64" syscall for Linux with an additional offset
-- (like pread, pwrite)
sysGetDirectoryEntries :: FileDescriptor -> Int -> SysRet [DirectoryEntry]
sysGetDirectoryEntries (FileDescriptor fd) buffersize = do

   let
      readEntries p n
         | n < sizeOf (undefined :: DirectoryEntryHeader) = return []
         | otherwise = do
               hdr  <- peek p
               let 
                  len     = fromIntegral (dirLength hdr)
                  sizede  = sizeOf (undefined :: DirectoryEntryHeader)
                  namepos = p `plusPtr` sizede
                  nextpos = p `plusPtr` len
                  nextlen = n - len
               name <- peekCString (castPtr namepos)
               let x = DirectoryEntry (dirInod hdr) (dirFileTyp hdr) name
               xs <- readEntries nextpos nextlen
               -- filter deleted files
               if dirInod hdr /= 0
                  then return (x:xs)
                  else return xs

   allocaArray buffersize $ \(ptr :: Ptr Word8) -> do
      onSuccessIO (syscall_getdents64 fd ptr (fromIntegral buffersize)) $ \nread -> 
         readEntries (castPtr ptr) (fromIntegral nread)

-- | Return the content of a directory
--
-- Warning: reading concurrently the same file descriptor returns mixed up
-- results because of the stateful kernel interface
listDirectory :: FileDescriptor -> SysRet [DirectoryEntry]
listDirectory fd = runEitherT $ do
      -- Return at the beginning of the directory
      EitherT $ sysSeek' fd 0 SeekSet
      -- Read contents using a given buffer size
      -- If another thread changes the current position in the directory file
      -- descriptor, the returned list can be corrupted (redundant entries or
      -- missing ones)
      EitherT $ rec []
   where
      bufferSize = 2 * 1024 * 1024

      rec xs = do
         ls <- sysGetDirectoryEntries fd bufferSize
         case ls of
            Left err -> return (Left err)
            Right [] -> return (Right xs)
            Right ks -> rec (xs ++ ks)

