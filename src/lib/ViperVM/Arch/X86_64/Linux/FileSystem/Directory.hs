{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ViperVM.Arch.X86_64.Linux.FileSystem.Directory
   ( sysGetDirectoryEntries
   , sysCreateDirectory
   , sysRemoveDirectory
   , DirectoryEntry(..)
   , DirectoryEntryHeader(..)
   )
where

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
import ViperVM.Arch.X86_64.Linux.Syscall
import ViperVM.Arch.X86_64.Linux.FileSystem

sysCreateDirectory :: Maybe FileDescriptor -> FilePath -> FilePermissions -> Bool -> SysRet ()
sysCreateDirectory fd path perm sticky = do
   let
      opt = if sticky then BitSet.fromList [FileOptSticky] else BitSet.empty
      mode = makeMode perm opt

   withCString path $ \path' ->
      case fd of
         Nothing -> onSuccess (syscall2 83 path' mode) (const ())
         Just (FileDescriptor fd') -> onSuccess (syscall3 258 fd' path' mode) (const ())


sysRemoveDirectory :: FilePath -> SysRet ()
sysRemoveDirectory path = withCString path $ \path' ->
   onSuccess (syscall1 84 path') (const ())


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
      onSuccessIO (syscall3 217 fd ptr buffersize) $ \nread -> 
         readEntries (castPtr ptr) (fromIntegral nread)
