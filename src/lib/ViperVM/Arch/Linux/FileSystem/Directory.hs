-- | Directory
module ViperVM.Arch.Linux.FileSystem.Directory
   ( listDirectory
   )
where

import ViperVM.Arch.X86_64.Linux.FileSystem.Directory
import ViperVM.Arch.X86_64.Linux.FileSystem (sysSeek', SeekWhence(..))
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor

import Control.Monad.Trans.Either

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

