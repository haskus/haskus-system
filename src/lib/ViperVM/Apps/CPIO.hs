-- | CPIO encoder/decoder app
module ViperVM.Apps.CPIO
   ( archiveFiles
   )
where

import ViperVM.Format.CPIO
import ViperVM.Format.Binary.Buffer
import ViperVM.Format.Binary.Put
import ViperVM.Format.Binary.BitSet as BitSet
import qualified ViperVM.Format.Text as Text
import ViperVM.Arch.Linux.FileSystem

import System.FilePath
import Control.Monad

-- | Archive several files in a CPIO archive using dummy metadata
--
-- TODO:
--    * handle hard links
--    * handle directories
--    * use real inode, dev, UID, GID and modiftime
--    * don't load all the files at once in memory
archiveFiles :: FilePath -> [FilePath] -> IO ()
archiveFiles dest files = do

   -- generate headers
   let 
      makeHdr n = FileDesc
         { fileInode     = n
         , fileMode      = fmod
         , fileUID       = 0
         , fileGID       = 0
         , fileNLink     = 1
         , fileModifTime = 0
         , fileDevMajor  = 0
         , fileDevMinor  = 0
         , fileRDevMajor = 0
         , fileRDevMinor = 0
         }
      fmod  = makeMode FileTypeFile (BitSet.fromList [PermUserRead]) BitSet.empty
      hds   = fmap makeHdr [1..]
      names = fmap (Text.pack . takeFileName) files

   -- read files
   bufs <- forM files bufferReadFile

   -- write output file
   let out = runPut $ putFiles (zip3 hds names bufs)
   bufferWriteFile dest out
