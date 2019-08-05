-- | CPIO encoder/decoder app
module Haskus.Apps.CPIO
   ( archiveFiles
   )
where

import Haskus.Format.CPIO
import Haskus.Binary.Buffer
import Haskus.Binary.Put
import Haskus.Binary.BitSet as BitSet
import qualified Haskus.Utils.Text as Text
import Haskus.System.Linux.FileSystem
import Haskus.Utils.Flow

import System.FilePath

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
