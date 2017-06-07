module Haskus.Apps.System.Build.Ramdisk
   ( ramdiskMain
   , ramdiskGetPath
   )
where

import Haskus.Apps.System.Build.Config
import Haskus.Apps.System.Build.Utils
import Haskus.Apps.System.Build.Stack

import System.IO.Temp
import System.FilePath
import System.Directory
import qualified Data.Text as Text

ramdiskMain :: RamdiskConfig -> IO ()
ramdiskMain config = do
   rd <- ramdiskGetPath config
   let
      rdinit = Text.unpack (ramdiskInit config)

   binfp <- stackGetBinPath rdinit

   withSystemTempDirectory "haskus-system-build" $ \tmpfp -> do
      showStep "Building ramdisk..."
      -- copy ramdisk files
      copyFile binfp (tmpfp </> rdinit)

      -- create ramdisk
      -- TODO: use our own `cpio` and `gzip`
      shellInErr tmpfp
         ("(find . | cpio -o -H newc | gzip) > " ++ rd)
            $ failWith "Cannot build ramdisk"


-- | Get ramdisk
ramdiskGetPath :: RamdiskConfig -> IO FilePath
ramdiskGetPath config = do
   workDir <- getWorkDir
   let rdDir  = workDir </> "ramdisk"
   createDirectoryIfMissing True rdDir
   return (rdDir </> Text.unpack (ramdiskFileName config))
