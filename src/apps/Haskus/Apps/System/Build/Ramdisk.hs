module Haskus.Apps.System.Build.Ramdisk
   ( ramdiskMain
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
   workDir <- getWorkDir
   let
      rd = workDir </> Text.unpack (ramdiskFileName config)
      rdinit = Text.unpack (ramdiskInit config)

   binfp <- stackGetBinPath rdinit

   withSystemTempDirectory "haskus-system-build" $ \tmpfp -> do
      showStep "Building ramdisk..."
      -- copy ramdisk files
      copyFile binfp (tmpfp </> rdinit)

      -- create ramdisk
      -- TODO: use our own `cpio`
      shellInErr tmpfp
         ("(find . | cpio -o -H newc | gzip) > " ++ rd)
            $ failWith "Cannot build ramdisk"
