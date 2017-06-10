module Haskus.Apps.System.Build.Disk
   ( withDisk
   , makeDisk
   )
where

import Haskus.Apps.System.Build.Config
import Haskus.Apps.System.Build.Utils
import Haskus.Apps.System.Build.Ramdisk
import Haskus.Apps.System.Build.Linux
import Haskus.Apps.System.Build.Syslinux

import System.IO.Temp
import System.FilePath
import System.Directory
import qualified Data.Text as Text
import qualified Data.Text.IO as Text


-- | Create a temp directory containing the system and call the callback
withDisk :: SystemConfig -> (FilePath -> IO a) -> IO a
withDisk config callback = do

   withSystemTempDirectory "haskus-system-build" $ \tmpfp -> do
      -- create the disk
      makeDisk config tmpfp

      -- call the callback
      callback tmpfp

-- | Create a disk in the given folder
makeDisk :: SystemConfig -> FilePath -> IO ()
makeDisk config tmpfp = do
   showStep "Creating system disk..."

   -- create directories
   let syslinuxfp = tmpfp </> "boot" </> "syslinux"
   createDirectoryIfMissing True syslinuxfp

   -- copy Syslinux
   syslinuxPath <- syslinuxMain (syslinuxConfig config)
   -- copy *.c32 files
   copyDirectory (syslinuxPath </> "bios") syslinuxfp True
      (return . (== ".c32") . takeExtension)
   -- copy isolinux.bin
   copyFile (syslinuxPath </> "bios" </> "core" </> "isolinux.bin")
            (syslinuxfp </> "isolinux.bin")

   -- copy linux
   srcLinuxFile <- linuxKernelFile (linuxConfig config)
   let
      kernelPath   = "boot" </> takeFileName srcLinuxFile
      linuxFile    = tmpfp </> kernelPath
   copyFile srcLinuxFile linuxFile

   -- copy the ramdisk
   srcRamdiskFile <- ramdiskGetPath (ramdiskConfig config)
   let
      ramdiskPath    = "boot" </> takeFileName srcRamdiskFile
      ramdiskFile    = tmpfp </> ramdiskPath
   copyFile srcRamdiskFile ramdiskFile

   -- configure Syslinux
   let 
      cfg     = syslinuxConfigFile config (Text.pack kernelPath)
                                          (Text.pack ramdiskPath)
      cfgPath = syslinuxfp </> "syslinux.cfg"
   Text.writeFile cfgPath cfg

