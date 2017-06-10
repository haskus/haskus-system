module Haskus.Apps.System.Build.Disk
   ( withDisk
   , makeDisk
   , makeDevice
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
import Control.Exception (finally)


-- | Create a temp directory containing the system and call the callback
withDisk :: SystemConfig -> (FilePath -> IO a) -> IO a
withDisk config callback = do

   withSystemTempDirectory "haskus-system-build" $ \tmpfp -> do
      -- create the disk
      makeDisk config tmpfp

      -- call the callback
      callback tmpfp

-- | Mount a device and install a system in it
makeDevice :: SystemConfig -> FilePath -> IO ()
makeDevice config dev = do
   -- TODO: allow the selection of another boot partition
   -- TODO: ensure that the partition is bootable
   -- TODO: check filesystem 
   let dev' = dev ++ "1"
   showStep $ "Installing in partition " ++ dev' ++"..."
   withDisk config $ \disk -> do
      withSystemTempDirectory "haskus-system-build" $ \tmpfp -> do
         shellWaitErr ("sudo mount "++dev'++" "++tmpfp++" -o rw")
            $ failWith "Unable to mount device"
         (do
            shellWaitErr ("sudo cp -r " ++ disk ++"/* "++tmpfp)
               (failWith "Cannot copy files on the mounted device")
            syslinuxInstall (syslinuxConfig config) dev tmpfp
            ) `finally`
               shellWaitErr ("sudo umount "++tmpfp)
                  (failWith "Unable to umount device")
      


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

