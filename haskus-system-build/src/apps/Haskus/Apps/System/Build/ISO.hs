module Haskus.Apps.System.Build.ISO
   ( isoMake
   )
where

import Haskus.Apps.System.Build.Config
import Haskus.Apps.System.Build.Utils
import Haskus.Apps.System.Build.Disk
import Haskus.Apps.System.Build.Syslinux

import System.FilePath
import System.Directory
import qualified Data.Text as Text
import qualified Data.List as List


isoMake :: SystemConfig -> IO FilePath
isoMake config = do
   
   wd <- getWorkDir
   let
      isoPath = wd </> "iso"
      isoFile = isoPath </> Text.unpack (ramdiskInit (ramdiskConfig config)) <.> "iso"

   -- get hybrid MBR file path
   syslinuxpath <- syslinuxMain (syslinuxConfig config)
   let mbrfile = syslinuxpath </> "bios" </> "mbr" </> "isohdpfx_c.bin"

   createDirectoryIfMissing True isoPath

   withDisk config $ \fp -> do
      showStep "Building ISO image..."
      shellWaitErr
         (mconcat $ List.intersperse " "
            [ "xorriso -as mkisofs"
            , "-R -J"                         -- use rock-ridge/joliet extensions
            , "-o ", isoFile
            , "-c boot/syslinux/boot.cat"     -- create boot catalog
            , "-b boot/syslinux/isolinux.bin" -- bootable binary file
            , "-no-emul-boot"                 -- don't use legacy floppy emulation
            , "-boot-info-table"              -- write additional boot info table 
                                              -- (required by Sylinux)
            , "-boot-load-size 4"
            , "-isohybrid-mbr", mbrfile
            , fp
            ])
         (failWith "Unable to create ISO image")
      putStrLn $ "ISO image: " ++ isoFile
      return isoFile
