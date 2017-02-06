import Development.Shake
import Development.Shake.FilePath
import Data.List
import System.Process
 
linuxVersion,syslinuxVersion :: String
linuxVersion   = "4.9.8"
syslinuxVersion = "6.03"

main :: IO ()
main = do

   -- read GHC version
   ghcVersion <- last . words <$> readProcess "stack" ["exec", "--", "ghc", "--version"] ""

   -- read stack resolver
   stackResolver <- last . words . head . filter ("resolver:" `isPrefixOf`) . lines <$> readFile "stack.yaml"

   putStrLn "==================================================="
   putStrLn "       Haskus system examples - config"
   putStrLn "---------------------------------------------------"
   putStrLn ("GHC version:      " ++ ghcVersion)
   putStrLn ("Stack resolver:   " ++ stackResolver)
   putStrLn ("Linux version:    " ++ linuxVersion)
   putStrLn ("Syslinux version: " ++ syslinuxVersion)
   putStrLn "==================================================="

   let
      stackPath :: FilePath -> FilePath
      stackPath x =
         ".stack-work/install/x86_64-linux"
            </> stackResolver
            </> ghcVersion
            </> "bin"
            </> x

   shakeArgs shakeOptions{shakeFiles="_build"} $ do
      want [ "_build/linux-"++linuxVersion++".bin"]

      -- build linux
      "_build/linux-*.bin" %> \out -> do
         let 
            srcdir   = "_sources/linux-"++linuxVersion
            makefile = srcdir </> "Makefile"
         need [makefile]
         unit $ cmd (Cwd srcdir) "make" "x86_64_defconfig"
         unit $ cmd (Cwd srcdir) "./scripts/config" "-e" "CONFIG_DRM_BOCHS"
         unit $ cmd (Cwd srcdir) "./scripts/config" "-e" "CONFIG_DRM_RADEON"
         unit $ cmd (Cwd srcdir) "./scripts/config" "-e" "CONFIG_DRM_NOUVEAU"
         unit $ cmd (Cwd srcdir) "./scripts/config" "-e" "CONFIG_DRM_VIRTIO_GPU"
         -- unit $ cmd (Cwd srcdir) "./scripts/config" "-e" "CONFIG_VIRTIO"
         -- unit $ cmd (Cwd srcdir) "./scripts/config" "-e" "CONFIG_VIRTIO_BLK"
         -- unit $ cmd (Cwd srcdir) "./scripts/config" "-e" "CONFIG_VIRTIO_NET"
         -- unit $ cmd (Cwd srcdir) "./scripts/config" "-e" "CONFIG_VIRTIO_INPUT"
         -- unit $ cmd (Cwd srcdir) "./scripts/config" "-e" "CONFIG_VIRTIO_MMIO"
         -- unit $ cmd (Cwd srcdir) "./scripts/config" "-d" "CONFIG_VIRTIO_MMIO_CMDLINE_DEVICES"
         -- unit $ cmd (Cwd srcdir) "./scripts/config" "-e" "CONFIG_VIRTIO_CONSOLE"
         -- unit $ cmd (Cwd srcdir) "./scripts/config" "-e" "HW_RANDOM_VIRTIO"
         -- unit $ cmd (Cwd srcdir) "./scripts/config" "-d" "VIRTIO_BALLOON"
         unit $ cmd (Cwd srcdir) "make" "oldconfig"
         unit $ cmd (Cwd srcdir) "make" "-j8"
         unit $ cmd "cp" (srcdir </> "arch/x86/boot/bzImage") out

      -- unpack linux
      "_sources/linux-*/Makefile" %> \_ -> do
         let src = "_downloads/linux-"++linuxVersion++".tar.xz"
         need [src]
         cmd (Cwd "_sources") "tar" "xf" (".." </> src)

      -- download linux
      "_downloads/linux-*.tar.xz" %> \_ -> do
         let src = "https://www.kernel.org/pub/linux/kernel/v4.x/linux-"++linuxVersion++".tar.xz"
         cmd (Cwd "_downloads") "wget" src

      -- download SysLinux
      "_downloads/syslinux-*.tar.xz" %> \_ -> do
         let src = "https://www.kernel.org/pub/linux/utils/boot/syslinux/syslinux-"++syslinuxVersion++".tar.xz"
         cmd (Cwd "_downloads") "wget" src

      -- unpack SysLinux
      "_sources/syslinux-*/bios/core/isolinux.bin" %> \_ -> do
         let src = "_downloads/syslinux-"++syslinuxVersion++".tar.xz"
         need [src]
         cmd (Cwd "_sources") "tar" "xf" (".." </> src)

      -- copy binary program
      "_build/bin/*" %> \out -> do
         let bin = stackPath (takeBaseName out)
         need [bin]
         cmd "cp" bin out

      -- make init ramdisk
      "_build/ramdisks/*.img" %> \out -> do
         let
            name   = dropExtension (takeBaseName out)
            bin    = "_build/bin" </> name
            imgdir = dropExtension out
         need [bin]
         unit $ cmd "mkdir" "-p" imgdir
         unit $ cmd "cp" "-f" bin imgdir
         unit $ cmd Shell $ "(cd "++imgdir++" ; find . | cpio -o -H newc | gzip) > " ++ out

      -- make disk
      "_build/disks/**/*.img" %> \out -> do
         let
            name    = dropExtension (takeBaseName out)
            ker     = "_build/linux-"++linuxVersion++".bin"
            img     = "_build/ramdisks" </> name <.> ".img"
            slsrc   = "_sources/syslinux-" ++ syslinuxVersion 
            syslin  = slsrc </> "bios/core/isolinux.bin"
            outdir  = takeDirectory (takeDirectory out)
            bootdir = outdir </> "boot"
            sldir   = bootdir </> "syslinux"
            slconf  = sldir </> "syslinux.cfg"
         need [ker,img,syslin]
         -- create boot directory
         unit $ cmd "mkdir" "-p" bootdir
         unit $ cmd "mkdir" "-p" sldir 
         -- copy kernel and init disk image
         unit $ cmd "cp" "-f" ker bootdir
         unit $ cmd "cp" "-f" img bootdir
         -- copy syslinux
         unit $ cmd "find" (slsrc </> "bios")
                  "-name" "*.c32"
                  "-exec" "cp" "{}" sldir ";"
         unit $ cmd "cp" "-f" syslin (sldir </> "isolinux.bin")
         -- configure syslinux
         let
            syslinuxConf =
                 "DEFAULT main\n\
                 \PROMPT 0\n\
                 \TIMEOUT 50\n\
                 \UI vesamenu.c32\n\
                 \\n\
                 \LABEL main\n\
                 \MENU LABEL MyOS\n\
                 \LINUX  /boot/" ++ takeBaseName ker ++ ".bin\n\
                 \INITRD /boot/" ++ takeBaseName img ++ ".img\n\
                 \APPEND rdinit=" ++ name ++ "\n"
         liftIO $ writeFile slconf syslinuxConf

      -- make ISO image
      "_build/isos/*.iso" %> \out -> do
         let
            name    = dropExtension (takeBaseName out)
            ker     = "_build/linux-"++linuxVersion++".bin"
            img     = "_build/ramdisks" </> name <.> ".img"
            disk    = "_build/disks"    </> name
            slsrc   = "_sources/syslinux-" ++ syslinuxVersion 
            syslin  = slsrc </> "bios/core/isolinux.bin"
         need [ker,img,syslin, disk </> "boot" </> name <.> "img"]
         -- create ISO
         unit $ cmd "mkdir" "-p" "_build/isos"
         unit $ cmd "xorriso" "-as" "mkisofs" 
                  "-R" "-J"                         -- use Rock-Ridge/Joliet extensions
                  "-o" out                          -- output ISO file
                  "-c" "boot/syslinux/boot.cat"     -- create boot catalog
                  "-b" "boot/syslinux/isolinux.bin" -- bootable binary file
                  "-no-emul-boot"                   -- doesn't use legacy floppy emulation
                  "-boot-info-table"                -- write additional Boot Info Table (required by SysLinux)
                  "-boot-load-size" "4"
                  "-isohybrid-mbr" (slsrc </> "bios/mbr/isohdpfx_c.bin")
                  disk

      let
         customCommands s
            -- launch the program with qemu
            | "qemu/" `isPrefixOf` s = Just $ do
               let
                  name = drop 5 s
                  img  = "_build/ramdisks" </> (name ++ ".img")
                  ker  = "_build/linux-"++linuxVersion++".bin"
               need [ker, img]
               cmd Shell "qemu-system-x86_64" 
                  "-enable-kvm"
                  "-serial" "stdio"
                  "-soundhw" "hda"
                  "-kernel" ker
                  "-initrd" img
                  "-append" ("\"rdinit=/" ++ name ++ " console=ttyS0 atkbd.softraw=0 quiet\"")
            -- launch the program with qemu (-machine q35)
            | "qemu2/" `isPrefixOf` s = Just $ do
               let
                  name = drop 6 s
                  img  = "_build/ramdisks" </> name <.> "img"
                  ker  = "_build/linux-"++linuxVersion++".bin"
               need [ker, img]
               cmd Shell "qemu-system-x86_64" 
                  "-enable-kvm"
                  "-machine" "q35"
                  "-soundhw" "hda"
                  "-serial" "stdio"
                  "-vga" "std"
                  --"-show-cursor"
                  "-usbdevice" "tablet"
                  "-kernel" ker
                  "-initrd" img
                  "-append" ("\"rdinit=/" ++ name ++ " console=ttyS0 atkbd.softraw=0\"")
            -- launch an ISO with qemu (-machine q35)
            | "iso/" `isPrefixOf` s = Just $ do
               let
                  name = drop 4 s
                  iso  = "_build/isos" </> name <.> "iso"
               need [iso]
               cmd Shell "qemu-system-x86_64" 
                  "-enable-kvm"
                  "-machine" "q35"
                  "-soundhw" "hda"
                  "-serial" "stdio"
                  "-vga" "std"
                  --"-show-cursor"
                  "-usbdevice" "tablet"
                  "-cdrom" iso
                  -- "-append" ("\"rdinit=/" ++ name ++ " console=ttyS0 atkbd.softraw=0\"")
            -- create disk
            | "disk/" `isPrefixOf` s = Just $ do
               let
                  name  = drop 5 s
                  disk  = "_build/disks" </> name </> "boot" </> name <.> "img"
               need [disk]
            | otherwise = Nothing

      phonys customCommands


