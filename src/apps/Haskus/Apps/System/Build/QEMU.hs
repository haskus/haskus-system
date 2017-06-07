{-# LANGUAGE OverloadedStrings #-}

module Haskus.Apps.System.Build.QEMU
   ( qemuExecRamdisk
   )
where

import Haskus.Apps.System.Build.Config
import Haskus.Apps.System.Build.Utils
import Haskus.Apps.System.Build.Ramdisk
import Haskus.Apps.System.Build.Linux

import Data.List
import qualified Data.Text as Text

-- | Execute (ramdisk + kernel)
qemuExecRamdisk :: SystemConfig -> IO ()
qemuExecRamdisk config = do
   
   let
      qConfig = qemuConfig config

   (args,kargs) <- case qemuProfile qConfig of
         "vanilla" -> return ("", "")
         "default" -> return $
            (concat $ intersperse " "
               [ "-enable-kvm"
               , "-machine q35"
               , "-soundhw hda"
               , "-serial stdio"
               , "-vga std"
               --, "-show-cursor"
               , "-usbdevice tablet"
               ]
            , "console=ttyS0 atkbd.softraw=0 quiet"
            )
         p         -> failWith $ "Invalid QEMU profile: " ++ Text.unpack p

   kernel  <- linuxKernelFile (linuxConfig config)
   ramdisk <- ramdiskGetPath (ramdiskConfig config)
   let rdinit = Text.unpack (ramdiskInit (ramdiskConfig config))

   let kerRdArgs = concat $ intersperse " "
         [ "-kernel", kernel
         , "-initrd", ramdisk
         , "-append", ("\"rdinit=/" ++ rdinit ++ " " ++ kargs ++ "\"")
         ]

   let cmd = "qemu-system-x86_64 " ++ args ++ " " ++ kerRdArgs


   showStep "Launching QEMU..."
   shellWaitErr cmd $ failWith "Cannot execute QEMU"

