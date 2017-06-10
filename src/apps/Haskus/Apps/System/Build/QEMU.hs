{-# LANGUAGE OverloadedStrings #-}

module Haskus.Apps.System.Build.QEMU
   ( qemuExecRamdisk
   , qemuExecISO
   , qemuGetProfileConfig
   , qemuExec
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
   
   (args,kargs) <- qemuGetProfileConfig (qemuConfig config)

   kernel  <- linuxKernelFile (linuxConfig config)
   ramdisk <- ramdiskGetPath (ramdiskConfig config)
   let rdinit = Text.unpack (ramdiskInit (ramdiskConfig config))

   let kerRdArgs = concat $ intersperse " "
         [ "-kernel", kernel
         , "-initrd", ramdisk
         , "-append", ("\"rdinit=/" ++ rdinit ++ " " ++ kargs ++ "\"")
         ]

   qemuExec (args ++ " " ++ kerRdArgs)

-- | Execute ISO
qemuExecISO :: SystemConfig -> FilePath -> IO ()
qemuExecISO config isoPath = do
   
   (args,kargs) <- qemuGetProfileConfig (qemuConfig config)

   let kerRdArgs = concat $ intersperse " "
         [ "-cdrom", isoPath
         , "-append", "\""++kargs++"\""
         ]

   qemuExec (args ++ " " ++ kerRdArgs)


qemuExec :: String -> IO ()
qemuExec args = do
   let cmd = "qemu-system-x86_64 " ++ args

   showStep "Launching QEMU..."
   shellWaitErr cmd $ failWith "Cannot execute QEMU"


qemuGetProfileConfig :: QEMUConfig -> IO (String,String)
qemuGetProfileConfig config =
   case qemuProfile config of
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

