{-# LANGUAGE TypeApplications #-}

import Haskus.System
import Haskus.Format.Binary.Word

import qualified Haskus.Arch.Linux.Terminal as Raw

main :: IO ()
main = runSys' <| do

   sys  <- defaultSystemInit
   term <- defaultTerminal
   let dm = systemDeviceManager sys

   -- Get handle for "zero", "null" and "urandom" virtual devices
   -- zeroDev <- getDeviceHandle dm (makeDevice CharDevice 1 5)
   -- nullDev <- getDeviceHandle dm (makeDevice CharDevice 1 3)
   -- randDev <- getDeviceHandle dm (makeDevice CharDevice 1 9)
   zeroDev <- getDeviceHandleByName dm "/virtual/mem/zero"
               >..~!!> sysErrorShow "Cannot get handle for \"zero\" device"
   nullDev <- getDeviceHandleByName dm "/virtual/mem/null"
               >..~!!> sysErrorShow "Cannot get handle for \"null\" device"
   randDev <- getDeviceHandleByName dm "/virtual/mem/urandom"
               >..~!!> sysErrorShow "Cannot get handle for \"urandom\" device"

   let
      --readWord64 fd = readBuffer fd Nothing 8 --FIXME
      readWord64 fd = readStorable @Word64 fd Nothing

   readWord64 randDev
      >.~.> (\a -> writeStrLn term ("From urandom device: " ++ show a))
      >..~!> const (writeStrLn term "Cannot read urandom device")

   readWord64 zeroDev
      >.~.> (\a -> writeStrLn term ("From zero device: "   ++ show a))
      >..~!> const (writeStrLn term "Cannot read zero device")


   void <| Raw.writeStrLn nullDev "Discarded string"

   -- Release the handles
   releaseDeviceHandle zeroDev
   releaseDeviceHandle nullDev
   releaseDeviceHandle randDev

   waitForKey term
   powerOff
