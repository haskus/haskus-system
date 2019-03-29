{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

import Haskus.System
import Haskus.Format.Binary.Word

import qualified Haskus.System.Linux.Terminal as Raw

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
               |> catchEvalE (sysErrorShow "Cannot get handle for \"zero\" device")
   nullDev <- getDeviceHandleByName dm "/virtual/mem/null"
               |> catchEvalE (sysErrorShow "Cannot get handle for \"null\" device")
   randDev <- getDeviceHandleByName dm "/virtual/mem/urandom"
               |> catchEvalE (sysErrorShow "Cannot get handle for \"urandom\" device")

   let
      --readWord64 fd = readBuffer fd Nothing 8 --FIXME
      readWord64 fd = readStorable @Word64 fd Nothing

   randX <- readWord64 randDev
            |> assertE "Read urandom device"
   writeStrLn term ("From urandom device: " ++ show randX)

   zeroX <- readWord64 zeroDev
            |> assertE "Read zero device"
   writeStrLn term ("From zero device: "   ++ show zeroX)

   Raw.writeStrLn nullDev "Discarded string"
      |> assertE "Write NULL device"


   -- Release the handles
   releaseDeviceHandle zeroDev
   releaseDeviceHandle nullDev
   releaseDeviceHandle randDev

   waitForKey term
   powerOff
