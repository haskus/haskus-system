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
               |> evalCatchFlowT (sysErrorShow "Cannot get handle for \"zero\" device")
   nullDev <- getDeviceHandleByName dm "/virtual/mem/null"
               |> evalCatchFlowT (sysErrorShow "Cannot get handle for \"null\" device")
   randDev <- getDeviceHandleByName dm "/virtual/mem/urandom"
               |> evalCatchFlowT (sysErrorShow "Cannot get handle for \"urandom\" device")

   let
      --readWord64 fd = readBuffer fd Nothing 8 --FIXME
      readWord64 fd = readStorable @Word64 fd Nothing

   randX <- readWord64 randDev
            |> evalCatchFlowT (sysErrorShow "Cannot read urandom device")
   writeStrLn term ("From urandom device: " ++ show randX)

   zeroX <- readWord64 zeroDev
            |> evalCatchFlowT (sysErrorShow "Cannot read zero device")
   writeStrLn term ("From zero device: "   ++ show zeroX)

   Raw.writeStrLn nullDev "Discarded string"
      |> evalCatchFlowT (sysErrorShow "Cannot write NULL device")


   -- Release the handles
   releaseDeviceHandle zeroDev
   releaseDeviceHandle nullDev
   releaseDeviceHandle randDev

   waitForKey term
   powerOff
