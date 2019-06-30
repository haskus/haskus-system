{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Haskus.System
import Haskus.Format.Binary.Word

import qualified Haskus.System.Linux.Terminal as Raw

main :: IO ()
main = runSys' do

   sys  <- defaultSystemInit
   term <- defaultTerminal
   let dm = systemDeviceManager sys

   zeroDev <- getDeviceHandleByName dm "/virtual/mem/zero"
               |> catchEvalE (sysErrorShow "Cannot get handle for \"zero\" device")
   nullDev <- getDeviceHandleByName dm "/virtual/mem/null"
               |> catchEvalE (sysErrorShow "Cannot get handle for \"null\" device")
   randDev <- getDeviceHandleByName dm "/virtual/mem/urandom"
               |> catchEvalE (sysErrorShow "Cannot get handle for \"urandom\" device")

   let
      readWord64 fd = readStorable @Word64 fd Nothing

   randValue <- readWord64 randDev
            |> assertE "Read urandom device"
   writeStrLn term ("From urandom device: " ++ show randValue)

   zeroValue <- readWord64 zeroDev
            |> assertE "Read zero device"
   writeStrLn term ("From zero device: "   ++ show zeroValue)

   Raw.writeStrLn nullDev "Discarded string"
      |> assertE "Write NULL device"

   -- Release the handles
   releaseDeviceHandle zeroDev
   releaseDeviceHandle nullDev
   releaseDeviceHandle randDev

   powerOff
