{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

import Haskus.System
import qualified Haskus.Format.Text as Text
import Haskus.Utils.Maybe

import qualified Haskus.System.Linux.Internals.Sound as Snd
import Haskus.System.Linux.Sound.Pcm
import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.FileSystem (close,OpenErrors)

import System.FilePath
import Data.List as List

main :: IO ()
main = runSys' <| do

   sys  <- defaultSystemInit
   term <- defaultTerminal
   let dm = systemDeviceManager sys

   soundDevs   <- listDevicesWithClass dm "sound"

   writeStrLn term "Sound devices:"

   forM_ (map fst soundDevs) <| \devPath -> do
      mhdl <- runFlow <| getDeviceHandleByName dm (Text.unpack devPath)
      forM_ mhdl <| \hdl -> do
         writeStrLn term ("\n- " ++ show devPath)

         -- "mixer*" device seems to return garbage in QEMU
         unless ("mixer" `List.isPrefixOf` (takeBaseName (Text.unpack devPath))) <| do
            hwinfo <- runFlow (Snd.ioctlHwInfo hdl)
            writeStrLn term ("HW info: " ++ show hwinfo)

            pcminfo <- runFlow (Snd.ioctlPcmInfo hdl)
            writeStrLn term ("PCM info: "++show pcminfo)

            midiinfo <- runFlow (Snd.ioctlMidiInfo hdl)
            writeStrLn term ("MIDI info: "++show midiinfo)

            timerinfo <- runFlow (Snd.ioctlTimerInfo hdl)
            writeStrLn term ("Timer info: "++show timerinfo)

            cardinfo <- runFlow (Snd.ioctlControlCardInfo hdl)
            writeStrLn term ("Control card info: "++show cardinfo)

            controlhwinfo <- runFlow (Snd.ioctlControlHwInfo hdl)
            writeStrLn term ("Control HW info: "++show controlhwinfo)

         runFlow_ (close hdl)

   -- select "hw" devices
   pcmDevs <- catMaybes <|| forM (map fst soundDevs) <| \devPath -> do
      let
         path     = Text.unpack devPath
         basename = takeBaseName path
      if "pcm" `List.isPrefixOf` basename
         then do
            ehdl <- runFlow (getDeviceHandleByName dm (Text.unpack devPath))
            return $ veitherCont (const Nothing) Just ehdl
         else
            return Nothing

   forM_ pcmDevs <| \pcm -> do
      writeStrLn term ("\n\nSetting params on device: " <> show pcm)
      writeStrLn term ("Request: " <> show anyParams)
      r <- runFlow (toConfig <$> Snd.ioctlPcmHwParams anyParams pcm)
      writeStrLn term ("Result: " <> show r)
      runFlow_ (close pcm)

   powerOff
