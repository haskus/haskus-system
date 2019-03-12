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
      mhdl <- fromVariantHead <$> (runFlowT <| getDeviceHandleByName dm (Text.unpack devPath))
      forM_ mhdl <| \hdl -> do
         writeStrLn term ("\n- " ++ show devPath)

         -- "mixer*" device seems to return garbage in QEMU
         unless ("mixer" `List.isPrefixOf` (takeBaseName (Text.unpack devPath))) <| do
            hwinfo <- runFlowT (Snd.ioctlHwInfo hdl)
            writeStrLn term ("HW info: " ++ show hwinfo)

            pcminfo <- runFlowT (Snd.ioctlPcmInfo hdl)
            writeStrLn term ("PCM info: "++show pcminfo)

            midiinfo <- runFlowT (Snd.ioctlMidiInfo hdl)
            writeStrLn term ("MIDI info: "++show midiinfo)

            timerinfo <- runFlowT (Snd.ioctlTimerInfo hdl)
            writeStrLn term ("Timer info: "++show timerinfo)

            cardinfo <- runFlowT (Snd.ioctlControlCardInfo hdl)
            writeStrLn term ("Control card info: "++show cardinfo)

            controlhwinfo <- runFlowT (Snd.ioctlControlHwInfo hdl)
            writeStrLn term ("Control HW info: "++show controlhwinfo)

         runFlowT_ (close hdl)

   -- select "hw" devices
   pcmDevs <- catMaybes <|| forM (map fst soundDevs) <| \devPath -> do
      let
         path     = Text.unpack devPath
         basename = takeBaseName path
      if "pcm" `List.isPrefixOf` basename
         then do
            fromVariantHead <$> runFlowT (getDeviceHandleByName dm (Text.unpack devPath))
         else
            return Nothing

   forM_ pcmDevs <| \pcm -> do
      writeStrLn term ("\n\nSetting params on device: " <> show pcm)
      writeStrLn term ("Request: " <> show anyParams)
      r <- runFlowT (toConfig <$> Snd.ioctlPcmHwParams anyParams pcm)
      writeStrLn term ("Result: " <> show r)
      runFlowT_ (close pcm)

   powerOff
