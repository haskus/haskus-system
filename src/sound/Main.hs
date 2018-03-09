{-# LANGUAGE TypeApplications #-}

import Haskus.System
import qualified Haskus.Format.Text as Text
import Haskus.Utils.Maybe

import qualified Haskus.System.Linux.Internals.Sound as Snd
import Haskus.System.Linux.Sound.Pcm
import Haskus.System.Linux.FileSystem (close)

import System.FilePath
import Data.List as List
import Data.Monoid

main :: IO ()
main = runSys' <| do

   sys  <- defaultSystemInit
   term <- defaultTerminal
   let dm = systemDeviceManager sys

   soundDevs   <- listDevicesWithClass dm "sound"

   writeStrLn term "Sound devices:"

   forM_ (map fst soundDevs) <| \devPath -> do
      devRes <- getDeviceHandleByName dm (Text.unpack devPath)
               >.-.> Just
               >..-.> const Nothing

      case devRes of
         Just hdl -> do
            writeStrLn term ("\n- " ++ show devPath)

            -- "mixer*" device seems to return garbage in QEMU
            unless ("mixer" `List.isPrefixOf` (takeBaseName (Text.unpack devPath))) <| do
               hwinfo <- liftIO (Snd.ioctlHwInfo hdl)
               writeStrLn term ("HW info: " ++ show hwinfo)

               pcminfo <- liftIO (Snd.ioctlPcmInfo hdl)
               writeStrLn term ("PCM info: "++show pcminfo)

               midiinfo <- liftIO (Snd.ioctlMidiInfo hdl)
               writeStrLn term ("MIDI info: "++show midiinfo)

               timerinfo <- liftIO (Snd.ioctlTimerInfo hdl)
               writeStrLn term ("Timer info: "++show timerinfo)

               cardinfo <- liftIO (Snd.ioctlControlCardInfo hdl)
               writeStrLn term ("Control card info: "++show cardinfo)

               controlhwinfo <- liftIO (Snd.ioctlControlHwInfo hdl)
               writeStrLn term ("Control HW info: "++show controlhwinfo)

            void (close hdl)

         _            -> return ()


   -- select "hw" devices
   pcmDevs <- catMaybes <|| forM (map fst soundDevs) <| \devPath -> do
      let
         path     = Text.unpack devPath
         basename = takeBaseName path
      if "pcm" `List.isPrefixOf` basename
         then do
            getDeviceHandleByName dm (Text.unpack devPath)
               >.-.> Just
               >..-.> const Nothing
         else
            return Nothing

   forM_ pcmDevs <| \pcm -> do
      writeStrLn term ("\n\nSetting params on device: " <> show pcm)
      writeStrLn term ("Request: " <> show anyParams)
      r <- liftIO <| Snd.ioctlPcmHwParams anyParams pcm
      writeStrLn term ("Result: " <> show r)
      r .~!> \r' -> writeStrLn term ("Result: " <> show (toConfig r'))
      void (close pcm)

   powerOff
