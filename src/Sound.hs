{-# LANGUAGE TypeApplications #-}

import Haskus.System
import qualified Haskus.Format.Text as Text

import qualified Haskus.Arch.Linux.Internals.Sound as Snd

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

            -- "mixer" device seems to return garbage in QEMU
            unless (Text.pack "mixer" `Text.isSuffixOf` devPath) <| do
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

         _            -> return ()


   powerOff
