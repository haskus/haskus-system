{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

import Haskus.System
import qualified Haskus.Utils.Text as Text
import Haskus.Utils.Maybe

import qualified Haskus.System.Linux.Internals.Sound as Snd
import Haskus.System.Linux.Sound.Pcm
import Haskus.System.Linux.FileSystem (close)
import Haskus.Binary.BitSet as BitSet
import Haskus.System.Linux.Memory
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent (newForeignPtr)

import System.FilePath
import Data.List as List
import qualified Data.Set as Set

main :: IO ()
main = runSys' <| do

   sys  <- defaultSystemInit
   term <- defaultTerminal
   let dm = systemDeviceManager sys

   soundDevs   <- listDevicesWithClass dm "sound"

   writeStrLn term "Sound devices:"

   forM_ (map fst soundDevs) <| \devPath -> do
      mhdl <- runE <| getDeviceHandleByName dm (Text.unpack devPath)
      forM_ mhdl <| \hdl -> do
         writeStrLn term ("\n- " ++ show devPath)

         do
            hwinfo <- runE (Snd.ioctlHwInfo hdl)
            writeStrLn term ("HW info: " ++ show hwinfo)

            pcminfo <- runE (Snd.ioctlPcmInfo hdl)
            writeStrLn term ("PCM info: "++show pcminfo)

            midiinfo <- runE (Snd.ioctlMidiInfo hdl)
            writeStrLn term ("MIDI info: "++show midiinfo)

            timerinfo <- runE (Snd.ioctlTimerInfo hdl)
            writeStrLn term ("Timer info: "++show timerinfo)

            cardinfo <- runE (Snd.ioctlControlCardInfo hdl)
            writeStrLn term ("Control card info: "++show cardinfo)

            controlhwinfo <- runE (Snd.ioctlControlHwInfo hdl)
            writeStrLn term ("Control HW info: "++show controlhwinfo)

         runE_ (close hdl)

   -- select "hw" devices
   pcmDevs <- catMaybes <|| forM (map fst soundDevs) <| \devPath -> do
      let
         path     = Text.unpack devPath
         basename = takeBaseName path
      if "pcm" `List.isPrefixOf` basename
         then do
            ehdl <- runE (getDeviceHandleByName dm (Text.unpack devPath))
            return $ veitherCont (const Nothing) Just ehdl
         else
            return Nothing

   forM_ pcmDevs <| \pcm -> do
      writeStrLn term ("\n\nSetting params on device: " <> show pcm)

      hw_params <- runE (toConfig <$> Snd.ioctlPcmHwParams anyHwParams pcm)
      writeStrLn term ("HW params: " <> show hw_params)
      sw_params <- runE (Snd.ioctlPcmSwParams defaultSwParams pcm)
      writeStrLn term ("SW params: " <> show sw_params)

      -- FIXME: we suppose at least 2 channels
      let chan1' = Snd.PcmChannelInfo 0 0 0 0
          chan2' = Snd.PcmChannelInfo 1 0 0 0
      chan1 <- runE (Snd.ioctlPcmChannelInfo chan1' pcm)
      chan2 <- runE (Snd.ioctlPcmChannelInfo chan2' pcm)
      writeStrLn term ("Channel 1: " <> show chan1)
      writeStrLn term ("Channel 2: " <> show chan2)

      -- FIXME: support more access modes
      case (Set.toList . pcmConfigAccess) <$> hw_params of
         VRight [Snd.PcmAccessMmapInterleaved] -> return ()
         _                                     -> error "Invalid access mode"

      rp <- runE (Snd.ioctlPcmPrepare pcm)
      writeStrLn term ("Prepare: " <> show rp)

      let sz = 1024 -- FIXME: buffer size
      vaddr <- runE $ sysMemMap Nothing sz
               (BitSet.fromList [ProtRead,ProtWrite])
               (BitSet.fromList [MapShared])
               Nothing
               (Just (pcm, 0)) -- FIXME: handle offsets..
                  -- -- free buffer on mapping error
                  -- |> onE_ (runE_ (handleFreeGenericBuffer hdl (cdHandle r)))

      case vaddr of
         VLeft _     -> error "Error while mapping"
         VRight addr -> do

            -- create a foreign pointer that automatically unmaps the buffer
            let finalizer = runE_ (sysMemUnmap addr sz)
            fptr <- liftIO (newForeignPtr addr finalizer)

            writeStrLn term ("Mmaped: " <> show addr)

      runE_ (close pcm)

   powerOff
