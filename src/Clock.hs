{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

import Haskus.System
import Clock.Render

import Haskus.Format.Binary.Endianness
import qualified Haskus.Format.Binary.BitSet as BitSet

import Haskus.Arch.Linux.Time
import Haskus.Arch.Linux.Graphics.Capability
import Haskus.Arch.Linux.Graphics.State
import Haskus.Arch.Linux.Graphics.FrameBuffer
import Haskus.Arch.Linux.Graphics.Mode
import Haskus.Arch.Linux.Graphics.PixelFormat
import Haskus.Arch.Linux.Graphics.Helper
--import Haskus.Arch.Linux.Internals.Graphics
import Haskus.System.Graphics.Drawing
import Haskus.System.Graphics.Diagrams (rasterizeDiagram,mkWidth)
import Data.Maybe (fromJust)
import Haskus.Utils.Variant
import qualified Haskus.Utils.Map as Map

import Codec.Picture.Types

main :: IO ()
main = runSys' <| do

   term <- defaultTerminal
   sys  <- defaultSystemInit
   cards <- loadGraphicCards (systemDeviceManager sys)

   forM_ cards <| \card -> do
      let fd    = graphicCardHandle card

      sysLogSequence "Load graphic card" <| do
         cap  <- fd `supports` CapGenericBuffer
                  >..~!!> assertShow "Card supports generic buffers"
         sysAssert "Generic buffer capability supported" cap
         
         state <- readGraphicsState fd
                  >..~!!> assertShow "Cannot read graphics state"

         conns <- if Map.null (graphicsConnectors state)
            then sysError "No graphics connector found" 
            else return (Map.elems (graphicsConnectors state))

         let
            isValid x  = case connectorState x of
               Connected d -> not (null <| connectedDeviceModes d)
               _           -> False
            validConns = filter isValid conns

            -- select first connector
            conn = head validConns

            Connected connDev = connectorState conn

            -- select highest mode
            mode   = head (connectedDeviceModes connDev)
            fmt    = makePixelFormat XRGB8888 LittleEndian
            width  = fromIntegral <| modeHorizontalDisplay mode
            height = fromIntegral <| modeVerticalDisplay mode

         gfb1@(GenericFrame fb1 [buf]) <- initGenericFrameBuffer card mode fmt
         gfb2@(GenericFrame _ [_])   <- initGenericFrameBuffer card mode fmt

         let Just ctrl = do
               encId  <- connectorEncoderID conn
               enc    <- Map.lookup encId (graphicsEncoders state)
               ctrlId <- encoderControllerID enc
               Map.lookup ctrlId (graphicsControllers state)

         writeStrLn term (show (surfacePitch (mappedSurfaceInfo buf)))

         -- set mode and connectors
         setController ctrl (SetFB fb1) [conn] (Just mode)
            |> flowAssertQuiet "Set controller"

         -- page flip
         let 
            setFb fb = switchFrameBuffer ctrl fb (BitSet.fromList [PageFlipEvent])
                        |> flowAssertQuiet "Switch framebuffer"

            --clp        = Clip 0 0 (modeHorizontalDisplay mode - 1) (modeVerticalDisplay mode - 1)
            --dirtyFb fb = sysCallAssertQuiet "Dirty framebuffer" <|
            --               dirtyFrameBuffer fd fb (Dirty [clp])

         setFb fb1

         let
            clockDiagram h m s = rasterizeDiagram (mkWidth width) (clockDiag width height h m s)

            mainLoop !b = do
               tv <- (fromJust . getVariantN @0) <$> sysGetTimeOfDay
               let
                  gfb = if b then gfb1 else gfb2
                  GenericFrame fb _ = gfb
                  ts  = tvSeconds tv `mod` (12*60*60)
                  tus = tvMicroSeconds tv
                  h   = fromIntegral ts / 3600
                  m   = fromIntegral (ts - (floor h * 3600)) / 60
                  s   = fromIntegral (ts - (floor h * 3600) - (floor m * 60)) + (fromIntegral tus / 1000000)
                  img = clockDiagram h m s
               liftIO <| blendImage gfb img BlendCopy (0,0) (0,0,imageWidth img, imageHeight img)
               --dirtyFb fb
               setFb fb
               mainLoop (not b)

         sysFork "Main display loop" <| mainLoop False

         return ()
               

      return ()

   waitForKey term
   powerOff
