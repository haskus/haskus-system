{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}

import Haskus.System

import Haskus.System.Linux.FileSystem.Directory
import Haskus.System.Linux.FileSystem
import qualified Haskus.Format.Binary.Buffer as B
import Haskus.Format.Binary.Endianness
import qualified Haskus.Format.Binary.BitSet as BitSet

import Haskus.System.Linux.Graphics.Capability
import Haskus.System.Linux.Graphics.State
import Haskus.System.Linux.Graphics.Mode
import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.System.Linux.Graphics.Helper
import Haskus.System.Linux.Internals.Graphics
import Haskus.System.Linux.ErrorCode
import Haskus.System.Graphics.Drawing
import qualified Haskus.System.Graphics.Diagrams as D
import Haskus.System.Graphics.Diagrams ((#),fc,lw,rasterizeDiagram,mkWidth,none)
import Haskus.Utils.Embed.ByteString
import Haskus.Utils.STM
import qualified Haskus.Utils.Map as Map

import Data.Colour.Names (blue)

import Data.Monoid ((<>))
import Control.Monad (when)
import Data.List (intersperse)
import Data.ByteString (ByteString)
import Codec.Picture.Types

rawlogo :: ByteString
rawlogo = $(embedBSFile "src/image/logo.png")


main :: IO ()
main = runSys' <| do

   let 
      logo = loadPng (B.Buffer rawlogo)

   term <- defaultTerminal

   writeStrLn term "Initializing the system..."
   sys  <- defaultSystemInit
   let dm = systemDeviceManager sys

   listDir term "/system/sys/class/sound"

   -- wait for mouse driver to be loaded (FIXME: use plug-and-play detection)
   threadDelaySec 2

   quitKey <- atomically <| newTVar False

   mousePos <- newTVarIO (250.0,250.0)

   let
      wf = 1024 / 0x7FFF :: Float
      hf = 768  / 0x7FFF :: Float
      updateMouseRel dx dy = modifyTVar mousePos \(x,y) -> (x+fromIntegral dx,y+fromIntegral dy)
      updateMouseAbsX v    = modifyTVar mousePos \(_,y) -> (fromIntegral v * wf,y)
      updateMouseAbsY v    = modifyTVar mousePos \(x,_) -> (x,fromIntegral v * hf)


   points <- newTVarIO []

   writeStrLn term "Loading input devices..."
   inputs <- loadInputDevices dm
   forM_ inputs \inp ->
      onEvent (inputDeviceBundles inp) \(InputEventBundle events) -> do
         atomically <| forM_ (fmap inputEventType events) \case
            InputKeyEvent KeyPress MouseLeft -> do
                                                   p <- readTVar mousePos
                                                   modifyTVar points (p:)
            -- mouse move: using qemu -show-cursor
            InputRelativeEvent RelativeX v -> updateMouseRel v 0
            InputRelativeEvent RelativeY v -> updateMouseRel 0 v
            -- mouse move: using qemu -usbdevice tablet
            InputAbsoluteEvent AbsoluteX v -> updateMouseAbsX v
            InputAbsoluteEvent AbsoluteY v -> updateMouseAbsY v
            -- Quit if ESC is pressed
            InputKeyEvent KeyPress Esc     -> writeTVar quitKey True
            _                              -> return ()


   writeStrLn term "Loading graphic cards..."
   cards <- loadGraphicCards dm

   forM_ cards \card -> do
      let fd    = graphicCardHandle card

      -- onEvent (graphicCardChan card) <| \ev -> do
      --    writeStrLn term (show ev)
         

      sysLogSequence "Load graphic card" <| do
         cap  <- fd `supports` CapHostBuffer
                  |> assertLogShowErrorE "Get generic buffer capability"
         sysAssert "Generic buffer capability supported" cap
         
         state <- readGraphicsState fd
                     |> assertE "Read graphics state"
         encoders <- assertE "Read encoders"
                     <| getHandleEncoders (graphicCardHandle card)
         let encoderMap = Map.fromList (fmap encoderID encoders `zip` encoders)

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
            width  = fromIntegral <| modeHorizontalDisplay mode :: Float
            height = fromIntegral <| modeVerticalDisplay mode :: Float

         gfb1@(GenericFrame fb1 _) <- initGenericFrameBuffer card mode fmt
         gfb2@(GenericFrame {})    <- initGenericFrameBuffer card mode fmt

         writeStrLn term (show fb1)

         let Just ctrl = do
               encId  <- connectorEncoderID conn
               enc    <- Map.lookup encId encoderMap
               ctrlId <- encoderControllerID enc
               Map.lookup ctrlId (graphicsControllers state)


         -- set mode and connectors
         setController ctrl (SetSource fb1) [conn] (Just mode)
            |> assertE "Set controller"

         -- let 
         --    gamma' = replicate (fromIntegral (controllerGammaTableSize ctrl)) 0x0000
         --    gamma  = (gamma',gamma',gamma')
         -- setControllerGamma ctrl gamma
         -- ga <- getControllerGamma ctrl
         -- writeStrLn term (show ga)

         -- page flip
         let setFb fb = switchFrame ctrl fb (BitSet.fromList [SwitchFrameGenerateEvent]) 0
                        |> assertE "Switch framebuffer"

         setFb fb1

         writeStrLn term "Drawing..."

         fps <- atomically <| newTVar (0 :: Word)

         let
            speed = 15

            mainLoop !color !b !step = do
               (mx,my) <- readTVarIO mousePos
               ps <- readTVarIO points
               let
                  gfb = if b then gfb1 else gfb2
                  GenericFrame fb [_] = gfb
                  drawColor = PixelRGBA8 0 0x86 0xc1 255
                  ptrColor = PixelRGBA8 0x50 0x50 0x50 255
                  redColor = PixelRGBA8 0x86 0xc1 0 255
                  bgColor   = PixelRGBA8 0 0 0 0
                  img = renderDrawing (floor width) (floor height) bgColor <| do
                     withTexture (uniformTexture drawColor) <| do
                        forM_ (ps `zip` tail ps) <| \((x1,y1),(x2,y2)) ->
                           stroke 17 JoinRound (CapRound, CapRound)
                              <| line (V2 x1 y1) (V2 x2 y2)
                     when (not (null ps)) <| do
                        let (lx,ly) = head ps
                        withTexture (uniformTexture redColor) <| do
                           stroke 17 JoinRound (CapRound, CapRound)
                              <| line (V2 lx ly) (V2 mx my)
                     withTexture (uniformTexture ptrColor) <| do
                        let len = 8
                        stroke 2 (JoinMiter 0) (CapStraight 0, CapStraight 0)
                            <| line (V2 (mx-len) my) (V2 (mx+len) my)
                        stroke 2 (JoinMiter 0) (CapStraight 0, CapStraight 0)
                            <| line (V2 mx (my-len)) (V2 mx (my+len))


                  img2 = rasterizeDiagram (mkWidth width) diag
                     where
                        pts = map (\(x,y) -> D.p2 (x,-1 * y)) ps :: [D.P2 Float]
                        diag = D.rect width height
                                 <> D.moveOriginBy (D.r2 (width / 2 :: Float, -1 * height / 2))
                                    (D.position (pts `zip` repeat spot)
                                    <> D.cubicSpline False pts
                                    )
                        spot = D.circle 5 # fc blue # lw none

               liftIO <| do
                  fillFrame gfb color
                  blendImage gfb img BlendAlpha (0,0) (0,0,imageWidth img, imageHeight img)
                  blendImage gfb img2 BlendAlpha (0,0) (0,0,imageWidth img2, imageHeight img2)
                  blendImage gfb logo BlendAlpha (20,20) (0,0,imageWidth logo,imageHeight logo)
               setFb fb
               atomically <| modifyTVar' fps (+1)
               --threadDelayMilliSec 100
               yield -- switch to another thread
               let
                  !step2  = case color of
                     c | c >= 0x00D3D3FF -> -1 * speed
                       | c <= 0x00D3D300 -> speed
                       | otherwise       -> step
                  !color2 = case color+step2 of
                     c | c >= 0x00D3D3FF -> 0xD3D3FF
                       | c <= 0x00D3D300 -> 0xD3D300
                       | otherwise       -> c
               mainLoop color2 (not b) step2

         sysFork "Main display loop" <| mainLoop 0x00D3D300 False speed
         --lift <| fillScreen 0x00D3D3D3 (mappedPlanePointer plane2)
         --setFb fb2

         -- show FPS
         sysFork "FPS displayer" <| forever <| do
            v <- atomically <| swapTVar fps 0
            writeStrLn term ("FPS: " ++ show v)
            threadDelaySec 1

         return ()
               

      return ()


   writeStrLn term "Done."

   -- wait for a key in the standard input (in the console) or ESC (in the graphic interface)
   sysFork "Wait for key" <| do
      waitForKey term
      atomically <| writeTVar quitKey True
   
   atomically <| do
      q <- readTVar quitKey
      case q of
         True  -> return ()
         False -> retry

   writeStrLn term "Log:"
   sysLogPrint

   -- shutdown the computer
   powerOff


listDir :: Terminal -> FilePath -> Sys ()
listDir term path = do
   dls <- assertE "List directory" do
      rt <- open Nothing path (BitSet.fromList [HandleDirectory]) BitSet.empty
               |> liftE @(ErrorCode : OpenErrors)
      ls <- liftE <| listDirectory rt
      void <| liftE (close rt)
      return ls
   writeStrLn term (concat . intersperse "\n" . fmap entryName <| dls)
