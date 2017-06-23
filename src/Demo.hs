{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}

import Haskus.System

import qualified Haskus.Format.Binary.Buffer as B

import Haskus.System.Linux.Info
import Haskus.System.Linux.Graphics.State
import Haskus.System.Linux.Graphics.Mode
import Haskus.System.Linux.Graphics.Property
import Haskus.System.Linux.Graphics.Config
import Haskus.System.Graphics.Drawing
import Haskus.System.Graphics.Diagrams (mkWidth, rasterizeDiagram)
import Haskus.Utils.Embed
import Haskus.Utils.STM
import Haskus.Utils.Monad
import qualified Haskus.Utils.Map as Map

import Codec.Picture.Types

import Demo.Diagrams
import Demo.Graphics

rawlogo :: B.Buffer
rawlogo = B.Buffer $(embedFile "src/image/logo_transparent.png")

rawabcd :: B.Buffer
rawabcd = B.Buffer $(embedFile "src/image/abcd.png")

data Page
   = PageNone
   | PageInfo
   | PageGraphics
   | PageDPMS
   | PageTerminal
   deriving (Show,Eq)

main :: IO ()
main = runSys' <| do

   let logo = loadPng rawlogo
   let abcd = loadPng rawabcd

   term <- defaultTerminal
   sys  <- defaultSystemInit
   let dm = systemDeviceManager sys

   -- wait for mouse driver to be loaded (FIXME: use plug-and-play detection)
   threadDelaySec 2

   quitKey <- newTVarIO False
   page    <- newTVarIO PageNone

   mousePos <- newTVarIO (250.0,250.0)
   lastKey  <- newTVarIO Nothing

   let
      wf = 1024 / 0x7FFF :: Float
      hf = 768  / 0x7FFF :: Float
      updateMouseRel dx dy = modifyTVar mousePos (\(x,y) -> (x+fromIntegral dx,y+fromIntegral dy))
      updateMouseAbsX v    = modifyTVar mousePos (\(_,y) -> (fromIntegral v * wf,y))
      updateMouseAbsY v    = modifyTVar mousePos (\(x,_) -> (x,fromIntegral v * hf))

   dpmsState <- atomically $ newEmptyTMVar

   writeStrLn term "Loading input devices..."
   inputs <- loadInputDevices dm
   forM_ inputs <| \inp -> onEvent (inputDeviceBundles inp) <| \(InputEventBundle events) -> do
      atomically <| forM_ (fmap inputEventType events) <| \case
         -- mouse move: using qemu -show-cursor
         InputRelativeEvent RelativeX v -> updateMouseRel v 0
         InputRelativeEvent RelativeY v -> updateMouseRel 0 v
         -- mouse move: using qemu -usbdevice tablet
         InputAbsoluteEvent AbsoluteX v -> updateMouseAbsX v
         InputAbsoluteEvent AbsoluteY v -> updateMouseAbsY v
         InputKeyEvent KeyPress k       -> do
            writeTVar lastKey (Just k)
            p <- readTVar page
            case k of
               Esc -> case p of
                  PageNone -> writeTVar quitKey True
                  _        -> writeTVar page PageNone
               F1  -> writeTVar page PageInfo
               F2  -> writeTVar page PageGraphics
               F3  -> writeTVar page PageDPMS
               F4  -> writeTVar page PageTerminal
               x   -> case p of
                  PageDPMS -> do
                     void (tryTakeTMVar dpmsState)
                     case x of
                        Key0 -> putTMVar dpmsState 0
                        Key1 -> putTMVar dpmsState 1
                        Key2 -> putTMVar dpmsState 2
                        Key3 -> putTMVar dpmsState 3
                        _    -> return ()
                  _        -> return ()
         _                              -> return ()

   cards <- loadGraphicCards dm

   forM_ cards <| \card -> do

      state <- readGraphicsState (graphicCardHandle card)
               >..~!!> assertShow "Cannot read graphics state"

      -- get connectors
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
         width, height :: Double
         width  = fromIntegral <| modeHorizontalDisplay mode
         height = fromIntegral <| modeVerticalDisplay mode

      let Just ctrl = do
            encId  <- connectorEncoderID conn
            enc    <- Map.lookup encId (graphicsEncoders state)
            ctrlId <- encoderControllerID enc
            Map.lookup ctrlId (graphicsControllers state)

      info <- systemInfo
         >.-.> Just
         >..~.> const (return Nothing)

      let
         bgColor  = 0x316594
         ptrColor = PixelRGBA8 0 0 0 255
         --whiteClr = PixelRGBA8 255 255 255 255
         trans    = PixelRGBA8 0 0 0 0
         ptrLen   = 8 :: Int
         ptrLenP1 = fromIntegral ptrLen + 1
         ptrWidth = 2*ptrLen+1
         ptrWidth'= fromIntegral ptrWidth
         ptr      = renderDrawing ptrWidth ptrWidth trans <| do
                        withTexture (uniformTexture ptrColor) <| do
                           stroke 1 (JoinMiter 0) (CapStraight 0, CapStraight 0)
                               <| line (V2 ptrLenP1 0) (V2 ptrLenP1 ptrWidth')
                           stroke 1 (JoinMiter 0) (CapStraight 0, CapStraight 0)
                               <| line (V2 0 ptrLenP1) (V2 ptrWidth' ptrLenP1)
         topBarDiagram = rasterizeDiagram (mkWidth (realToFrac width)) 
                                 (topBarDiag (realToFrac width) 
                                             (realToFrac height))
         makeInfoPage i = rasterizeDiagram (mkWidth (realToFrac width)) (infoPageDiag i)
         infoPage = makeInfoPage <$> info

      dpmsProp <- graphicsConfig (graphicCardHandle card) <| do
         getPropertyM conn
            >.-.> filter (\p -> propertyName (propertyMeta p) == "DPMS")
            >..-.> const []

      let 
         dpmsPage = rasterizeDiagram (mkWidth 200)
            <| case dpmsProp of
               []    -> customPage ["DPMS not found"]
               (p:_) -> case propertyType (propertyMeta p) of
                  PropEnum xs -> customPage
                     <| fmap (\(n,lbl) -> show n ++ " - " ++ lbl) xs
                  _           -> customPage ["Invalid DPMS property type"]

      sysFork "DPMS state" <| forever <| do
         s <- atomically $ takeTMVar dpmsState
         graphicsConfig (graphicCardHandle card) <| do
            forM_ dpmsProp $ \prop -> do
               setPropertyM conn (propertyID (propertyMeta prop)) s
            commitConfig NonAtomic Commit Synchronous AllowFullModeset
               >..~!> (\err -> lift $ sysWarning <| "Cannot set DPMS: " ++ show err)

      oldState <- newTVarIO (0,0,PageNone)
         

      initRenderingEngine card ctrl mode 3 [WaitDrawn,WaitPending] <| \_ gfb -> do
         let
            centerPos x = ( (floor width  - imageWidth x ) `div` 2
                          , (floor height - imageHeight x) `div` 2
                          )

            fullImg x   = ( 0
                          , 0
                          , imageWidth x
                          , imageHeight x
                          )

         do
            -- compare with old state, if nothing has changed, wait
            atomically $ do
               (mx,my) <- readTVar mousePos
               page' <- readTVar page
               (oldX,oldY,oldPage) <- readTVar oldState
               when (oldX == mx && oldY == my && oldPage == page')
                  retry
               writeTVar oldState (mx,my,page')

            (mx,my) <- readTVarIO mousePos
            liftIO <| fillFrame gfb bgColor
            readTVarIO page >>= \case
               PageNone -> liftIO <| blendImage gfb logo BlendAlpha (centerPos logo) (fullImg logo)

               PageInfo -> case infoPage of
                  Just d  -> liftIO <| blendImage gfb d BlendAlpha (centerPos d) (fullImg d)
                  Nothing -> return ()

               PageGraphics -> do
                  graphicsPage card >.~!> \diag -> do
                     let d = rasterizeDiagram (mkWidth (realToFrac width)) diag
                     liftIO <| blendImage gfb d BlendAlpha (centerPos d) (fullImg d)

               PageDPMS -> do
                  liftIO <| blendImage gfb dpmsPage BlendAlpha (10,50) (fullImg dpmsPage)

               PageTerminal -> do
                  liftIO <| blendImage gfb abcd BlendAlpha (10,50) (fullImg abcd)

            liftIO <| blendImage gfb topBarDiagram BlendAlpha (0,0) (fullImg topBarDiagram)
            liftIO <| blendImage gfb ptr BlendAlpha (floor mx-ptrLen,floor my-ptrLen) (fullImg ptr)



   writeStrLn term "Done."

   -- wait for a key in the standard input (in the console) or ESC (in the graphic interface)
   sysFork "Terminal wait for key" <| do
      waitForKey term
      atomically <| writeTVar quitKey True
   
   atomically <| do
      q <- readTVar quitKey
      unless q retry

   writeStrLn term "Log:"
   sysLogPrint

   -- shutdown the computer
   powerOff
