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
import qualified Haskus.System.Linux.Internals.Input as Key
import Haskus.System.Graphics.Drawing
import Haskus.System.Graphics.Diagrams (mkWidth, rasterizeDiagram)
import Haskus.Utils.Embed
import Haskus.Utils.STM
import Haskus.Utils.Monad
import qualified Haskus.Utils.Map as Map

import Data.Char
import Codec.Picture.Types
import Graphics.Text.TrueType
import qualified Data.ByteString.Lazy as LBS

import Demo.Diagrams
import Demo.Graphics

rawlogo :: B.Buffer
rawlogo = B.Buffer $(embedFile "src/image/logo_transparent.png")

rawFontNormal :: B.Buffer
rawFontNormal = B.Buffer $(embedFile "src/Demo/VeraMono.ttf")

rawFontBold :: B.Buffer
rawFontBold = B.Buffer $(embedFile "src/Demo/VeraMoBd.ttf")

rawFontItalic :: B.Buffer
rawFontItalic = B.Buffer $(embedFile "src/Demo/VeraMoIt.ttf")

rawFontBoldItalic :: B.Buffer
rawFontBoldItalic = B.Buffer $(embedFile "src/Demo/VeraMoBI.ttf")

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

   let
   -- fonts
      loadFont             = decodeFont . LBS.fromStrict . B.bufferUnpackByteString
      Right fontNormal     = loadFont rawFontNormal
      Right fontBold       = loadFont rawFontBold
      Right fontBoldItalic = loadFont rawFontBoldItalic
      Right fontItalic     = loadFont rawFontItalic
   
   let blackTexture = Just . uniformTexture $ PixelRGBA8 0 0 0 255
       redTexture   = Just . uniformTexture $ PixelRGBA8 255 0 0 255

   term <- defaultTerminal
   sys  <- defaultSystemInit
   let dm = systemDeviceManager sys

   -- wait for mouse driver to be loaded (FIXME: use plug-and-play detection)
   threadDelaySec 2

   -------------------------------------------------
   -- redrawing management
   -------------------------------------------------
   needRedrawVar <- newTVarIO True
   let needRedraw   = writeTVar needRedrawVar True
       redrawNeeded = readTVar needRedrawVar
       redrawing    = writeTVar needRedrawVar False
   -------------------------------------------------


   -------------------------------------------------
   -- system exit management
   -------------------------------------------------
   quitKey <- newTVarIO False

   let mustQuit     = writeTVar quitKey True
       quitRequired = readTVar quitKey

   -------------------------------------------------


   -------------------------------------------------
   -- page management
   -------------------------------------------------
   page    <- newTVarIO PageNone

   let changePage newp = do
         oldp <- readTVar page
         when (oldp /= newp) $ do
            writeTVar page newp
            needRedraw
       currentPage   = readTVar page
       currentPageIO = readTVarIO page
   -------------------------------------------------


   -------------------------------------------------
   -- mouse management
   -------------------------------------------------
   mousePos <- newTVarIO (250.0,250.0)

   let
      wf = 1024 / 0x7FFF :: Float
      hf = 768  / 0x7FFF :: Float
      changeMousePos f     = do
         modifyTVar mousePos f
         needRedraw
      getMousePos          = readTVar mousePos
      getMousePosIO        = readTVarIO mousePos
      updateMouseRel dx dy = changeMousePos (\(x,y) -> (x+fromIntegral dx,y+fromIntegral dy))
      updateMouseAbsX v    = changeMousePos (\(_,y) -> (fromIntegral v * wf,y))
      updateMouseAbsY v    = changeMousePos (\(x,_) -> (x,fromIntegral v * hf))
   -------------------------------------------------


   -------------------------------------------------
   -- terminal management
   -------------------------------------------------
   termContents <- newTVarIO [[TextRange fontBold (PointSize 12) "Welcome to the terminal!" redTexture]]
   termStr <- newTVarIO ""

   let
      termAppend s = do
         modifyTVar termStr (++ s)
         needRedraw
      termNewLine  = do
         s <- readTVar termStr
         modifyTVar termContents ([TextRange fontNormal (PointSize 12) s blackTexture]:)
         writeTVar termStr ""
         needRedraw
      termGetContents = do
         cs <- readTVar termContents
         c  <- readTVar termStr
         let c' = [TextRange fontNormal (PointSize 12) c blackTexture]
         return (c':cs)
   -------------------------------------------------

         
   -------------------------------------------------
   -- DPMS management
   -------------------------------------------------
   dpmsState <- atomically $ newEmptyTMVar

   let dpmsSet = putTMVar dpmsState
   -------------------------------------------------

   let
      charMapFr = charMapEn . f
         where f = \case
                  Q         -> A
                  A         -> Q
                  M         -> SemiColon
                  SemiColon -> M
                  k         -> k

      charMapEn = \case
         Key.Space -> " "
         Key0      -> "0"
         Key1      -> "1"
         Key2      -> "2"
         Key3      -> "3"
         Key4      -> "4"
         Key5      -> "5"
         Key6      -> "6"
         Key7      -> "7"
         Key8      -> "8"
         Key9      -> "9"
         Minus     -> "-"
         Equal     -> "="
         Tab       -> "    "
         SemiColon -> ";"
         Slash     -> "/"
         BackSlash -> "\\"
         Comma     -> ","
         Dot       -> "."
         k         -> show k

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
            p <- currentPage
            case k of
               Esc -> case p of
                  PageNone -> mustQuit
                  _        -> changePage PageNone
               F1  -> changePage PageInfo
               F2  -> changePage PageGraphics
               F3  -> changePage PageDPMS
               F4  -> changePage PageTerminal
               x   -> case p of
                  PageDPMS -> do
                     void (tryTakeTMVar dpmsState)
                     case x of
                        Key0 -> dpmsSet 0
                        Key1 -> dpmsSet 1
                        Key2 -> dpmsSet 2
                        Key3 -> dpmsSet 3
                        _    -> return ()
                  PageTerminal -> do
                     case x of
                        Enter -> termNewLine
                        _     -> termAppend (charMapFr x)
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
            -- wait if redrawing is not needed
            atomically $ do
               redrawNeeded >>= \case
                  False -> retry
                  True  -> redrawing

            (mx,my) <- getMousePosIO
            liftIO <| fillFrame gfb bgColor
            currentPageIO >>= \case
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
                  termLines <- atomically termGetContents
                  let abcd = renderDrawing 1000 800 (PixelRGBA8 255 255 255 255)
                              . withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $ do
                                  printTextRanges (V2 20 40)
                                    [ TextRange fontNormal (PointSize 12) "A simple text test! " blackTexture
                                    , TextRange fontBold   (PointSize 12) "Bold text" redTexture
                                    ]
                                  printTextRanges (V2 20 60)
                                    [ TextRange fontItalic     (PointSize 12) "Another simple text test " blackTexture
                                    , TextRange fontBoldItalic (PointSize 12) "C'était comme ça Â\233 €" redTexture
                                    ]
                                  printTextAt fontNormal (PointSize 12) (V2 20 80)
                                       ([chr i | i <- [1..100]])
                                  printTextAt fontNormal (PointSize 12) (V2 20 100)
                                       ([chr i | i <- [101..200]])
                                  printTextAt fontNormal (PointSize 12) (V2 20 120)
                                       ([chr i | i <- [201..255]])
                                  printTextAt fontBold (PointSize 12) (V2 20 140)
                                       ([chr i | i <- [1..100]])
                                  printTextAt fontBold (PointSize 12) (V2 20 160)
                                       ([chr i | i <- [101..200]])
                                  printTextAt fontBold (PointSize 12) (V2 20 180)
                                       ([chr i | i <- [201..255]])
                                  forM_ (reverse termLines `zip` [0..]) $ \(rs,i) ->
                                    printTextRanges (V2 20 (20*i + 200)) rs

                  liftIO <| blendImage gfb abcd BlendAlpha (10,50) (fullImg abcd)

            liftIO <| blendImage gfb topBarDiagram BlendAlpha (0,0) (fullImg topBarDiagram)
            liftIO <| blendImage gfb ptr BlendAlpha (floor mx-ptrLen,floor my-ptrLen) (fullImg ptr)



   writeStrLn term "Done."

   -- wait for a key in the standard input (in the console) or ESC (in the graphic interface)
   sysFork "Terminal wait for key" <| do
      waitForKey term
      atomically <| mustQuit
   
   atomically <| do
      q <- quitRequired
      unless q retry

   writeStrLn term "Log:"
   sysLogPrint

   -- shutdown the computer
   powerOff
