{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

import Haskus.System

import qualified Haskus.Format.Binary.Buffer as B

import Haskus.System.Linux.Info
import Haskus.System.Linux.Graphics.State
import Haskus.System.Linux.Graphics.Mode
import Haskus.System.Linux.Graphics.Property
import Haskus.System.Linux.Graphics.AtomicConfig
import qualified Haskus.System.Linux.Internals.Input as Key
import Haskus.System.Graphics.Drawing
import Haskus.System.Graphics.Diagrams (mkWidth, rasterizeDiagram)
import Haskus.Utils.Embed
import Haskus.Utils.STM
import Haskus.Utils.Maybe
import Haskus.Format.String
import Haskus.Format.Text (textFormat,(%),shown)
import qualified Haskus.Utils.Map as Map

import Data.Char
import Codec.Picture.Types
import Graphics.Text.TrueType
import qualified Data.ByteString.Lazy as LBS

import Demo.Diagrams
import Demo.Graphics
import Demo.Art

rawlogo :: B.Buffer
rawlogo = B.Buffer $(embedFile "src/image/logo_transparent.png")

rawFontNormal :: B.Buffer
rawFontNormal = B.Buffer $(embedFile "src/demo/VeraMono.ttf")

rawFontBold :: B.Buffer
rawFontBold = B.Buffer $(embedFile "src/demo/VeraMoBd.ttf")

rawFontItalic :: B.Buffer
rawFontItalic = B.Buffer $(embedFile "src/demo/VeraMoIt.ttf")

rawFontBoldItalic :: B.Buffer
rawFontBoldItalic = B.Buffer $(embedFile "src/demo/VeraMoBI.ttf")

data Page
   = PageNone
   | PageInfo
   | PageGraphics
   | PageDPMS
   | PageTerminal
   | PageArt
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
       gradDef      = [ (0  , PixelRGBA8 0 0x86 0xc1 255)
                      , (0.5, PixelRGBA8 0xff 0xf4 0xc1 255)
                      , (1  , PixelRGBA8 0xFF 0x53 0x73 255)
                      ]
       rainbowTexture   = Just (linearGradientTexture gradDef (V2 40 40) (V2 130 130))

   term <- defaultTerminal
   sys  <- defaultSystemInit
   let dm = systemDeviceManager sys

   info <- fromVariantHead <$> runFlowT systemInfo

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
      getMousePosIO        = readTVarIO mousePos
      updateMouseRel dx dy = changeMousePos (\(x,y) -> (x+fromIntegral dx,y+fromIntegral dy))
      updateMouseAbsX v    = changeMousePos (\(_,y) -> (fromIntegral v * wf,y))
      updateMouseAbsY v    = changeMousePos (\(x,_) -> (x,fromIntegral v * hf))
   -------------------------------------------------


   -------------------------------------------------
   -- terminal management
   -------------------------------------------------
   termContents <- newTVarIO [[TextRange fontBold (PointSize 12) "Enter commands in the terminal:" redTexture]]
   termStr <- newTVarIO ""

   let
      termAppend s = do
         modifyTVar termStr (++ s)
         needRedraw
      termModifyContents f = do
         modifyTVar termContents f
         needRedraw
      termModifyCurrent f = do
         modifyTVar termStr f
         needRedraw
      termAppendStyled s = termModifyContents $ \case
            []     -> [[s]]
            (x:xs) -> (x ++ [s]):xs
      termNewLine = termModifyContents ([]:)
      termPrompt = termAppendStyled (TextRange fontBoldItalic (PointSize 12) "$> " blackTexture)
      termBackErase = termModifyCurrent $ \case
            "" -> ""
            s  -> init s
         
      termEnter  = do
         s <- readTVar termStr
         termAppendStyled (TextRange fontNormal (PointSize 12) s blackTexture)
         termNewLine
         case s of
            "exit" -> do
               termAppendStyled (TextRange fontBoldItalic (PointSize 12) "Exiting..." blackTexture)
               termNewLine
               mustQuit
            "help" -> do
               termAppendStyled (TextRange fontBoldItalic (PointSize 16) "You are on your own for now..." rainbowTexture)
               termNewLine
            "uname" -> do
               
               let res = case info of
                     Just info' -> fromCStringBuffer (systemName info') ++ " " 
                           ++ fromCStringBuffer (systemRelease info')
                           ++ " (" ++ fromCStringBuffer (systemMachine info') ++ ") - "
                           ++ fromCStringBuffer (systemVersion info')
                     Nothing -> "Information unavailable"
               termAppendStyled (TextRange fontItalic (PointSize 12) res blackTexture)
               termNewLine
            _       -> do
               termAppendStyled (TextRange fontItalic (PointSize 12) "Unknown command" redTexture)
               termNewLine
         termPrompt
         writeTVar termStr ""
         needRedraw
      termGetContents = do
         cs <- readTVar termContents
         c  <- readTVar termStr
         let c' = [TextRange fontNormal (PointSize 12) c blackTexture]
         return $ case cs of
            []     -> [c']
            (x:xs) -> (x++c'):xs

   atomically (termNewLine >> termPrompt)
   -------------------------------------------------

         
   -------------------------------------------------
   -- DPMS management
   -------------------------------------------------
   dpmsState <- atomically $ newEmptyTMVar

   let dpmsSet = putTMVar dpmsState
   -------------------------------------------------

   -------------------------------------------------
   -- Char map management
   -------------------------------------------------
   let
      charMapFr = charMapEn . f
         where f = \case
                  Q         -> A
                  A         -> Q
                  M         -> SemiColon
                  SemiColon -> M
                  Z         -> W
                  W         -> Z
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
         k         -> fmap toLower (show k)
   -------------------------------------------------

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
               F5  -> changePage PageArt
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
                        BackSpace -> termBackErase
                        Enter     -> termEnter
                        _         -> termAppend (charMapFr x)
                  _        -> return ()
         _                              -> return ()

   cards <- loadGraphicCards dm

   forM_ cards <| \card -> do

      state <- flowAssertQuiet "Read graphics state"
                  <| readGraphicsState (graphicCardHandle card)

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

      let defaultCtrl = do
            encId  <- connectorEncoderID conn
            enc    <- Map.lookup encId (graphicsEncoders state)
            ctrlId <- encoderControllerID enc
            Map.lookup ctrlId (graphicsControllers state)

          Just ctrl = case defaultCtrl of
            -- we already have a connected controller, use it
            Just c  -> Just c
            -- we need to select a controller and an encoder
            Nothing -> do
               encId  <- headMaybe (connectorPossibleEncoderIDs conn)
               enc    <- Map.lookup encId (graphicsEncoders state)
               ctrlId <- headMaybe (encoderPossibleControllers enc)
               Map.lookup ctrlId (graphicsControllers state)
            

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
         evalCatchFlowT (const (return []))
            <|  filter (\p -> propertyName (propertyMeta p) == "DPMS")
            <|| getPropertyM conn

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
               |> evalCatchFlowT (\err -> lift $ sysWarning (textFormat ("Cannot set DPMS: " % shown) err))


      initRenderingEngine card ctrl mode conn 3 [WaitDrawn,WaitPending] <| \_ gfb -> do
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

               PageGraphics -> runFlowT_ <| do
                  diag <- graphicsPage card
                  let d = rasterizeDiagram (mkWidth (realToFrac width)) diag
                  liftIO <| blendImage gfb d BlendAlpha (centerPos d) (fullImg d)

               PageArt -> do
                  let d = makeArt 1520476193207 60 60 12
                  liftIO <| blendImage gfb d BlendAlpha (centerPos d) (fullImg d)

               PageDPMS -> do
                  liftIO <| blendImage gfb dpmsPage BlendAlpha (10,50) (fullImg dpmsPage)

               PageTerminal -> do
                  termLines <- atomically termGetContents
                  let abcd = renderDrawing 1000 700 (PixelRGBA8 255 255 255 255)
                              . withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $ do
                                  forM_ (reverse termLines `zip` [0..]) $ \(rs,i) ->
                                    printTextRanges (V2 20 (20*i + 20)) rs

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
