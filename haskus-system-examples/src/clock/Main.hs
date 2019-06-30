{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

import Haskus.System
import Clock.Render

import Haskus.Format.Binary.Endianness
import qualified Haskus.Format.Binary.BitSet as BitSet

import Haskus.System.Linux.Time
import Haskus.System.Linux.Graphics.Capability
import Haskus.System.Linux.Graphics.State
import Haskus.System.Linux.Graphics.Mode
import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.System.Linux.Graphics.Helper
import Haskus.System.Linux.Graphics.Entities
import Haskus.System.Linux.Internals.Graphics
import Haskus.System.Graphics.Drawing
import Haskus.System.Graphics.Diagrams (rasterizeDiagram,mkWidth)
import Haskus.Utils.Maybe
import Haskus.Utils.Text ((%),left,center,stext,text,hex,textFormat,char)
import Formatting ((%.))
import qualified Haskus.Utils.Map as Map

import Codec.Picture.Types

main :: IO ()
main = runSys' do

   term <- defaultTerminal
   sys  <- defaultSystemInit

   memMap <- getProcessMemoryMap sys
               |> assertE "getProcessMemoryMap"
   showProcessMemoryMap term memMap

   cards <- loadGraphicCards (systemDeviceManager sys)

   forM_ cards \card -> do
      let fd = graphicCardHandle card

      void <| sysLogSequence "Load graphic card" do
         cap  <- (fd `supports` CapHostBuffer)
                     |> assertE "Test card capabilities"
         sysAssert "Card supports host buffers" cap
         
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
               Connected d -> not (null <| videoModes d)
               _           -> False
            validConns = filter isValid conns

            -- select first connector
            conn = head validConns

            Connected connDev = connectorState conn

            -- select highest mode
            mode   = head (videoModes connDev)
            fmt    = makePixelFormat XRGB8888 LittleEndian
            width  = fromIntegral <| modeHorizontalDisplay mode
            height = fromIntegral <| modeVerticalDisplay mode

         ~gfb1@(GenericFrame fb1 [buf]) <- initGenericFrameBuffer card mode fmt
         ~gfb2@(GenericFrame _ [_])     <- initGenericFrameBuffer card mode fmt

         let defaultCtrl = do
               encId  <- connectorEncoderID conn
               enc    <- Map.lookup encId encoderMap
               ctrlId <- encoderControllerID enc
               Map.lookup ctrlId (graphicsControllers state)

             Just ctrl = case defaultCtrl of
               -- we already have a connected controller, use it
               Just c  -> Just c
               -- we need to select a controller and an encoder
               Nothing -> do
                  encId  <- headMaybe (connectorPossibleEncoderIDs conn)
                  enc    <- Map.lookup encId encoderMap
                  ctrlId <- headMaybe (encoderPossibleControllers enc)
                  Map.lookup ctrlId (graphicsControllers state)

         writeStrLn term (show (fbPitch (mappedSurfaceInfo buf)))

         -- set mode and connectors
         setController ctrl (SetSource fb1) [conn] (Just mode)
            |> assertE "Set controller"

         -- frame switch
         let 
            setFb fb = switchFrame ctrl fb (BitSet.fromList [SwitchFrameGenerateEvent]) 0
                        |> assertE "Switch frame"

            --clp        = Clip 0 0 (modeHorizontalDisplay mode - 1) (modeVerticalDisplay mode - 1)
            --dirtyFb fb = sysCallAssertQuiet "Dirty frame" <|
            --               dirtyFrameBuffer fd fb (Dirty [clp])

         setFb fb1

         let
            clockDiagram h m s = rasterizeDiagram (mkWidth width) (clockDiag width height h m s)

            mainLoop !b = do
               tv <- assertE "getTimeOfDay" sysGetTimeOfDay
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

         void <| sysFork "Main display loop" <| mainLoop False

   sysLogPrint

   waitForKey term
   powerOff

showProcessMemoryMap :: Terminal -> [MemoryMapEntry] -> Sys ()
showProcessMemoryMap term x = do
   writeTextLn term (textFormat
      ( (center 25 ' ' %. text)
      % " "
      % text
      % " "
      % text
      )
      "Memory range"
      "Flgs"
      "Mapping"
      )

   let hasReadPerm [] = False
       hasReadPerm (PermRead:_) = True
       hasReadPerm (_:xs) = hasReadPerm xs

   let hasWritePerm [] = False
       hasWritePerm (PermWrite:_) = True
       hasWritePerm (_:xs) = hasWritePerm xs

   let hasExecPerm [] = False
       hasExecPerm (PermExec:_) = True
       hasExecPerm (_:xs) = hasExecPerm xs

   forM_ x <| \y -> do
      writeTextLn term (textFormat
         ((left 12 '0' %. hex)
         % "-"
         % (left 12 '0' %. hex)
         % " "
         % char % char % char % char
         % " "
         % stext
         )
         (entryStartAddr y)
         (entryStopAddr y)
         (if hasReadPerm (entryPerms y) then 'r' else '-')
         (if hasWritePerm (entryPerms y) then 'w' else '-')
         (if hasExecPerm (entryPerms y) then 'x' else '-')
         (case entrySharing y of
            Private -> 'p'
            Shared  -> 's'
         )
         (case entryType y of
            AnonymousMapping  -> ""
            NamedMapping s    -> textFormat ("[" % stext % "]") s
            fm@FileMapping {} -> textFormat (stext % " @ " % hex)
                                 (fileMappingPath fm)
                                 (fileMappingOffset fm)
         )
         )
   writeStrLn term (show x)

