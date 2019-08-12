{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

import Haskus.System

import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.System.Linux.Graphics.Entities
import Haskus.System.Graphics.Drawing
import Haskus.System.Graphics.Diagrams (lc,fc,rect,dims2D,rasterizeDiagram)
import Haskus.System.Graphics.Config
import Haskus.Number.Word
import Haskus.UI.Color

import Codec.Picture.Types
import System.Random
import Data.Array.MArray
import Data.Array.IO
import Data.Array
import qualified Data.IORef as IORef
import System.Clock
import Data.Tuple
import Data.Bifunctor

main :: IO ()
main = runSys' do
   sys   <- defaultSystemInit
   term  <- defaultTerminal
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   forM_ cards \card -> do
      forEachConnectedDisplay card \conn display -> do
         -- get a primary plane
         plane <- getEntities card
                  |> assertE "Can't get entities"
                  ||> entitiesPlanes
                  ||> filter (\p -> planeType p == Primary)
                  ||> head

         -- test that the plane supports the pixel format we want to use
         let pixelFormat = makePixelFormat XRGB8888 LittleEndian
         unless (pixelFormat `elem` planeFormats plane) do
            writeStrLn term "Pixel format not supported!"

         -- create and fill a fullscreen frame for the preferred mode (first in
         -- the list)
         let mode = head (displayModes display)

         -- select a controller
         let ctrlID = planePossibleControllers plane
                      |> head


         let
            -- build the pipeline
            initPipeline frame = assertLogShowErrorE "Initialize pipeline" <| do
               withModeBlob card mode \modeBlobID ->
                  configureGraphics card Commit EnableVSync EnableFullModeset do
                     setConnectorSource conn ctrlID -- connector  <-> controller
                     setPlaneTarget plane ctrlID    -- controller <-> plane
                     setPlaneSource plane frame     -- plane      <-> frame
                     -- sizes and modes
                     setPlaneSize plane (frameWidth frame) (frameHeight frame)
                     setPlaneSourceSize plane (frameWidth frame) (frameHeight frame)
                     setMode ctrlID modeBlobID
                     -- enable the controller
                     enableController ctrlID True

            -- switch frame
            switchFrame frame = assertLogShowErrorE "Switch frame" <| do
               configureGraphics card Commit EnableVSync DisableFullModeset do
                  setPlaneSource plane frame

         frame1 <- createGenericFullScreenFrame card mode pixelFormat 0
         frame2 <- createGenericFullScreenFrame card mode pixelFormat 0

         let
            gameWidth  = 10 :: Int
            gameHeight = 20 :: Int

            shapeI = newShape 1 4
                        [ 1
                        , 1
                        , 1
                        , 1
                        ]

            shapeO = newShape 2 2
                        [ 1, 1
                        , 1, 1
                        ]
            shapeL = newShape 2 3
                        [ 1, 0
                        , 1, 0
                        , 1, 1
                        ]
            shapeS = newShape 3 2
                        [ 0, 1, 1
                        , 1, 1, 0
                        ]
            shapeZ = newShape 3 2
                        [ 1, 1, 0
                        , 0, 1, 1
                        ]
            shapeJ = newShape 2 3
                        [ 0, 1
                        , 0, 1
                        , 1, 1
                        ]
            shapeT = newShape 3 2
                        [ 1, 1, 1
                        , 0, 1, 0
                        ]

            --shapes = [shapeI, shapeO, shapeL, shapeS, shapeZ, shapeJ, shapeT]
            shapes = [shapeI, shapeO]


            randomShape = randomRIO (0,length shapes-1)
                           |> liftIO
                           ||> (shapes !!)

            randomPosX w = randomRIO (0,gameWidth - 1 - w) |> liftIO

            newIORef     x = liftIO (IORef.newIORef x)
            writeIORef r x = liftIO (IORef.writeIORef r x)
            readIORef  r   = liftIO (IORef.readIORef r)

         gameArray <- liftIO <| newArray @IOUArray ((0,0),(gameWidth-1,gameHeight-1)) (0 :: Word32)
         currentShape <- newIORef =<< randomShape
         currentPosX  <- newIORef 0
         currentPosY  <- newIORef (-1)
         currentFallPeriod <- newIORef (TimeSpec 1 0)
         lastDropTime <- newIORef (TimeSpec 0 0)

         let
            readGameArray x y    = liftIO (readArray gameArray (x,y))
            writeGameArray x y e = liftIO (writeArray gameArray (x,y) e)

            popShape = do
               sh <- randomShape
               let w = fst (snd (bounds (shapeArray sh)))
               writeIORef currentShape sh
               writeIORef currentPosX  =<< randomPosX w
               writeIORef currentPosY 0
               valid <- isValidMove id id
               when (not valid) do
                  writeStrLn term "Game over"
                  powerOff_

            isValidPosition x y
               | x < 0 || x >= gameWidth = pure False
               | y >= gameHeight         = pure False
               | y < 0                   = pure True
               | otherwise = do
                  c <- readGameArray x y
                  pure (c == 0)

            isValidShapePosition sh x y = go (assocs (shapeArray sh))
               where
                  go []                   = pure True
                  go ((_,False):rs)       = go rs
                  go (((sx,sy),True):rs)  = do
                     let nx = sx+x
                     let ny = sy+y
                     isValidPosition nx ny >>= \case
                        True  -> go rs
                        False -> pure False

            isValidMove fx fy = do
               sx <- readIORef currentPosX
               sy <- readIORef currentPosY
               sh <- readIORef currentShape
               isValidShapePosition sh (fx sx) (fy sy)

            tryMove fx fy = do
               b <- isValidMove fx fy
               when b do
                  sx <- readIORef currentPosX
                  sy <- readIORef currentPosY
                  writeIORef currentPosX (fx sx)
                  writeIORef currentPosY (fy sy)
               pure b


            block  c  = rasterizeDiagram (dims2D @Float 20 20) do
                           rect 20 20 |> lc white |> fc c

            redBlock  = block red
            blueBlock = block blue
            grayBlock = block gray

         popShape -- fix invalid shape popping

         inputs <- loadInputDevices (systemDeviceManager sys)
         forM_ inputs \inp -> onEvent (inputDeviceBundles inp) \(InputEventBundle events) -> do
            forM_ (fmap inputEventType events) \case
               InputKeyEvent KeyPress k -> case k of
                  Esc      -> powerOff_
                  KeyLeft  -> void <| tryMove (\x -> x-1) id
                  KeyRight -> void <| tryMove (+1) id
                  Down     -> void <| replicateM 4 (tryMove id (+1))
                  _        -> pure ()
               _ -> pure ()

         let
            -- margins in blocks (must not be 0)
            leftMargin = 5
            topMargin  = 5

            drawBlock frame x y b = liftIO do
               let
                  fullImg i = ( 0
                              , 0
                              , imageWidth i
                              , imageHeight i
                              )
               blendImage frame b BlendAlpha ((x+leftMargin)*25,(y+topMargin)*25) (fullImg b)
            fixShape = do
               cx <- readIORef currentPosX
               cy <- readIORef currentPosY
               sh <- readIORef currentShape
               forM_ (assocs (shapeArray sh)) \((x,y),c) -> do
                  when c <| writeGameArray (cx+x) (cy+y) 1

               -- remove full lines
               let checkFullLine y = go [0..gameWidth-1]
                     where
                        go []     = pure True
                        go (x:xs) = do
                           c <- readGameArray x y
                           if c /= 0
                              then go xs
                              else pure False
               fulls <- forMaybeM [0..snd (snd (bounds (shapeArray sh)))] \y -> do
                  checkFullLine (y+cy) >>= \case
                     True  -> pure <| Just (y+cy)
                     False -> pure Nothing
               let
                  copyLine ysrc ydest
                     | ysrc < 0  = forM_ [0..gameWidth-1] \x -> writeGameArray x ydest 0
                     | otherwise = forM_ [0..gameWidth-1] \x -> writeGameArray x ydest =<< readGameArray x ysrc

                  -- copy a line <= ysrc, skipping removed ones
                  copyLineFrom ysrc [] ydest     = copyLine ysrc ydest
                  copyLineFrom ysrc (y:ys) ydest
                     | y == ysrc = copyLineFrom (y-1) ys ydest
                     | otherwise = copyLine ysrc ydest


                  removeLines _ _ 0 = do
                     forM_ [0..gameWidth-1] \x -> do
                        writeGameArray x 0 0

                  removeLines off (fy:ys) y
                     | y == fy = do
                        let off' = off+1
                        copyLineFrom (y-off') ys y
                        removeLines off' ys (y-1)

                  removeLines off ys y = do
                     copyLineFrom (y-off) ys y
                     removeLines off ys (y-1)

               removeLines 0 (reverse fulls) (gameHeight-1)
                  

            render frame = do
               liftIO (fillFrame frame 0)

               -- draw the game array
               forM_ [0..gameHeight] \y -> do
                  drawBlock frame (-1) y grayBlock
                  drawBlock frame gameWidth y grayBlock
               forM_ [0..(gameWidth-1)] \x -> do
                  drawBlock frame x gameHeight grayBlock

               -- draw the current blocks
               as <- liftIO (getAssocs gameArray)
               forM_ as \((x,y),c) -> do
                  when (c /= 0) <| drawBlock frame x y redBlock

               -- draw the current falling shape
               cx <- readIORef currentPosX
               cy <- readIORef currentPosY
               sh <- readIORef currentShape
               forM_ (assocs (shapeArray sh)) \((x,y),c) -> do
                  when c <| drawBlock frame (cx+x) (cy+y) blueBlock

            renderLoop b = do
               -- update the game
               t <- liftIO (getTime Monotonic)
               lastt <- readIORef lastDropTime
               period <- readIORef currentFallPeriod
               when ((t-lastt) > period) do
                  hasMoved <- tryMove id (+1)
                  when (not hasMoved) do
                     fixShape
                     popShape
                  writeIORef lastDropTime t

               let frame = if b then frame1 else frame2
               render frame
               switchFrame frame
               renderLoop (not b)

         render frame1
         initPipeline frame1

         sysFork "Render loop" (renderLoop False)

   waitForKey term
   powerOff


newtype Shape = Shape
   { shapeArray :: Array (Int,Int) Bool
   }

newShape :: Int -> Int -> [Int] -> Shape
newShape x y xs = Shape (transpose (listArray ((0,0),(y-1,x-1)) (fmap (/=0) xs)))

transpose :: Array (Int,Int) e -> Array (Int,Int) e
transpose a = array (second swap (bounds a)) (fmap (first swap) (assocs a))
