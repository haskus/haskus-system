{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}

-- | Manage graphics devices
module Haskus.System.Graphics
   ( -- * DRM Client
     setClientCapabilityWarn
     -- * Card
   , GraphicCard (..)
   , InvalidCard (..)
   , loadGraphicCards
     -- * Entities
   , EntitiesIDs (..)
   , EntitiesMap (..)
   , Entities (..)
   , getEntities
   , getEntitiesMap
   , getEntitiesIDs
   , forEachConnectedDisplay
   , forEachConnectedDisplay_
   -- * Generic buffers and frames
   , GenericBuffer (..)
   , createGenericBuffer
   , freeGenericBuffer
   , withGenericBufferPtr
   , createGenericFrame
   , createGenericFullScreenFrame
   , freeGenericFrame
   -- * Frames
   , createFrame
   , freeFrame
   , dirtyFrame
     -- * Generic rendering engine
   , RenderingEngine (..)
   , BufferingState (..)
   , FrameWait (..)
   , initRenderingEngine
   )
where

import Haskus.System.Sys
import Haskus.System.Devices
import Haskus.System.Process
import Haskus.System.Event
import Haskus.Format.Binary.BitSet (BitOffset(..), BitSet)
import qualified Haskus.Format.Binary.BitSet as BitSet
import qualified Haskus.Utils.Text as Text
import Haskus.Utils.Text (textFormat,shown,(%))
import Haskus.Format.Binary.Storable
import Haskus.Utils.Flow
import Haskus.Utils.List (isPrefixOf)
import Haskus.Utils.Maybe
import Haskus.Utils.STM
import Haskus.Format.Binary.Word
import Haskus.System.Linux.Handle
import Haskus.System.Linux.FileSystem.ReadWrite

import Haskus.System.Linux.Internals.Graphics
import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Graphics.State
import Haskus.System.Linux.Graphics.Capability
import Haskus.System.Linux.Graphics.GenericBuffer
import Haskus.System.Linux.Graphics.Helper
import Haskus.System.Linux.Graphics.Mode
import Haskus.System.Linux.Graphics.Entities
import Haskus.System.Linux.Graphics.Object
import Haskus.System.Linux.Graphics.Frame
import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.System.Linux.Graphics.Event as Graphics

import System.FilePath (takeBaseName)

-------------------------------------------------------------
-- Card
-------------------------------------------------------------

-- | Graphic card
data GraphicCard = GraphicCard
   { graphicCardPath    :: DevicePath           -- ^ Path to the graphic card in SysFS
   , graphicCardDev     :: Device               -- ^ Device major/minor to create the device file descriptor
   , graphicCardID      :: Int                  -- ^ Card identifier
   , graphicCardHandle  :: Handle               -- ^ Device handle
   , graphicCardChan    :: TChan Graphics.Event -- ^ Event stream
   }

-- - Invalid card error
data InvalidCard
   = InvalidCardHandle -- ^ The card handle is invalid
   deriving (Show,Eq)

-- | Return detected graphics devices
--
-- Graphic cards are /class/drm/cardN directories in SysFS where N is the card
-- identifier. In this directory, the dev file contains device major/minor to
-- create appropriate device node.
loadGraphicCards :: DeviceManager -> Sys [GraphicCard]
loadGraphicCards dm = sysLogSequence "Load graphic cards" $ do

   devs <- listDevicesWithClass dm "drm"
   let
      isCard (p,_) = "card" `isPrefixOf` takeBaseName (Text.unpack p)
      hasDevice (p,d) = case deviceDevice d of
         Nothing -> Nothing
         Just x  -> Just (p,x)
      devs' = filter isCard (mapMaybe hasDevice devs)

      readDevInfo devpath dev = do
         let cardID = read (drop 4 (takeBaseName (Text.unpack devpath)))
         hdl <- getDeviceHandle dm dev
         -- We support these capabilities
         lift <| setClientCapabilityWarn hdl ClientCapStereo3D            True
         lift <| setClientCapabilityWarn hdl ClientCapUniversalPlanes     True
         lift <| setClientCapabilityWarn hdl ClientCapAtomic              True
         lift <| setClientCapabilityWarn hdl ClientCapAspectRatio         True
         lift <| setClientCapabilityWarn hdl ClientCapWritebackConnectors True
         -- Create the DRM event reader thread
         GraphicCard devpath dev cardID hdl
            <$> lift (newEventWaiterThread hdl)

   forMaybeM devs' <| \(devpath,dev) -> do
      readDevInfo devpath dev
         ||> Just
         |> catchEvalE (const (return Nothing))

-------------------------------------------------------------
-- Entities
-------------------------------------------------------------


-- | Get card entities IDs
getEntitiesIDs :: MonadInIO m => GraphicCard -> Excepts '[InvalidCard] m EntitiesIDs
getEntitiesIDs card =
   getHandleEntitiesIDs (graphicCardHandle card)
      |> catchE (\InvalidHandle -> failureE InvalidCardHandle)

-- | Get card entities
getEntities :: MonadInIO m => GraphicCard -> Excepts '[InvalidCard] m Entities
getEntities card =
   getHandleEntities (graphicCardHandle card)
      |> catchE (\InvalidHandle -> failureE InvalidCardHandle)

-- | Get card entities as Maps (EntitiyID -> EntityInfo)
getEntitiesMap :: MonadInIO m => GraphicCard -> Excepts '[InvalidCard] m EntitiesMap
getEntitiesMap card =
   getHandleEntitiesMap (graphicCardHandle card)
      |> catchE (\InvalidHandle -> failureE InvalidCardHandle)

-- | Do something for each connected display (ignore errors)
forEachConnectedDisplay ::
   ( Monad m
   , MonadInIO m
   ) => GraphicCard -> (Connector -> Display -> m a) -> m [a]
forEachConnectedDisplay card action = do
   mentities <- runE (getEntities card)
   case mentities of
      VLeft _         -> pure []
      VRight entities -> do
         forMaybeM (entitiesConnectors entities) \conn -> do
            case connectorState conn of
               Disconnected           -> pure Nothing
               ConnectionUnknown      -> pure Nothing
               Connected videoDisplay -> action conn videoDisplay ||> Just

-- | Do something for each connected display (ignore errors)
forEachConnectedDisplay_ ::
   ( Monad m
   , MonadInIO m
   ) => GraphicCard -> (Connector -> Display -> m a) -> m ()
forEachConnectedDisplay_ card action = void (forEachConnectedDisplay card action)

-------------------------------------------------------------
-- Frames
-------------------------------------------------------------

-- | Create a frame
createFrame :: MonadInIO m => GraphicCard -> Word32 -> Word32 -> PixelFormat -> FrameFlags -> [FrameBuffer b] -> Excepts '[ErrorCode] m (Frame b)
createFrame card width height fmt flags fbs = handleCreateFrame (graphicCardHandle card) width height fmt flags fbs

-------------------------------------------------------------
-- Generic buffers and frames
-------------------------------------------------------------

-- | Create a generic buffer and map it in user memory.
--
-- The foreign pointer targets the memory mapping and is automatically unmapped.
createGenericBuffer :: MonadInIO m => GraphicCard -> Word32 -> Word32 -> Word32 -> Word32 -> Excepts '[ErrorCode] m GenericBuffer
createGenericBuffer card width height bpp flags = do
   handleCreateGenericBuffer (graphicCardHandle card) width height bpp flags

-- | Allocate a generic full-screen frame with the pixel format (dimensions are
-- given by the mode)
createGenericFullScreenFrame :: GraphicCard -> Mode -> PixelFormat -> Sys (Frame GenericBuffer)
createGenericFullScreenFrame card mode pixfmt =
   createGenericFrame card (fromIntegral <| modeHorizontalDisplay mode) (fromIntegral <| modeVerticalDisplay mode) pixfmt

-- | Allocate a generic frame with the given dimensions and pixel format
createGenericFrame :: GraphicCard -> Word32 -> Word32 -> PixelFormat -> Sys (Frame GenericBuffer)
createGenericFrame card width height pixfmt = do
   let
      fmt    = formatFormat pixfmt
      bpps   = formatBitDepth fmt
      flags  = 0

   fbs <- forM bpps $ \bpp -> do
      buf <- createGenericBuffer card width height bpp flags
               |> assertLogShowErrorE "Create a generic buffer"

      return <| FrameBuffer buf (genericBufferHandle buf) (genericBufferPitch buf) 0 0
   
   createFrame card width height pixfmt BitSet.empty fbs
      |> assertLogShowErrorE "Create frame"


-- | Free the generic buffers, the frame buffers and the frame
freeGenericFrame :: Frame GenericBuffer -> Sys ()
freeGenericFrame frame = do

   -- free the frame
   freeFrame frame
      |> assertLogShowErrorE "Free generic frame"

   -- frame buffers don't old any kernel entity

   -- free the generic buffers
   forM_ (frameBuffers frame) \fb -> do
      freeGenericBuffer (fbBuffer fb)
         |> assertLogShowErrorE "Free generic buffer"

-------------------------------------------------------------
-- Generic rendering engine
-------------------------------------------------------------


-- | Create a new thread reading input events and putting them in a TChan
newEventWaiterThread :: Handle -> Sys (TChan Graphics.Event)
newEventWaiterThread h = do
   let
      bufsz = 1000 -- buffer size

   ch <- newBroadcastTChanIO
   sysFork "Graphics event reader" <|
      allocaBytes bufsz <| \ptr -> forever <| runE <| do
         threadWaitRead h
         sz2 <- sysRead h ptr (fromIntegral bufsz)
         -- FIXME: we should somehow signal that an error occured and
         -- that we won't report future events (if any)
         evs <- peekEvents ptr (fromIntegral sz2)
         atomically $ mapM_ (writeTChan ch) evs
   return ch





-----------------------------------------------------------------------
-- Multi-buffering
-----------------------------------------------------------------------

-- | Rendering engine
--
-- Manage multi-buffering
data RenderingEngine = RenderingEngine
   { engineBuffereringState :: TVar (BufferingState (Frame GenericBuffer)) -- ^ Multi-buffering state
   }

-- | Multi-buffering state
data BufferingState a = BufferingState
   { fbShown    :: a          -- ^ Frame currently displayed
   , fbPending  :: Maybe a    -- ^ Frame pending for being displayed
   , fbDrawn    :: Maybe a    -- ^ Frame drawn but not yet submitted to be displayed
   , fbDrawable :: [a]        -- ^ Frames free to be drawed on
   }

-- | Rendering wait
data FrameWait
   = WaitPending  -- ^ Wait if there is a pending frame (that will be displayed asap)
   | WaitDrawn    -- ^ wait if there is already a rendered frame
   deriving (Show,Eq,Enum,BitOffset)


-- | Init the rendering engine
--
-- TODO: indicate failure (GenericBuffer not supported, frame allocation
-- error, etc.)
-- TODO: support multiple controllers/connectors
-- TODO: support connections/disconnections
-- TODO: better support for mode setting (change during rendering, etc.)
-- TODO: support accelerated buffers
initRenderingEngine :: GraphicCard -> Controller -> Mode -> Connector -> Word -> [FrameWait] -> (Mode -> Frame GenericBuffer -> Sys ()) -> Sys RenderingEngine
initRenderingEngine card ctrl mode conn nfb flags draw
   | nfb <= 2  = error "initRenderingEngine: require at least 2 buffers"
   | otherwise = do

      -- Check support for generic buffers
      let fd    = graphicCardHandle card
      sysLogSequence "Load graphic card" $ do
         let checkCap c t = do
               cap <- fd `supports` c
                        |> assertLogShowErrorE t
               sysAssert (t <> " supported") cap

         checkCap CapGenericBuffer              "Generic buffers"
         checkCap CapControllerInVBlankEvent "Controllers in VBlank events"

      -- TODO: support other formats
      let fmt = makePixelFormat XRGB8888 LittleEndian

      -- initialize generic frames
      frames <- forM [1..nfb] (const (createGenericFullScreenFrame card mode fmt))
      let (initFrame:otherFrames) = frames

      -- perform initial mode-setting
      setController ctrl (SetSource initFrame) [conn] (Just mode)
         |> assertE "Perform initial mode-setting"

      -- frame switching
      fbState <- newTVarIO (BufferingState
                  { fbShown    = initFrame
                  , fbPending  = Nothing
                  , fbDrawn    = Nothing
                  , fbDrawable = otherFrames
                  })

      fps <- newTVarIO (0 :: Word)

      onEvent (graphicCardChan card) $ \case
         -- on frame switch complete
         FrameSwitched evData
            -- we used to use user_data field to know which controller has
            -- switched its frame but in later kernel versions (5.1?) a reserved
            -- field has been dedicated to this in the event payload.
            -- We check that it is supported with `CapControllerInVBlankEvent`
            -- capability.
            | eventControllerId evData == fromIntegral (getObjectID ctrl) ->
               atomically $ do
                  s <- readTVar fbState
                  case fbPending s of
                     -- no pending frame? weird. we do nothing
                     Nothing -> return ()
                     -- shown   --> drawable
                     -- pending --> shown
                     Just p  -> do
                        writeTVar fbState $ s
                           { fbShown    = p
                           , fbPending  = Nothing
                           , fbDrawable = fbShown s : fbDrawable s
                           }
                        modifyTVar' fps (+1)
         _                          -> return ()


      -- on drawn frame
      sysFork "Multi-buffering manager" $ forever $ do
         frame <- atomically $ do
            s <- readTVar fbState
            -- check that the previous frame has switched
            -- and that we have a frame to draw
            case (fbPending s, fbDrawn s) of
               (Nothing, Just d) -> do
                  -- make the frame pending
                  writeTVar fbState $ s
                     { fbPending = Just d
                     , fbDrawn   = Nothing
                     }
                  return d
               -- otherwise retry
               _ -> retry

         -- switch to the pending frame
         switchFrame ctrl frame (BitSet.fromList [SwitchFrameGenerateEvent]) 0 -- empty user data
            |> assertE "Switch frame"
            

      let
         -- draw the next frame
         drawNext :: BitSet Word FrameWait -> (Frame GenericBuffer -> Sys ()) -> Sys ()
         drawNext wait f = do
            -- reserve next frame
            frame <- atomically $ do
                     s <- readTVar fbState
                     when (BitSet.member wait WaitPending && isJust (fbPending s)) retry
                     when (BitSet.member wait WaitDrawn   && isJust (fbDrawn   s)) retry
                     -- find a drawable frame
                     case fbDrawable s of
                        []     -> retry
                        (x:xs) -> do
                           writeTVar fbState (s { fbDrawable = xs })
                           return x
            -- draw it
            f frame
            -- indicate it is drawn
            atomically $ do
               s <- readTVar fbState
               case fbDrawn s of
                  Nothing -> writeTVar fbState (s { fbDrawn = Just frame })
                  Just d  -> do
                     -- drop the alreay drawn frame and set the newer
                     writeTVar fbState $ s
                        { fbDrawn    = Just frame
                        , fbDrawable = d : fbDrawable s
                        }
            -- switch to another thread
            yield

      -- Force the generation of the first frame-switch event
      switchFrame ctrl initFrame (BitSet.fromList [SwitchFrameGenerateEvent]) 0 -- empty user data
         |> assertE "Switch frame"

      sysFork "Display rendering loop" $ forever $ drawNext (BitSet.fromList flags) $ \gfb -> do
         draw mode gfb
         -- TODO: draw FPS if required as an overlay
         -- TODO: maybe add option to dynamically enable/disable FPS counting

      let rdr = RenderingEngine fbState

      return rdr

-- | Set a client capability
setClientCapabilityWarn :: Handle -> ClientCapability -> Bool -> Sys ()
setClientCapabilityWarn hdl cap b =
   setClientCapability hdl cap b
      |> warningShowE (textFormat ("Set client capability " % shown) cap)
      |> runE_
