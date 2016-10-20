{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Manage graphics devices
module ViperVM.System.Graphics
   ( GraphicCard (..)
   , loadGraphicCards
   , MappedSurface (..)
   , GenericFrame (..)
   , initGenericFrameBuffer
   , freeGenericFrameBuffer
   , graphicCardConnectors
   , graphicCardControllers
   , graphicCardEncoders
   , graphicCardPlanes
   -- * Multi-buffering
   , RenderingEngine (..)
   , BufferingState (..)
   , FrameWait (..)
   , initRenderingEngine
   )
where

import ViperVM.System.Sys
import ViperVM.System.Devices
import ViperVM.System.Process
import ViperVM.System.Event
import ViperVM.Format.Binary.BitSet (CBitSet(..), BitSet)
import qualified ViperVM.Format.Binary.BitSet as BitSet
import qualified ViperVM.Format.Text as Text
import ViperVM.Format.Binary.Ptr
import ViperVM.Utils.Flow
import ViperVM.Utils.List (isPrefixOf)
import ViperVM.Utils.Maybe
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.FileSystem.ReadWrite
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.Memory

import ViperVM.Arch.Linux.Internals.Graphics
import ViperVM.Arch.Linux.Graphics.Capability
import ViperVM.Arch.Linux.Graphics.GenericBuffer
import ViperVM.Arch.Linux.Graphics.Connector
import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Graphics.Encoder
import ViperVM.Arch.Linux.Graphics.Helper
import ViperVM.Arch.Linux.Graphics.Plane
import ViperVM.Arch.Linux.Graphics.Property
import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.FrameBuffer
import ViperVM.Arch.Linux.Graphics.PixelFormat
import ViperVM.Arch.Linux.Graphics.Event as Graphics

import Control.Concurrent.STM
import Control.Concurrent
import Foreign.Marshal (allocaBytes)
import System.Posix.Types (Fd(..))
import System.FilePath (takeBaseName)

-- | Graphic card
data GraphicCard = GraphicCard
   { graphicCardPath    :: DevicePath           -- ^ Path to the graphic card in SysFS
   , graphicCardDev     :: Device               -- ^ Device major/minor to create the device file descriptor
   , graphicCardID      :: Int                  -- ^ Card identifier
   , graphicCardHandle  :: Handle               -- ^ Device handle
   , graphicCardChan    :: TChan Graphics.Event -- ^ Event stream
   }


-- | Return detected graphic cards
--
-- Graphic cards are /class/drm/cardN directories in SysFS where N is the card
-- identifier. The this directory, the dev file contains device major/minor to
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
   flowForFilter devs' $ \(devpath,dev) -> do
      let cardID = read (drop 4 (takeBaseName (Text.unpack devpath)))
      getDeviceHandle dm dev
         >.~.> (\hdl -> do
            -- We support these capabilities
            setClientCapability hdl ClientCapStereo3D        True
            setClientCapability hdl ClientCapUniversalPlanes True
            setClientCapability hdl ClientCapAtomic          True
            -- Create the DRM event reader thread
            GraphicCard devpath dev cardID hdl
               <$> newEventWaiterThread hdl
            )


-- | Create a new thread reading input events and putting them in a TChan
newEventWaiterThread :: Handle -> Sys (TChan Graphics.Event)
newEventWaiterThread fd@(Handle lowfd) = do
   let
      bufsz = 1000 -- buffer size
      rfd = Fd (fromIntegral lowfd)

   ch <- sysIO $ newBroadcastTChanIO
   sysFork "Graphics event reader" $ sysIO $ allocaBytes bufsz $ \ptr -> forever $ do
      threadWaitRead rfd
      sysRead fd ptr (fromIntegral bufsz)
         >.~!> \sz2 -> do
         -- FIXME: we should somehow signal that an error occured and
         -- that we won't report future events (if any)
         evs <- peekEvents ptr (fromIntegral sz2)
         atomically $ mapM_ (writeTChan ch) evs
   return ch


data MappedSurface = MappedSurface
   { mappedSurfaceBuffer  :: GenericBuffer
   , mappedSurfaceMapping :: GenericBufferMap
   , mappedSurfacePointer :: Ptr ()
   , mappedSurfaceInfo    :: Surface
   }

data GenericFrame = GenericFrame
   { genericFrameBuffer  :: FrameBuffer
   , genericFrameBuffers :: [MappedSurface]
   }

-- | Allocate and map fullscreen planes for the given format and mode
initGenericFrameBuffer :: GraphicCard -> Mode -> PixelFormat -> Sys GenericFrame
initGenericFrameBuffer card mode pixfmt = do
   let
      fmt    = formatFormat pixfmt
      hdl    = graphicCardHandle card
      width  = fromIntegral $ modeHorizontalDisplay mode
      height = fromIntegral $ modeVerticalDisplay mode
      bpps   = formatBitDepth fmt
      flags  = 0

   mappedPlanes <- forM bpps $ \bpp -> do
      buf <- sysCallAssert "Create a generic buffer" $
         createGenericBuffer hdl width height bpp flags

      bufKerMap <- sysCallAssert "Map generic buffer" $
         mapGenericBuffer hdl buf

      addr <- sysCallAssert "Map generic buffer in user space" $ 
         sysMemMap Nothing
            (cdSize buf)
            (BitSet.fromList [ProtRead,ProtWrite])
            (BitSet.fromList [MapShared])
            Nothing
            (Just (hdl, mdOffset bufKerMap))

      let plane = Surface (cdHandle buf) (cdPitch buf) 0 0

      return (MappedSurface buf bufKerMap addr plane)
   
   let planes = fmap mappedSurfaceInfo mappedPlanes

   fb <- sysCallAssert "Add frame buffer" $ addFrameBuffer hdl width height pixfmt BitSet.empty planes

   return $ GenericFrame fb mappedPlanes


freeGenericFrameBuffer :: GraphicCard -> GenericFrame -> Sys ()
freeGenericFrameBuffer card (GenericFrame fb mappedBufs) = do

   let hdl = graphicCardHandle card

   forM_ mappedBufs $ \(MappedSurface buf _ addr _) -> do
      -- unmap generic buffer from user-space
      sysCallAssert "Unmap generic buffer from user space" $ 
         sysMemUnmap addr (cdSize buf)

      -- destroy the generic buffer
      sysCallAssert "Destroy generic buffer" $ destroyGenericBuffer hdl buf


   -- remove the framebuffer
   sysCallAssert "Remove framebuffer" $ removeFrameBuffer hdl fb


-- | Retreive graphic card connectors
graphicCardConnectors :: GraphicCard -> Flow Sys '[[Connector],InvalidParam,EntryNotFound,InvalidProperty,InvalidHandle]
graphicCardConnectors = getConnectors . graphicCardHandle

-- | Retrieve graphic card controllers
graphicCardControllers :: GraphicCard -> Flow Sys '[[Controller],EntryNotFound,InvalidHandle]
graphicCardControllers = getControllers . graphicCardHandle

-- | Retrieve graphic card encoders
graphicCardEncoders :: GraphicCard -> Flow Sys '[[Encoder],EntryNotFound,InvalidHandle]
graphicCardEncoders = getEncoders . graphicCardHandle

-- | Retrieve graphic card planes
graphicCardPlanes :: GraphicCard -> Sys [Plane]
graphicCardPlanes card = do
   let hdl = graphicCardHandle card
   getPlaneResources hdl
      >.~^> flowTraverse (getPlane hdl)
      -- shouldn't happen, except if we unplug the graphic card or unload the
      -- driver
      >..%~!!> (\(InvalidHandle _) -> error "Invalid handle")
      -- shouldn't happen, planes are invariant
      >..%~!!> (\(InvalidPlane _)  -> error "Invalid plane" )
      -- return the result
      |> flowRes


-----------------------------------------------------------------------
-- Multi-buffering
-----------------------------------------------------------------------

-- | Rendering engine
--
-- Manage multi-buffering
data RenderingEngine = RenderingEngine
   { engineBuffereringState :: TVar (BufferingState GenericFrame) -- ^ Multi-buffering state
   }

-- | Multi-buffering state
data BufferingState a = BufferingState
   { fbShown    :: a          -- ^ Framebuffer currently displayed
   , fbPending  :: Maybe a    -- ^ Framebuffer being flipped
   , fbDrawn    :: Maybe a    -- ^ Framebuffer drawn but not yet submitted to be flipped
   , fbDrawable :: [a]        -- ^ Framebuffers free to be drawed on
   }

-- | Rendering wait
data FrameWait
   = WaitPending  -- ^ Wait if there is a pending frame (that will be displayed asap)
   | WaitDrawn    -- ^ wait if there is already a rendered frame
   deriving (Show,Eq,Enum,CBitSet)


-- | Init the rendering engine
--
-- TODO: indicate failure (GenericBuffer not supported, framebuffer allocation
-- error, etc.)
-- TODO: support multiple controllers/connectors
-- TODO: support connections/disconnections
-- TODO: better support for mode setting (change during rendering, etc.)
-- TODO: support accelerated buffers
initRenderingEngine :: GraphicCard -> Controller -> Mode -> Word -> [FrameWait] -> (Mode -> GenericFrame -> Sys ()) -> Sys RenderingEngine
initRenderingEngine card ctrl mode nfb flags draw
   | nfb <= 2  = error "initRenderingEngine: require at least 2 buffers"
   | otherwise = do

      -- Check support for generic buffers
      let fd    = graphicCardHandle card
      sysLogSequence "Load graphic card" $ do
         cap  <- sysCallAssert "Get GenericBuffer capability" (fd `supports` CapGenericBuffer)
         sysAssert "Generic buffer capability supported" cap

      -- TODO: support other formats
      let fmt = makePixelFormat XRGB8888 LittleEndian

      -- initialize generic framebuffers
      bufs <- forM [1..nfb] (const (initGenericFrameBuffer card mode fmt))

      -- page flip
      fbState <- sysIO $ newTVarIO (BufferingState
                  { fbShown    = head bufs
                  , fbPending  = Nothing
                  , fbDrawn    = Nothing
                  , fbDrawable = tail bufs
                  })

      fps <- sysIO $ newTVarIO (0 :: Word)

      -- on page flip complete
      -- FIXME: how do we know which controller has flipped?
      onEvent (graphicCardChan card) $ \case
         VBlankEvent FlipComplete _ ->
            sysIO $ atomically $ do
               s <- readTVar fbState
               case fbPending s of
                  Nothing -> return ()
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
         gfb <- sysIO $ atomically $ do
            s <- readTVar fbState
            -- check that the previous frame is flipped
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

         -- flip the pending frame
         let (GenericFrame fb _) = gfb
         sysCallAssertQuiet "Switch framebuffer" $
            switchFrameBuffer ctrl fb (BitSet.fromList [PageFlipEvent])
            

      let
         -- draw the next frame
         drawNext :: BitSet Word FrameWait -> (GenericFrame -> Sys ()) -> Sys ()
         drawNext wait f = do
            -- reserve next frame
            gfb <- sysIO $ atomically $ do
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
            f gfb
            -- indicate it is drawn
            sysIO $ atomically $ do
               s <- readTVar fbState
               case fbDrawn s of
                  Nothing -> writeTVar fbState (s { fbDrawn = Just gfb })
                  Just d  -> do
                     -- drop the alreay drawn frame and set the newer
                     writeTVar fbState $ s
                        { fbDrawn    = Just gfb
                        , fbDrawable = d : fbDrawable s
                        }
            -- switch to another thread
            sysIO yield

      -- set mode and connectors
      -- sysCallAssertQuiet "Set controller" $
      --    setController ctrl (SetFB fb1) [conn] (Just mode)

      -- Force the generation of the first page-flip event
      sysCallAssertQuiet "Switch framebuffer" $
         switchFrameBuffer ctrl (genericFrameBuffer (head bufs)) (BitSet.fromList [PageFlipEvent])

      sysFork "Display rendering loop" $ forever $ drawNext (BitSet.fromList flags) $ \gfb -> do
         draw mode gfb
         -- TODO: draw FPS if required as an overlay
         -- TODO: maybe add option to dynamically enable/disable FPS counting

      let rdr = RenderingEngine fbState

      return rdr

