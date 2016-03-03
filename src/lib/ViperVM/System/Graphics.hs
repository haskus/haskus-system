-- | Manage graphics devices
module ViperVM.System.Graphics
   ( GraphicCard(..)
   , loadGraphicCards
   , MappedPlane(..)
   , initFrameBuffer
   )
where

import ViperVM.System.System
import ViperVM.Arch.Linux.FileSystem.OpenClose
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.FileSystem
import ViperVM.Arch.Linux.FileSystem.ReadWrite
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.Memory

import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.GenericBuffer
import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.FrameBuffer
import ViperVM.Arch.Linux.Graphics.PixelFormat
import ViperVM.Arch.Linux.Graphics.Event as Graphics

import Prelude hiding (init,tail)
import Control.Monad (void,forM)
import Foreign.Ptr

import Control.Concurrent.STM
import Control.Concurrent
import Data.Foldable (traverse_)
import Control.Monad.Trans.Class (lift)
import Foreign.Marshal (allocaBytes)
import System.Posix.Types (Fd(..))
import Data.List (isPrefixOf)
import System.FilePath (takeBaseName)

-- | Graphic card
data GraphicCard = GraphicCard
   { graphicCardPath    :: FilePath             -- ^ Path to the graphic card in SysFS
   , graphicCardDev     :: Device               -- ^ Device major/minor to create the device file descriptor
   , graphicCardID      :: Int                  -- ^ Card identifier
   , graphicCardHandle  :: FileDescriptor       -- ^ Device handle
   , graphicCardChan    :: TChan Graphics.Event -- ^ Event stream
   }


-- | Return detected graphic cards
--
-- Graphic cards are /class/drm/cardN directories in SysFS where N is the card
-- identifier. The this directory, the dev file contains device major/minor to
-- create appropriate device node.
loadGraphicCards :: System -> Sys [GraphicCard]
loadGraphicCards system = sysLogSequence "Load graphic cards" $ do

   devs <- listDevicesWithClass system "drm"
   let
      isCard (p,_) = "card" `isPrefixOf` takeBaseName p
      devs' = filter isCard devs
   forM devs' $ \(devpath,dev) -> do
      fd   <- getDeviceHandle system CharDevice dev
      GraphicCard devpath dev (read (drop 4 devpath)) fd
         <$> newEventWaiterThread fd


-- | Create a new thread reading input events and putting them in a TChan
newEventWaiterThread :: FileDescriptor -> Sys (TChan Graphics.Event)
newEventWaiterThread fd@(FileDescriptor lowfd) = do
   let
      bufsz = 1000 -- buffer size
      rfd = Fd (fromIntegral lowfd)

   ch <- lift $ newBroadcastTChanIO
   void $ lift $ forkIO $ allocaBytes bufsz $ \ptr -> do
      let go = do
            threadWaitRead rfd
            r <- sysRead fd ptr (fromIntegral bufsz)
            case r of
               -- FIXME: we should somehow signal that an error occured and
               -- that we won't report future events (if any)
               Left _  -> return ()
               Right sz2 -> do
                  evs <- peekEvents ptr (fromIntegral sz2)
                  atomically $ traverse_ (writeTChan ch) evs
                  go
      go
   return ch


data MappedPlane = MappedPlane
   { mappedPlaneBuffer  :: GenericBuffer
   , mappedPlaneMapping :: GenericBufferMap
   , mappedPlanePointer :: Ptr ()
   , mappedPlaneInfo    :: Plane
   }

-- | Allocate and map fullscreen planes for the given format and mode
initFrameBuffer :: Card -> Mode -> PixelFormat -> Sys (FrameBuffer, [MappedPlane])
initFrameBuffer card mode pixfmt@(PixelFormat fmt _) = do
   let
      width  = fromIntegral $ modeHorizontalDisplay mode
      height = fromIntegral $ modeVerticalDisplay mode
      bpps   = formatBitDepth fmt
      flags  = 0
      fd     = cardHandle card

   mappedPlanes <- forM bpps $ \bpp -> do
      buf <- sysCallAssert "Create a generic buffer" $
         createGenericBuffer card width height bpp flags

      bufKerMap <- sysCallAssert "Map generic buffer" $
         mapGenericBuffer card buf

      addr <- sysCallAssert "Map generic buffer in user space" $ 
         sysMemMap Nothing
            (genericBufferSize buf)
            [ProtRead,ProtWrite]
            [MapShared]
            (Just (fd, genericMapOffset bufKerMap))

      let plane = Plane (genericBufferHandle buf) (genericBufferPitch buf) 0 0

      return (MappedPlane buf bufKerMap addr plane)
   
   let planes = fmap mappedPlaneInfo mappedPlanes

   fb <- sysCallAssert "Add frame buffer" $ addFrameBuffer card width height pixfmt 0 planes

   return (fb, mappedPlanes)

