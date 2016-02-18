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
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.Memory

import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.GenericBuffer
import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.FrameBuffer
import ViperVM.Arch.Linux.Graphics.PixelFormat

import Prelude hiding (init,tail)
import Control.Monad (void,forM)
import Data.Maybe (isJust)
import Foreign.Ptr

import Text.Megaparsec
import Text.Megaparsec.Lexer hiding (space)

-- | Graphic card
data GraphicCard = GraphicCard
   { graphicCardPath    :: FilePath    -- ^ Path to the graphic card in SysFS
   , graphicCardDev     :: Device      -- ^ Device major/minor to create the device file descriptor
   , graphicCardID      :: Int         -- ^ Card identifier
   } deriving (Show)


-- | Return detected graphic cards
--
-- Graphic cards are /class/drm/cardN directories in SysFS where N is the card
-- identifier. The this directory, the dev file contains device major/minor to
-- create appropriate device node.
loadGraphicCards :: System -> Sys [GraphicCard]
loadGraphicCards system = sysLogSequence "Load graphic cards" $ do
      devs <- listDevicesWithClass system "drm" isCard
      return $ fmap makeCard devs
   where
      parseCard = void (string "card" >> decimal)
      isCard    = isJust . parseMaybe parseCard
      makeCard (path,dev) = GraphicCard path dev devid
         where  devid = read (drop 4 path)

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
      fd     = cardFileDescriptor card

   mappedPlanes <- forM bpps $ \bpp -> do
      buf <- sysCallAssert "Create a generic buffer" $
         cardCreateGenericBuffer card width height bpp flags

      bufKerMap <- sysCallAssert "Map generic buffer" $
         cardMapGenericBuffer card buf

      addr <- sysCallAssert "Map generic buffer in user space" $ 
         sysMemMap Nothing
            (genericBufferSize buf)
            [ProtRead,ProtWrite]
            [MapShared]
            (Just (fd, genericMapOffset bufKerMap))

      let plane = Plane (genericBufferHandle buf) (genericBufferPitch buf) 0

      return (MappedPlane buf bufKerMap addr plane)
   
   let planes = fmap mappedPlaneInfo mappedPlanes

   fb <- sysCallAssert "Add frame buffer" $ cardAddFrameBuffer card width height pixfmt 0 planes

   return (fb, mappedPlanes)

