{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Video controller management
--
-- Controllers are called CRTC in original terminology
module ViperVM.Arch.Linux.Graphics.Controller
   ( Controller(..)
   , setController
   , PageFlip(..)
   , cardControllers
   -- * Low level
   , cardControllerFromID
   , ControllerStruct(..)
   , fromControllerStruct
   , ControllerLutStruct(..)
   , PageFlipStruct(..)
   )
where

import Foreign.Storable
import Foreign.CStorable
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.Word
import GHC.Generics (Generic)

import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Utils.BitSet

-- | Video controller
--
-- A controller is used to configure what is displayed on the screen
data Controller = Controller
   { controllerID               :: ControllerID
   , controllerGammaSize        :: Word32
   , controllerMode             :: Maybe Mode
   , controllerFrameBuffer      :: Maybe (FrameBufferID, Word32, Word32)   -- ^ Associated frame buffer and its dimensions (WxH)
   , controllerCard             :: Card
   } deriving (Show)


-- | Data matching the C structure drm_mode_crtc
data ControllerStruct = ControllerStruct
   { contSetConnPtr :: Word64
   , contConnCount  :: Word32
   , contID         :: Word32
   , contFbID       :: Word32
   , contFbX        :: Word32
   , contFbY        :: Word32
   , contGammaSize  :: Word32
   , contModeValid  :: Word32
   , contModeInfo   :: ModeStruct
   } deriving Generic

instance CStorable ControllerStruct
instance Storable ControllerStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

emptyControllerStruct :: ControllerStruct
emptyControllerStruct = ControllerStruct 0 0 0 0 0 0 0 0 emptyModeStruct

fromControllerStruct :: Card -> ControllerStruct -> Controller
fromControllerStruct card (ControllerStruct {..}) =
   Controller
      (ControllerID contID)
      contGammaSize
      (if contModeValid /= 0
         then Just (fromModeStruct contModeInfo)
         else Nothing)
      (if contFbID /= 0 
         then Just (FrameBufferID contFbID, contFbX, contFbY)
         else Nothing)
      card

      
-- | Get Controller
cardControllerFromID :: Card -> ControllerID -> SysRet Controller
cardControllerFromID card crtcid = withCard card $ \ioctl fd -> do
   let
      ControllerID cid = crtcid
      crtc = emptyControllerStruct { contID = cid }

   fmap (fromControllerStruct card) <$> ioctlReadWrite ioctl 0x64 0xA1 defaultCheck fd crtc

-- | Set Controller
setController :: IOCTL -> FileDescriptor -> ControllerID -> Maybe FrameBufferID -> [ConnectorID] -> Maybe Mode -> SysRet ()
setController ioctl fd crtcid fb conns mode = do
   let
      ControllerID cid = crtcid
      conns' = fmap (\(ConnectorID i) -> i) conns


   withArray conns' $ \conArray -> do
      let
         crtc = emptyControllerStruct
            { contID = cid
            , contFbID = case fb of
               Nothing                -> 0
               Just (FrameBufferID x) -> x
            , contModeInfo = case mode of
               Nothing -> emptyModeStruct
               Just x  -> toModeStruct x
            , contConnCount  = fromIntegral (length conns)
            , contSetConnPtr = fromIntegral (ptrToWordPtr conArray)
            }

      fmap (const ()) <$> ioctlReadWrite ioctl 0x64 0xA2 defaultCheck fd crtc

-- | Data matching the C structure drm_mode_crtc_lut
data ControllerLutStruct = ControllerLutStruct
   { clsCrtcId       :: Word32
   , clsGammaSize    :: Word32
   , clsRed          :: Word64
   , clsGreen        :: Word64
   , clsBlue         :: Word64
   } deriving Generic

instance CStorable ControllerLutStruct
instance Storable  ControllerLutStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke


data PageFlip
   = PageFlipEvent
   | PageFlipAsync
   deriving (Show,Eq,Enum)

instance EnumBitSet PageFlip

-- | Data matching the C structure drm_mode_crtc_page_flip
data PageFlipStruct = PageFlipStruct
   { pfCrtcId        :: Word32
   , pfFbId          :: Word32
   , pfFlags         :: Word32
   , pfReserved      :: Word32
   , pfUserData      :: Word64
   } deriving Generic

instance CStorable PageFlipStruct
instance Storable  PageFlipStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke


-- | Get controllers (discard errors)
cardControllers :: Card -> IO [Controller]
cardControllers = cardEntities cardControllerIDs cardControllerFromID
