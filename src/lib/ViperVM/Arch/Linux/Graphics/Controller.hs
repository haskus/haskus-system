{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Video controller management
--
-- Controllers are called CRTC in original terminology
module ViperVM.Arch.Linux.Graphics.Controller
   ( Controller (..)
   , FrameBufferPos (..)
   , setController'
   , switchFrameBuffer'
   , PageFlipFlag(..)
   , PageFlipFlags
   , cardControllers
   -- * Low level
   , cardControllerFromID
   , ControllerStruct(..)
   , fromControllerStruct
   , ControllerLutStruct(..)
   )
where

import Foreign.Storable
import Foreign.CStorable
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.Word
import GHC.Generics (Generic)
import Control.Monad (void)

import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Internals
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Format.Binary.BitSet

-- | Video controller
--
-- A controller is used to configure what is displayed on the screen
data Controller = Controller
   { controllerID             :: ControllerID
   , controllerMode           :: Maybe Mode
   , controllerFrameBuffer    :: Maybe FrameBufferPos -- ^ Associated frame buffer and its position (x,y)
   , controllerGammaTableSize :: Word32
   , controllerCard           :: Card
   } deriving (Show)

data FrameBufferPos = FrameBufferPos
   { frameBufferPosID :: FrameBufferID
   , frameBufferPosX  :: Word32
   , frameBufferPosY  :: Word32
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
fromControllerStruct card ControllerStruct{..} =
   Controller
      (ControllerID contID)
      (if contModeValid /= 0
         then Just (fromModeStruct contModeInfo)
         else Nothing)
      (if contFbID /= 0 
         then Just (FrameBufferPos (FrameBufferID contFbID) contFbX contFbY)
         else Nothing)
      contGammaSize
      card

      
-- | Get Controller
cardControllerFromID :: Card -> ControllerID -> SysRet Controller
cardControllerFromID card crtcid = do
   let
      fd               = cardHandle card
      ControllerID cid = crtcid
      crtc             = emptyControllerStruct { contID = cid }

   fmap (fromControllerStruct card) <$> ioctlModeGetController fd crtc

setController' :: Card -> ControllerID -> Maybe FrameBufferPos -> [ConnectorID] -> Maybe Mode -> SysRet ()
setController' card crtcid fb conns mode = do
   let
      ControllerID cid = crtcid
      conns' = fmap (\(ConnectorID i) -> i) conns

      (fbid,fbx,fby) = case fb of
         Nothing -> (0,0,0)
         Just (FrameBufferPos (FrameBufferID z) x y) -> (z,x,y)

   withArray conns' $ \conArray -> do
      let
         crtc = ControllerStruct
            { contID = cid
            , contFbID = fbid
            , contFbX  = fbx
            , contFbY  = fby
            , contModeInfo = case mode of
               Nothing -> emptyModeStruct
               Just x  -> toModeStruct x
            , contModeValid  = case mode of
               Nothing -> 0
               Just _  -> 1
            , contConnCount  = fromIntegral (length conns)
            , contSetConnPtr = fromIntegral (ptrToWordPtr conArray)
            , contGammaSize = 0
            }

      void <$> ioctlModeSetController (cardHandle card) crtc

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


data PageFlipFlag
   = PageFlipEvent
   | PageFlipAsync
   deriving (Show,Eq,Enum)

instance EnumBitSet PageFlipFlag
type PageFlipFlags = BitSet Word32 PageFlipFlag

-- | Data matching the C structure drm_mode_crtc_page_flip
data PageFlipStruct = PageFlipStruct
   { pfCrtcId        :: Word32
   , pfFbId          :: Word32
   , pfFlags         :: PageFlipFlags
   , pfReserved      :: Word32
   , pfUserData      :: Word64
   } deriving Generic

instance CStorable PageFlipStruct
instance Storable  PageFlipStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- | Switch to another framebuffer for the given controller
-- without doing a full mode change
--
-- Called "mode_page_flip" in the original terminology
switchFrameBuffer' :: Card -> ControllerID -> FrameBufferID -> PageFlipFlags -> SysRet ()
switchFrameBuffer' card crtcid fb flags = do
   let
      ControllerID cid = crtcid
      FrameBufferID fid = fb
      s = PageFlipStruct cid fid flags 0 0

   void <$> ioctlModePageFlip (cardHandle card) s

-- | Get controllers (discard errors)
cardControllers :: Card -> IO [Controller]
cardControllers = cardEntities cardControllerIDs cardControllerFromID
