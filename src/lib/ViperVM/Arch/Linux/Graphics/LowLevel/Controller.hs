{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module ViperVM.Arch.Linux.Graphics.LowLevel.Controller
   ( Controller(..)
   , cardControllerFromID
   , setController
   , ControllerStruct(..)
   , fromControllerStruct
   )
where

import Foreign.Storable
import Foreign.CStorable
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.Word
import GHC.Generics (Generic)
import Control.Applicative ((<$>))

import ViperVM.Arch.Linux.Graphics.LowLevel.Mode
import ViperVM.Arch.Linux.Graphics.LowLevel.IDs
import ViperVM.Arch.Linux.Graphics.LowLevel.Card
import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor

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
