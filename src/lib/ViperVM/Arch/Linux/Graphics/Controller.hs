{-# LANGUAGE RecordWildCards
           , GeneralizedNewtypeDeriving #-}

-- | Video controller (CRTC in original terminology) management
module ViperVM.Arch.Linux.Graphics.Controller
   ( Controller(..)
   , getController
   , setController
   )
where

import Control.Applicative ((<$>), (<*>))
import Foreign.Storable
import Data.Word

import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.IDs

-- | Video controller
--
-- A controller is used to configure what is displayed on the screen
data Controller = Controller
   { controllerSetConnectorsPtr :: Word64
   , controllerConnectorCount   :: Word32
   , controllerID               :: ControllerID
   , controllerFrameBuffer      :: Maybe (FrameBufferID, Word32, Word32)   -- ^ Associated frame buffer and its dimensions (WxH)
   , controllerGammaSize        :: Word32
   , controllerMode             :: Maybe Mode
   } deriving (Show)

instance Storable Controller where
   sizeOf _    = 8 + 7*4 + sizeOf (undefined :: Mode)
   alignment _ = 8
   peek ptr    = do
      -- Read FB position only if FB is valid
      fbId <- peekByteOff ptr 16 :: IO Word32
      fb <- if fbId == 0
               then return Nothing
               else do
                  x <- peekByteOff ptr 20
                  y <- peekByteOff ptr 24
                  return $ (Just (FrameBufferID fbId,x,y))
      -- Read mode only if mode is valid
      modeIsValid <- peekByteOff ptr 32 :: IO Word32
      mode <- if modeIsValid == 0
               then return Nothing
               else Just <$> peekByteOff ptr 36
      Controller
         <$> peekByteOff ptr 0
         <*> peekByteOff ptr 8
         <*> peekByteOff ptr 12
         <*> return fb
         <*> peekByteOff ptr 28
         <*> return mode
   poke ptr (Controller {..}) = do
      pokeByteOff ptr 0  controllerSetConnectorsPtr
      pokeByteOff ptr 8  controllerConnectorCount
      pokeByteOff ptr 12 controllerID
      pokeByteOff ptr 28 controllerGammaSize
      case controllerMode of
         Nothing -> pokeByteOff ptr 32 (0 :: Word32)
         Just m  -> do
            pokeByteOff ptr 32 (1 :: Word32)
            pokeByteOff ptr 36 m
      case controllerFrameBuffer of
         Nothing -> do
            pokeByteOff ptr 16 (0 :: Word32)
            pokeByteOff ptr 20 (0 :: Word32)
            pokeByteOff ptr 24 (0 :: Word32)
         Just (fbId,x,y) -> do
            pokeByteOff ptr 16 fbId
            pokeByteOff ptr 20 x
            pokeByteOff ptr 24 y

-- | Get Controller
getController :: IOCTL -> FileDescriptor -> ControllerID -> SysRet Controller
getController ioctl fd crtcid = do
   let crtc = Controller 0 0 crtcid Nothing 0 Nothing
   ioctlReadWrite ioctl 0x64 0xA1 defaultCheck fd crtc

-- | Set Controller
setController :: IOCTL -> FileDescriptor -> ControllerID -> Maybe FrameBufferID -> [ConnectorID] -> Maybe Mode -> SysRet ()
setController ioctl fd crtcid fb conns mode = do
   let crtc = Controller 0 0 crtcid Nothing 0 Nothing
   ret <- ioctlReadWrite ioctl 0x64 0xA2 defaultCheck fd crtc
   return (fmap (const ()) ret)
