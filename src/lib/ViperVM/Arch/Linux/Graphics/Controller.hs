{-# LANGUAGE RecordWildCards
           , GeneralizedNewtypeDeriving #-}

-- | A video controller (CRTC in original terminology)
module ViperVM.Arch.Linux.Graphics.Controller
   ( Controller(..)
   , ControllerID(..)
   , getController
   )
where

import Control.Applicative ((<$>), (<*>))
import Foreign.Storable
import Data.Word

import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Graphics.FrameBuffer
import ViperVM.Arch.Linux.Graphics.Mode

newtype ControllerID   = ControllerID Word32 deriving (Show,Eq,Storable)

data Controller = Controller
   { crtcSetConnectorsPtr :: Word64
   , crtcConnectorCount   :: Word32
   , crtcID               :: ControllerID
   , crtcFrameBuffer      :: Maybe (FrameBufferID,Word32, Word32)
   --, crtcFrameBufferX     :: Word32
   --, crtcFrameBufferY     :: Word32
   , crtcGammaSize        :: Word32
   --, crtcModeIsValid      :: Word32
   , crtcMode             :: Maybe Mode
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
      pokeByteOff ptr 0  crtcSetConnectorsPtr
      pokeByteOff ptr 8  crtcConnectorCount
      pokeByteOff ptr 12 crtcID
      pokeByteOff ptr 28 crtcGammaSize
      case crtcMode of
         Nothing -> pokeByteOff ptr 32 (0 :: Word32)
         Just m  -> do
            pokeByteOff ptr 32 (1 :: Word32)
            pokeByteOff ptr 36 m
      case crtcFrameBuffer of
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
