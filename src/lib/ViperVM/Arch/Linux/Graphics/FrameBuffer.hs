{-# LANGUAGE RecordWildCards
           , GeneralizedNewtypeDeriving #-}

-- | Frame buffer management
module ViperVM.Arch.Linux.Graphics.FrameBuffer
   ( Plane(..)
   , FrameBuffer(..)
   , cardAddFrameBuffer
   , cardRemoveFrameBuffer
   )
where

import Data.Word

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Graphics.PixelFormat
import ViperVM.Arch.Linux.Graphics.LowLevel.FrameBuffer
import ViperVM.Arch.Linux.Graphics.LowLevel.Card

-- | Create a framebuffer
cardAddFrameBuffer :: Card -> Word32 -> Word32 -> PixelFormat -> Word32 -> [Plane] -> SysRet FrameBuffer
cardAddFrameBuffer card width height fmt flags planes =
   withCard card addFrameBuffer width height fmt flags planes

-- | Release a frame buffer
cardRemoveFrameBuffer :: Card -> FrameBuffer -> SysRet ()
cardRemoveFrameBuffer card fb = withCard card removeFrameBuffer fb
