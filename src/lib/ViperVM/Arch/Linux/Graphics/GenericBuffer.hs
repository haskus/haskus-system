{-# LANGUAGE RecordWildCards #-}

-- | Generic buffer management
--
-- Generic buffers are unaccelerated buffers that can be used with all devices
-- that support them with the same API (contrary to accelerated buffers)
--
-- Generic buffers are called "dumb buffers" in original terminology
--
module ViperVM.Arch.Linux.Graphics.GenericBuffer
   ( GenericBuffer(..)
   , GenericBufferMap(..)
   , cardCreateGenericBuffer
   , cardDestroyGenericBuffer
   , cardMapGenericBuffer
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Graphics.LowLevel.GenericBuffer
import ViperVM.Arch.Linux.Graphics.LowLevel.Card

import Data.Word

-- | Create a new Generic buffer
--
-- This is supported by all drivers (software rendering)
cardCreateGenericBuffer :: Card -> Word32 -> Word32 -> Word32 -> Word32 -> SysRet GenericBuffer
cardCreateGenericBuffer card width height bpp flags =
   withCard card createGenericBuffer width height bpp flags

-- | Destroy a generic buffer
cardDestroyGenericBuffer :: Card -> GenericBuffer -> SysRet ()
cardDestroyGenericBuffer card buffer =
   withCard card destroyGenericBuffer buffer

-- | Map a Generic buffer
cardMapGenericBuffer :: Card -> GenericBuffer -> SysRet GenericBufferMap
cardMapGenericBuffer card buffer =
   withCard card mapGenericBuffer buffer
