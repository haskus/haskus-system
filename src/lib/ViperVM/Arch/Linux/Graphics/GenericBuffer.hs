{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Generic buffer management
--
-- Generic buffers are unaccelerated buffers that can be used with all devices
-- that support them with the same API (contrary to accelerated buffers)
--
-- Generic buffers are called "dumb buffers" in original terminology
--
module ViperVM.Arch.Linux.Graphics.GenericBuffer
   ( GenericBuffer
   , GenericBufferMap
   , createGenericBuffer
   , destroyGenericBuffer
   , mapGenericBuffer
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Internals

import Control.Monad (void)
import Data.Word

type GenericBuffer = StructCreateDumb
type GenericBufferMap = StructMapDumb

-- | Create a generic buffer
createGenericBuffer :: Card -> Word32 -> Word32 -> Word32 -> Word32 -> SysRet GenericBuffer
createGenericBuffer card width height bpp flags = do
   let s = StructCreateDumb height width bpp flags 0 0 0
   ioctlCreateGenericBuffer (cardHandle card) s

-- | Destroy a generic buffer
destroyGenericBuffer :: Card -> GenericBuffer -> SysRet ()
destroyGenericBuffer card buffer = do
   let s = StructDestroyDumb (cdHandle buffer)
   void <$> ioctlDestroyGenericBuffer (cardHandle card) s

-- | Map a Generic buffer
mapGenericBuffer :: Card -> GenericBuffer -> SysRet GenericBufferMap
mapGenericBuffer card buffer = do
   let s = StructMapDumb (cdHandle buffer) 0 0
   ioctlMapGenericBuffer (cardHandle card) s
