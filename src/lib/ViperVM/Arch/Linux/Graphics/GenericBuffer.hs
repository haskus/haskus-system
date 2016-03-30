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
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Internals.Graphics

import Control.Monad (void)
import Data.Word

type GenericBuffer = StructCreateDumb
type GenericBufferMap = StructMapDumb

-- | Create a generic buffer
createGenericBuffer :: Handle -> Word32 -> Word32 -> Word32 -> Word32 -> SysRet GenericBuffer
createGenericBuffer hdl width height bpp flags = do
   let s = StructCreateDumb height width bpp flags 0 0 0
   ioctlCreateGenericBuffer s hdl

-- | Destroy a generic buffer
destroyGenericBuffer :: Handle -> GenericBuffer -> SysRet ()
destroyGenericBuffer hdl buffer = do
   let s = StructDestroyDumb (cdHandle buffer)
   void <$> ioctlDestroyGenericBuffer s hdl

-- | Map a Generic buffer
mapGenericBuffer :: Handle -> GenericBuffer -> SysRet GenericBufferMap
mapGenericBuffer hdl buffer = do
   let s = StructMapDumb (cdHandle buffer) 0 0
   ioctlMapGenericBuffer s hdl
