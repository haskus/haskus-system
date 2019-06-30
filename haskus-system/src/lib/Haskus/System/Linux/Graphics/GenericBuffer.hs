{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

-- | Generic buffers
--
-- Generic buffers are unaccelerated buffers that can be used with all devices
-- that support them with the same API (contrary to hardware specific buffers)
--
-- Generic buffers are called "dumb buffers" in original terminology
--
module Haskus.System.Linux.Graphics.GenericBuffer
   ( GenericBuffer
   , GenericBufferMap
   , createGenericBuffer
   , freeGenericBuffer
   , mapGenericBuffer
   )
where

import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Internals.Graphics
import Haskus.Utils.Flow
import Haskus.Format.Binary.Word

type GenericBuffer = StructCreateDumb
type GenericBufferMap = StructMapDumb

-- | Create a host buffer
createGenericBuffer :: MonadInIO m => Handle -> Word32 -> Word32 -> Word32 -> Word32 -> Excepts '[ErrorCode] m GenericBuffer
createGenericBuffer hdl width height bpp flags = do
   let s = StructCreateDumb height width bpp flags 0 0 0
   ioctlCreateGenericBuffer s hdl

-- | Free a host buffer
freeGenericBuffer :: MonadInIO m => Handle -> GenericBuffer -> Excepts '[ErrorCode] m ()
freeGenericBuffer hdl buffer = do
   let s = StructDestroyDumb (cdHandle buffer)
   void (ioctlDestroyGenericBuffer s hdl)

-- | Map a host buffer
mapGenericBuffer :: MonadInIO m => Handle -> GenericBuffer -> Excepts '[ErrorCode] m GenericBufferMap
mapGenericBuffer hdl buffer = do
   let s = StructMapDumb (cdHandle buffer) 0 0
   ioctlMapGenericBuffer s hdl
