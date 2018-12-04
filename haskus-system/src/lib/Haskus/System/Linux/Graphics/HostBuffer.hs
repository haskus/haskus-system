{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

-- | Host buffers
--
-- Host buffers are unaccelerated buffers that can be used with all devices
-- that support them with the same API (contrary to accelerated buffers)
--
-- Host buffers are called "dumb buffers" in original terminology
--
module Haskus.System.Linux.Graphics.HostBuffer
   ( HostBuffer
   , HostBufferMap
   , createHostBuffer
   , destroyHostBuffer
   , mapHostBuffer
   )
where

import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Internals.Graphics
import Haskus.Utils.Flow
import Haskus.Format.Binary.Word

type HostBuffer = StructCreateDumb
type HostBufferMap = StructMapDumb

-- | Create a host buffer
createHostBuffer :: MonadInIO m => Handle -> Word32 -> Word32 -> Word32 -> Word32 -> FlowT '[ErrorCode] m HostBuffer
createHostBuffer hdl width height bpp flags = do
   let s = StructCreateDumb height width bpp flags 0 0 0
   ioctlCreateHostBuffer s hdl

-- | Destroy a host buffer
destroyHostBuffer :: MonadInIO m => Handle -> HostBuffer -> FlowT '[ErrorCode] m ()
destroyHostBuffer hdl buffer = do
   let s = StructDestroyDumb (cdHandle buffer)
   void (ioctlDestroyHostBuffer s hdl)

-- | Map a host buffer
mapHostBuffer :: MonadInIO m => Handle -> HostBuffer -> FlowT '[ErrorCode] m HostBufferMap
mapHostBuffer hdl buffer = do
   let s = StructMapDumb (cdHandle buffer) 0 0
   ioctlMapHostBuffer s hdl
