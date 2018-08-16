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
createHostBuffer :: MonadIO m => Handle -> Word32 -> Word32 -> Word32 -> Word32 -> Flow m '[HostBuffer,ErrorCode]
createHostBuffer hdl width height bpp flags = do
   let s = StructCreateDumb height width bpp flags 0 0 0
   liftIO (ioctlCreateHostBuffer s hdl)

-- | Destroy a host buffer
destroyHostBuffer :: MonadIO m => Handle -> HostBuffer -> Flow m '[(),ErrorCode]
destroyHostBuffer hdl buffer = do
   let s = StructDestroyDumb (cdHandle buffer)
   liftIO (ioctlDestroyHostBuffer s hdl) >.-.> const ()

-- | Map a host buffer
mapHostBuffer :: MonadIO m => Handle -> HostBuffer -> Flow m '[HostBufferMap,ErrorCode]
mapHostBuffer hdl buffer = do
   let s = StructMapDumb (cdHandle buffer) 0 0
   liftIO (ioctlMapHostBuffer s hdl)
