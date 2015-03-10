{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Generic buffer management
--
-- Generic buffers are unaccelerated buffers that can be used with all devices
-- that support them with the same API (contrary to accelerated buffers)
--
-- Generic buffers are called "dumb buffers" in original terminology
--
module ViperVM.Arch.Linux.Graphics.LowLevel.GenericBuffer
   ( GenericBuffer(..)
   , GenericBufferMap(..)
   , createGenericBuffer
   , destroyGenericBuffer
   , mapGenericBuffer
   )
where

import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor

import Control.Applicative ((<$>))
import Foreign.Marshal.Utils (with)
import Foreign.Storable
import Foreign.CStorable
import GHC.Generics
import Data.Word

-- | A generic buffer
--
-- Data matching the C structure drm_mode_create_dumb
data GenericBuffer = GenericBuffer
   { genericBufferHeight   :: Word32
   , genericBufferWidth    :: Word32
   , genericBufferBPP      :: Word32   -- ^ Bits per pixel
   , genericBufferFlags    :: Word32
   , genericBufferHandle   :: Word32
   , genericBufferPitch    :: Word32
   , genericBufferSize     :: Word64
   } deriving (Show,Generic)

instance CStorable GenericBuffer
instance Storable  GenericBuffer where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke


-- | Mapping of a dump buffer
--
-- Data matching the C structure drm_mode_map_dumb
data GenericBufferMap = GenericBufferMap
   { genericMapHandle :: Word32
   , genericMapPad    :: Word32  -- Padding field: not useful
   , genericMapOffset :: Word64
   } deriving (Show,Generic)

instance CStorable GenericBufferMap
instance Storable  GenericBufferMap where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke


-- | Create a new Generic buffer
--
-- This is supported by all drivers (software rendering)
createGenericBuffer :: IOCTL -> FileDescriptor -> Word32 -> Word32 -> Word32 -> Word32 -> SysRet GenericBuffer
createGenericBuffer ioctl fd width height bpp flags = do
   let res = GenericBuffer height width bpp flags 0 0 0
   ioctlReadWrite ioctl 0x64 0xB2 defaultCheck fd res

-- | Destroy a generic buffer
-- 
-- We don't use a data matching the C structure drm_mode_destroy_dumb because it contains only a single Word32 field that we can allocate directly
destroyGenericBuffer :: IOCTL -> FileDescriptor -> GenericBuffer -> SysRet ()
destroyGenericBuffer ioctl fd buffer = do
   
   with (genericBufferHandle buffer) $ \bufPtr ->
      fmap (const ()) <$> ioctlReadWrite ioctl 0x64 0xB4 defaultCheck fd bufPtr

-- | Map a Generic buffer
mapGenericBuffer :: IOCTL -> FileDescriptor -> GenericBuffer -> SysRet GenericBufferMap
mapGenericBuffer ioctl fd buffer = do
   let res = GenericBufferMap (genericBufferHandle buffer) 0 0
   ioctlReadWrite ioctl 0x64 0xB3 defaultCheck fd res

