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
   ( GenericBuffer(..)
   , GenericBufferMap(..)
   , createGenericBuffer
   , destroyGenericBuffer
   , mapGenericBuffer
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Internals

import Foreign.Marshal.Utils (with)
import Foreign.Storable
import Foreign.CStorable
import Control.Monad (void)
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
   } deriving (Show,Generic,CStorable)

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
   } deriving (Show,Generic,CStorable)

instance Storable  GenericBufferMap where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke


-- | Create a generic buffer
createGenericBuffer :: Card -> Word32 -> Word32 -> Word32 -> Word32 -> SysRet GenericBuffer
createGenericBuffer card width height bpp flags = do
   let res = GenericBuffer height width bpp flags 0 0 0
   ioctlModeCreateGenericBuffer (cardHandle card) res

-- | Destroy a generic buffer
destroyGenericBuffer :: Card -> GenericBuffer -> SysRet ()
destroyGenericBuffer card buffer =
   -- We don't use a data matching the C structure drm_mode_destroy_dumb because
   -- it only contains a single Word32 field that we can allocate directly
   with (genericBufferHandle buffer) $ \bufPtr ->
      void <$> ioctlModeDestroyGenericBuffer (cardHandle card) bufPtr

-- | Map a Generic buffer
mapGenericBuffer :: Card -> GenericBuffer -> SysRet GenericBufferMap
mapGenericBuffer card buffer = do
   let res = GenericBufferMap (genericBufferHandle buffer) 0 0
   ioctlModeMapGenericBuffer (cardHandle card) res
