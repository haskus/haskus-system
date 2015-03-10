{-# LANGUAGE RecordWildCards #-}

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

import Control.Applicative ((<$>), (<*>))
import Foreign.Marshal.Utils (with)
import Foreign.Storable
import Data.Word

-- | A generic buffer
data GenericBuffer = GenericBuffer
   { genericBufferHeight   :: Word32
   , genericBufferWidth    :: Word32
   , genericBufferBPP      :: Word32   -- ^ Bits per pixel
   , genericBufferFlags    :: Word32
   , genericBufferHandle   :: Word32
   , genericBufferPitch    :: Word32
   , genericBufferSize     :: Word64
   } deriving (Show)

instance Storable GenericBuffer where
   sizeOf _    = 6*4 + 8
   alignment _ = 8
   peek ptr    = GenericBuffer
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr 4
      <*> peekByteOff ptr 8
      <*> peekByteOff ptr 12
      <*> peekByteOff ptr 16
      <*> peekByteOff ptr 20
      <*> peekByteOff ptr 24

   poke ptr (GenericBuffer {..}) = do
      pokeByteOff ptr 0  genericBufferHeight
      pokeByteOff ptr 4  genericBufferWidth
      pokeByteOff ptr 8  genericBufferBPP
      pokeByteOff ptr 12 genericBufferFlags
      pokeByteOff ptr 16 genericBufferHandle
      pokeByteOff ptr 20 genericBufferPitch
      pokeByteOff ptr 24 genericBufferSize

-- | Mapping of a dump buffer
data GenericBufferMap = GenericBufferMap
   { genericMapHandle :: Word32
   --, genericMapPad    :: Word32  -- Padding field: not useful
   , genericMapOffset :: Word64
   } deriving (Show)

instance Storable GenericBufferMap where
   sizeOf _    = 16
   alignment _ = 8
   peek ptr    = GenericBufferMap
      <$> peekByteOff ptr 0
      -- <*> peekByteOff ptr 4
      <*> peekByteOff ptr 8

   poke ptr (GenericBufferMap {..}) = do
      pokeByteOff ptr 0  genericMapHandle
      -- pokeByteOff ptr 4  genericMapPad
      pokeByteOff ptr 8  genericMapOffset


-- | Create a new Generic buffer
--
-- This is supported by all drivers (software rendering)
createGenericBuffer :: IOCTL -> FileDescriptor -> Word32 -> Word32 -> Word32 -> Word32 -> SysRet GenericBuffer
createGenericBuffer ioctl fd width height bpp flags = do
   let res = GenericBuffer height width bpp flags 0 0 0
   ioctlReadWrite ioctl 0x64 0xB2 defaultCheck fd res

-- | Destroy a generic buffer
destroyGenericBuffer :: IOCTL -> FileDescriptor -> GenericBuffer -> SysRet ()
destroyGenericBuffer ioctl fd buffer = do
   
   with (genericBufferHandle buffer) $ \bufPtr ->
      fmap (const ()) <$> ioctlReadWrite ioctl 0x64 0xB4 defaultCheck fd bufPtr

-- | Map a Generic buffer
mapGenericBuffer :: IOCTL -> FileDescriptor -> GenericBuffer -> SysRet GenericBufferMap
mapGenericBuffer ioctl fd buffer = do
   let res = GenericBufferMap (genericBufferHandle buffer) 0
   ioctlReadWrite ioctl 0x64 0xB3 defaultCheck fd res

