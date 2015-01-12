{-# LANGUAGE RecordWildCards #-}

-- | Dumb buffer management
--
-- Dumb buffers are unaccelerated buffers that can be used with all devices
-- that support them with the same API (contrary to accelerated buffers)
module ViperVM.Arch.Linux.Graphics.DumbBuffer
   ( DumbBuffer(..)
   , DumbBufferMap(..)
   , createDumbBuffer
   , destroyDumbBuffer
   , mapDumbBuffer
   )
where

import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor

import Control.Applicative ((<$>), (<*>))
import Foreign.Marshal.Utils (with)
import Foreign.Storable
import Data.Word

-- | A dumb buffer
data DumbBuffer = DumbBuffer
   { dumbBufferHeight   :: Word32
   , dumbBufferWidth    :: Word32
   , dumbBufferBPP      :: Word32   -- ^ Bits per pixel
   , dumbBufferFlags    :: Word32
   , dumbBufferHandle   :: Word32
   , dumbBufferPitch    :: Word32
   , dumbBufferSize     :: Word64
   } deriving (Show)

instance Storable DumbBuffer where
   sizeOf _    = 6*4 + 8
   alignment _ = 8
   peek ptr    = DumbBuffer
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr 4
      <*> peekByteOff ptr 8
      <*> peekByteOff ptr 12
      <*> peekByteOff ptr 16
      <*> peekByteOff ptr 20
      <*> peekByteOff ptr 24

   poke ptr (DumbBuffer {..}) = do
      pokeByteOff ptr 0  dumbBufferHeight
      pokeByteOff ptr 4  dumbBufferWidth
      pokeByteOff ptr 8  dumbBufferBPP
      pokeByteOff ptr 12 dumbBufferFlags
      pokeByteOff ptr 16 dumbBufferHandle
      pokeByteOff ptr 20 dumbBufferPitch
      pokeByteOff ptr 24 dumbBufferSize

-- | Mapping of a dump buffer
data DumbBufferMap = DumbBufferMap
   { dumbMapHandle :: Word32
   --, dumbMapPad    :: Word32  -- Padding field: not useful
   , dumbMapOffset :: Word64
   } deriving (Show)

instance Storable DumbBufferMap where
   sizeOf _    = 16
   alignment _ = 8
   peek ptr    = DumbBufferMap
      <$> peekByteOff ptr 0
      -- <*> peekByteOff ptr 4
      <*> peekByteOff ptr 8

   poke ptr (DumbBufferMap {..}) = do
      pokeByteOff ptr 0  dumbMapHandle
      -- pokeByteOff ptr 4  dumbMapPad
      pokeByteOff ptr 8  dumbMapOffset


-- | Create a new Dumb buffer
--
-- This is supported by all drivers (software rendering)
createDumbBuffer :: IOCTL -> FileDescriptor -> Word32 -> Word32 -> Word32 -> Word32 -> SysRet DumbBuffer
createDumbBuffer ioctl fd width height bpp flags = do
   let res = DumbBuffer height width bpp flags 0 0 0
   ioctlReadWrite ioctl 0x64 0xB2 defaultCheck fd res

-- | Destroy a dumb buffer
destroyDumbBuffer :: IOCTL -> FileDescriptor -> DumbBuffer -> SysRet ()
destroyDumbBuffer ioctl fd buffer = do
   
   with (dumbBufferHandle buffer) $ \bufPtr ->
      fmap (const ()) <$> ioctlReadWrite ioctl 0x64 0xB4 defaultCheck fd bufPtr

-- | Map a Dumb buffer
mapDumbBuffer :: IOCTL -> FileDescriptor -> DumbBuffer -> SysRet DumbBufferMap
mapDumbBuffer ioctl fd buffer = do
   let res = DumbBufferMap (dumbBufferHandle buffer) 0
   ioctlReadWrite ioctl 0x64 0xB3 defaultCheck fd res
