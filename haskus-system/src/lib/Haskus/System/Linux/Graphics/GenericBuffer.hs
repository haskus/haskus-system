{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

-- | Generic buffers
--
-- Generic buffers are unaccelerated buffers that can be used with all devices
-- that support them with the same API (contrary to hardware specific buffers).
-- They are mappable into memory (we handle the mapping directly and
-- automatically after the buffer creation to make them even simplier to use).
--
-- Generic buffers are called "dumb buffers" in original terminology
--
module Haskus.System.Linux.Graphics.GenericBuffer
   ( GenericBuffer (..)
   , showGenericBuffer
   , freeGenericBuffer
   , withGenericBufferPtr
   , handleCreateGenericBuffer
   , handleFreeGenericBuffer
   , handleGetGenericBufferMapOffset
   )
where

import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Memory
import Haskus.System.Linux.Graphics.KIO
import Haskus.System.Linux.Graphics.Entities
import Haskus.Utils.Flow
import Haskus.Binary.BitSet as BitSet
import Haskus.Number.Word
-- required to use a finalizer calling back into Haskell (sysMemUnmap)
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Ptr

-- | A generic buffer
data GenericBuffer = GenericBuffer
   { genericBufferHeight       :: {-# UNPACK #-} !Word32 -- ^ Height in pixels
   , genericBufferWidth        :: {-# UNPACK #-} !Word32 -- ^ Width in pixels
   , genericBufferBitsPerPixel :: {-# UNPACK #-} !Word32 -- ^ Bits per pixel
   , genericBufferFlags        :: {-# UNPACK #-} !Word32 -- ^ Flags
   , genericBufferHandle       :: {-# UNPACK #-} !Word32 -- ^ Handle
   , genericBufferPitch        :: {-# UNPACK #-} !Word32 -- ^ Pitch in bytes
   , genericBufferSize         :: {-# UNPACK #-} !Word64 -- ^ Size in bytes
   , genericBufferCardHandle   :: Handle                 -- ^ Card handle
   , genericBufferPtr          :: ForeignPtr ()          -- ^ Mapping in host memory
   }
   deriving (Show)

-- | Show a generic buffer
showGenericBuffer :: GenericBuffer -> String
showGenericBuffer GenericBuffer{..} = mconcat
   [ "Type: generic buffer\n"
   , "Width:  ", show genericBufferWidth, " pixels\n"
   , "Height: ", show genericBufferHeight, " pixels\n"
   , "Bits per pixels: ", show genericBufferBitsPerPixel, "\n"
   , "Flags: ", show genericBufferFlags, "\n"
   , "Handle: ", show genericBufferHandle, "\n"
   , "Pitch: ", show genericBufferPitch, " bytes\n"
   , "Size: ", show genericBufferSize, " bytes\n"
   , "Address: ", show genericBufferPtr, "\n"
   ]

instance ShowBuffer GenericBuffer where
   showBuffer = showGenericBuffer

-- | Create a generic buffer and map it in user memory.
--
-- The foreign pointer targets the memory mapping and is automatically unmapped.
handleCreateGenericBuffer :: MonadInIO m => Handle -> Word32 -> Word32 -> Word32 -> Word32 -> Excepts '[ErrorCode] m GenericBuffer
handleCreateGenericBuffer hdl width height bpp flags = do
   let s = StructCreateDumb height width bpp flags 0 0 0
   -- allocate the buffer
   r <- ioctlCreateGenericBuffer s hdl
   -- map it in user space
   offset <- handleGetGenericBufferMapOffset hdl (cdHandle r)
   addr <- sysMemMap Nothing (cdSize r)
            (BitSet.fromList [ProtRead,ProtWrite])
            (BitSet.fromList [MapShared])
            Nothing
            (Just (hdl, offset))
               -- free buffer on mapping error
               |> onE_ (runE_ (handleFreeGenericBuffer hdl (cdHandle r)))
   -- create a foreign pointer that automatically unmaps the buffer
   let finalizer = runE_ (sysMemUnmap addr (cdSize r))
   fptr <- liftIO (newForeignPtr addr finalizer)
   pure <| GenericBuffer
     { genericBufferHeight       = cdHeight r
     , genericBufferWidth        = cdWidth r
     , genericBufferBitsPerPixel = cdBPP r
     , genericBufferFlags        = cdFlags r
     , genericBufferHandle       = cdHandle r
     , genericBufferPitch        = cdPitch r
     , genericBufferSize         = cdSize r
     , genericBufferCardHandle   = hdl
     , genericBufferPtr          = fptr
     }

-- | Free a generic buffer
handleFreeGenericBuffer :: MonadInIO m => Handle -> Word32 -> Excepts '[ErrorCode] m ()
handleFreeGenericBuffer hdl bufferHdl = do
   let s = StructDestroyDumb bufferHdl
   void (ioctlDestroyGenericBuffer s hdl)

-- | Free a generic buffer
freeGenericBuffer :: MonadInIO m => GenericBuffer -> Excepts '[ErrorCode] m ()
freeGenericBuffer buffer = do
   liftIO <| finalizeForeignPtr (genericBufferPtr buffer)
   handleFreeGenericBuffer (genericBufferCardHandle buffer) (genericBufferHandle buffer)

-- | Get the map offset of a generic buffer
--
-- The offset can be used as an mmap offset (on the card handle)
handleGetGenericBufferMapOffset :: MonadInIO m => Handle -> Word32 -> Excepts '[ErrorCode] m Word64
handleGetGenericBufferMapOffset hdl bufferHdl = do
   let s = StructMapDumb bufferHdl 0 0
   ioctlMapGenericBuffer s hdl
      ||> mdOffset

-- | Use the generic buffer mapped area
withGenericBufferPtr :: MonadInIO m => GenericBuffer -> (Ptr () -> m a) -> m a
withGenericBufferPtr buffer action =
   liftWith (withForeignPtr (genericBufferPtr buffer)) action
