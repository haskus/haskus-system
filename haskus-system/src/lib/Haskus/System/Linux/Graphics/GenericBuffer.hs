{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

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
   , freeGenericBuffer
   , withGenericBufferPtr
   , handleCreateGenericBuffer
   , handleFreeGenericBuffer
   , handleGetGenericBufferMapOffset
   )
where

import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Internals.Graphics
import Haskus.System.Linux.Memory
import Haskus.Utils.Flow
import Haskus.Format.Binary.BitSet as BitSet
import Haskus.Format.Binary.Word
import Foreign.ForeignPtr
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
   let finalizer _ = runE_ (sysMemUnmap addr (cdSize r))
   finalizerPtr <- liftIO (mkFinalizerPtr finalizer)
   fptr <- liftIO (newForeignPtr finalizerPtr addr)
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

foreign import ccall "wrapper"
   mkFinalizerPtr :: (Ptr () -> IO ()) -> IO (FinalizerPtr ())

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
