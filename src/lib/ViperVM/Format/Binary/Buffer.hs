{-# LANGUAGE ScopedTypeVariables #-}

-- | Buffer
--
-- A buffer is similar to a ByteString with the following differences:
--    * use Word64 for size and offset
--    * integrate nicely with Storable and other binary stuff
module ViperVM.Format.Binary.Buffer
   ( Buffer
   , withBufferPtr
   , bufferSize
   , bufferPeek
   , bufferRead
   , bufferDrop
   , bufferTake
   , bufferPack
   , bufferPackList
   , bufferPackPtr
   )
where

import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Word
import Foreign.Storable
import System.IO.Unsafe
import Control.Monad

import ViperVM.Utils.Memory (memCopy)

-- | A buffer
data Buffer = Buffer
   { bufferPtr    :: {-# UNPACK #-} !(ForeignPtr ()) -- ^ Pointer
   , bufferSize   :: {-# UNPACK #-} !Word64          -- ^ Size
   , bufferOffset :: {-# UNPACK #-} !Word64          -- ^ Offset
   }

-- | Unsafe: don't modify the buffer contents or you will break referential
-- transparency
withBufferPtr :: Buffer -> (Ptr b -> IO a) -> IO a
withBufferPtr buf f =
   withForeignPtr (bufferPtr buf) $ \ptr ->
      f (castPtr (ptr `plusPtr` fromIntegral (bufferOffset buf)))

-- | Peek a storable
bufferPeek :: forall a. Storable a => Buffer -> a
bufferPeek buf
   | bufferSize buf < sza = error "bufferPeek: out of bounds"
   | otherwise            = unsafePerformIO $ withBufferPtr buf peek
   where
      sza = fromIntegral (sizeOf (undefined :: a)) 

-- | Drop some bytes O(1)
bufferDrop :: Word64 -> Buffer -> Buffer
bufferDrop n buf
   | bufferSize buf < n = error "bufferDrop: out of bounds"
   | otherwise          = buf
         { bufferSize   = bufferSize buf - n
         , bufferOffset = bufferOffset buf + n
         }

-- | Read a Storable and return the new buffer
bufferRead :: forall a. Storable a => Buffer -> (Buffer,a)
bufferRead buf
   | bufferSize buf < sza = error "bufferRead: out of bounds"
   | otherwise            = unsafePerformIO $ do
         a <- withBufferPtr buf peek
         return (bufferDrop sza buf, a)
   where
      sza = fromIntegral (sizeOf (undefined :: a)) 

-- | Take some bytes O(1)
bufferTake :: Word64 -> Buffer -> Buffer
bufferTake n buf
   | bufferSize buf < n = error "bufferTake: out of bounds"
   | otherwise          = buf
         { bufferSize   = n
         }

-- | Pack a Storable
bufferPack :: forall a. Storable a => a -> Buffer
bufferPack x = unsafePerformIO $ do
   let sza = sizeOf (undefined :: a)

   fp <- mallocForeignPtr
   withForeignPtr fp (`poke` x)

   return (Buffer (castForeignPtr fp) (fromIntegral sza) 0)

-- | Pack a list of Storable
bufferPackList :: forall a. Storable a => [a] -> Buffer
bufferPackList xs = unsafePerformIO $ do
   let 
      sza = sizeOf (undefined :: a)
      lxs = length xs
   fp <- mallocForeignPtrArray lxs

   withForeignPtr fp $ \p ->
      forM_ (xs `zip` [0..]) $ \(x,o) ->
         pokeElemOff p o x

   return (Buffer (castForeignPtr fp) (fromIntegral $ sza * lxs) 0)

-- | Pack from a pointer
bufferPackPtr :: Word64 -> Ptr () -> IO Buffer
bufferPackPtr sz ptr = do
   fp <- mallocForeignPtrBytes (fromIntegral sz)
   withForeignPtr fp $ \p ->
      memCopy p ptr sz
   return (Buffer fp sz 0)
