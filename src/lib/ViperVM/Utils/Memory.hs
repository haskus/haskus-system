{-# LANGUAGE ForeignFunctionInterface #-}

-- | Memory utilities
module ViperVM.Utils.Memory
   ( memCopy
   , memSet
   , allocaArrays
   , peekArrays
   , pokeArrays
   , withArrays
   , getHostEndianness
   , withMaybeOrNull
   )
where

import Data.Word
import Data.Foldable (traverse_)
import Foreign.Ptr
import Control.Monad (void)
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc (alloca)
import Data.Bits ((.|.), shiftL)

import ViperVM.Format.Binary.Endianness


-- | Copy memory
memCopy :: Ptr a -> Ptr b -> Word64 -> IO ()
memCopy dest src size = void (memcpy dest src size)

{-# INLINE memCopy #-}

-- | memcpy
foreign import ccall unsafe memcpy  :: Ptr a -> Ptr b -> Word64 -> IO (Ptr c)



-- | Set memory
memSet :: Ptr a -> Word64 -> Word8 -> IO ()
memSet dest size fill = void (memset dest fill size)

{-# INLINE memSet #-}

-- | memset
foreign import ccall unsafe memset  :: Ptr a -> Word8 -> Word64 -> IO (Ptr c)


-- | Allocate several arrays
allocaArrays :: (Storable s, Integral a) => [a] -> ([Ptr s] -> IO b) -> IO b
allocaArrays sizes f = go [] sizes
   where
      go as []     = f (reverse as)
      go as (x:xs) = allocaArray (fromIntegral x) $ \a -> go (a:as) xs

-- | Peek several arrays
peekArrays :: (Storable s, Integral a) => [a] -> [Ptr s] -> IO [[s]]
peekArrays szs ptrs = traverse f (szs `zip` ptrs)
   where
      f (sz,p) = peekArray (fromIntegral sz) p

-- | Poke several arrays
pokeArrays :: (Storable s) => [Ptr s] -> [[s]] -> IO ()
pokeArrays ptrs vs = traverse_ f (ptrs `zip` vs)
   where
      f = uncurry pokeArray

-- | Allocate several arrays
withArrays :: (Storable s) => [[s]] -> ([Ptr s] -> IO b) -> IO b
withArrays vs f = go [] vs
   where
      go as []     = f (reverse as)
      go as (x:xs) = withArray x $ \a -> go (a:as) xs

-- | Detect the endianness of the host memory
getHostEndianness :: IO Endianness
getHostEndianness = do
   -- Write a 32 bit Int and check byte ordering
   let magic = 1 .|. shiftL 8 2 .|. shiftL 16 3 .|. shiftL 24 4 :: Word32
   alloca $ \p -> do
      poke p magic
      rs <- peekArray 4 (castPtr p :: Ptr Word8)
      return $ if rs == [1,2,3,4] then BigEndian else LittleEndian

-- | Execute f with a pointer to 'a' or NULL
withMaybeOrNull :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybeOrNull s f = case s of
   Nothing -> f nullPtr
   Just x  -> with x f
