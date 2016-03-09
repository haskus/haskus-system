{-# LANGUAGE ForeignFunctionInterface #-}

module ViperVM.Utils.Memory
   ( memCopy
   , memSet
   , allocaArrays
   , peekArrays
   )
where

import Data.Word
import Foreign.Ptr
import Control.Monad (void)
import Foreign.Storable
import Foreign.Marshal.Array (peekArray,allocaArray)

-- | Copy memory
memCopy :: Ptr a -> Ptr b -> Word64 -> IO ()
memCopy dest src size = void (memcpy dest src size)

{-# INLINE memCopy #-}

foreign import ccall unsafe memcpy  :: Ptr a -> Ptr b -> Word64 -> IO (Ptr c)



-- | Set memory
memSet :: Ptr a -> Word64 -> Word8 -> IO ()
memSet dest size fill = void (memset dest fill size)

{-# INLINE memSet #-}

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
