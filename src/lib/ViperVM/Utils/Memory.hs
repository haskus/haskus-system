{-# LANGUAGE ForeignFunctionInterface #-}

module ViperVM.Utils.Memory
   ( memCopy
   , memSet
   )
where

import Data.Word
import Foreign.Ptr
import Control.Monad (void)

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
