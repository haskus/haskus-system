{-# LANGUAGE ForeignFunctionInterface #-}

module ViperVM.Utils.Memory
   ( memCopy
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
