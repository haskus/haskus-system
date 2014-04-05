{-# LANGUAGE ForeignFunctionInterface #-}

-- | Memory management using Posix API
module ViperVM.Arch.Posix.Malloc (
   malloc, free
) where

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CSize(..))

foreign import ccall unsafe "stdlib.h malloc"  malloc :: CSize -> IO (Ptr a)
foreign import ccall unsafe "stdlib.h free"    free   :: Ptr a -> IO ()
