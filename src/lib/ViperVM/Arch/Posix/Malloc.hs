{-# LANGUAGE ForeignFunctionInterface #-}

-- | Memory management using Posix API
module ViperVM.Arch.Posix.Malloc
   ( malloc
   , free
   )
where

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types (CSize(..))

import ViperVM.Arch.Common.Errors

foreign import ccall unsafe "stdlib.h malloc"  malloc_ :: CSize -> IO (Ptr a)

foreign import ccall unsafe "stdlib.h free"    free   :: Ptr a -> IO ()

malloc :: CSize -> IO (Either AllocError (Ptr a))
malloc sz = do
   ptr <- malloc_ sz
   return $ if ptr == nullPtr && sz /= 0
      then Left AllocOutOfMemory
      else Right ptr
