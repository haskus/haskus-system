{-# LANGUAGE ForeignFunctionInterface #-}

-- | Memory management using Posix API
module Haskus.System.Posix.Malloc
   ( malloc
   , free
   , AllocError(..)
   )
where

import Foreign.Ptr (Ptr, nullPtr)
import Haskus.Binary.CTypes


-- | Buffer allocation error
data AllocError
   = AllocOutOfMemory
   | AllocUnknownError
   deriving (Show,Eq)

foreign import ccall unsafe "stdlib.h malloc"  malloc_ :: CSize -> IO (Ptr a)

foreign import ccall unsafe "stdlib.h free"    free   :: Ptr a -> IO ()

malloc :: CSize -> IO (Either AllocError (Ptr a))
malloc sz = do
   ptr <- malloc_ sz
   return $ if ptr == nullPtr && sz /= 0
      then Left AllocOutOfMemory
      else Right ptr
