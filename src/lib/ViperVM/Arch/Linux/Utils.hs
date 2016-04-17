module ViperVM.Arch.Linux.Utils
   ( withMaybeOrNull
   )
where

import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

withMaybeOrNull :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybeOrNull s f = case s of
   Nothing -> f nullPtr
   Just x  -> with x f
