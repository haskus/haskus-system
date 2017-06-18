-- | Helper class to pass parameters to system calls
module Haskus.System.Linux.Internals.Arg
   ( Arg (..)
   )
where

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Ptr (Ptr, ptrToWordPtr)


-- | Parameters that can be directly passed to system calls
class Arg a where
   toArg :: a -> Int64

instance Arg Int     where toArg = fromIntegral
instance Arg Int32   where toArg = fromIntegral
instance Arg Int64   where toArg = id
instance Arg Word    where toArg = fromIntegral
instance Arg Word64  where toArg = fromIntegral
instance Arg Word32  where toArg = fromIntegral
instance Arg CUShort where toArg = fromIntegral
instance Arg (Ptr a) where toArg = fromIntegral . ptrToWordPtr

