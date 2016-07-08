
-- | Unsigned and signed words
module ViperVM.Format.Binary.Word
   ( Word8
   , Word16
   , Word32
   , Word64
   , Int8
   , Int16
   , Int32
   , Int64
   -- * Some C types
   , CSize(..)
   , CUShort
   , CShort
   , CUInt
   , CInt
   , CULong
   , CLong
   )
where

import Data.Word
import Data.Int
import Foreign.C.Types
