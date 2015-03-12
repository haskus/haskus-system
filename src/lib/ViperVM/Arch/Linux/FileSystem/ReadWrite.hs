module ViperVM.Arch.Linux.FileSystem.ReadWrite
   ( readByteString
   )
where

import Foreign.Marshal.Alloc
import Data.ByteString
import Data.ByteString.Unsafe
import Control.Applicative ((<$>))
import Foreign.Ptr (castPtr)

import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.X86_64.Linux.FileSystem.ReadWrite

readByteString :: FileDescriptor -> Int -> SysRet ByteString
readByteString fd size = do
   b <- mallocBytes size
   ret <- sysRead fd b (fromIntegral size)
   case ret of
      Left err -> return (Left err)
      Right sz -> Right <$> unsafePackCStringLen (b',sz')
         where
            sz' = fromIntegral sz
            b'  = castPtr b

