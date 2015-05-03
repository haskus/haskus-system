module ViperVM.Arch.Linux.FileSystem.ReadWrite
   ( readByteString
   , writeByteString
   )
where

import Foreign.Marshal.Alloc
import Data.ByteString
import Data.ByteString.Unsafe
import Foreign.Ptr (castPtr)
import Data.Word (Word64)

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

writeByteString :: FileDescriptor -> ByteString -> SysRet Word64
writeByteString fd bs = unsafeUseAsCStringLen bs $ \(ptr,len) ->
   sysWrite fd ptr (fromIntegral len)
