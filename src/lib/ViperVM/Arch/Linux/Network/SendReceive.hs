module ViperVM.Arch.Linux.Network.SendReceive
   ( receiveByteString
   )
where

import Foreign.Marshal.Alloc
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe
import Foreign.Ptr (castPtr)

import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.X86_64.Linux.Network.SendReceive

receiveByteString :: FileDescriptor -> Int -> [SendReceiveFlag] -> SysRet ByteString
receiveByteString fd size flags = do
   b <- mallocBytes size
   ret <- sysReceive fd b (fromIntegral size) flags (Nothing :: Maybe Int)
   case ret of
      Left err -> return (Left err)
      Right sz -> Right <$> unsafePackMallocCStringLen (castPtr b, fromIntegral sz)
