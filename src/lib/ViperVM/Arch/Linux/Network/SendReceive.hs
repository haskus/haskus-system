module ViperVM.Arch.Linux.Network.SendReceive
   ( receiveByteString
   )
where

import Foreign.Marshal.Alloc
import Data.ByteString
import Data.ByteString.Unsafe
import Control.Applicative ((<$>))
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
      Right sz -> Right <$> unsafePackCStringLen (b',sz')
         where
            sz' = fromIntegral sz
            b'  = castPtr b

