module ViperVM.Arch.X86_64.Linux.Pipe where

import Foreign.Ptr (Ptr)
import Data.Word (Word)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (peekElemOff)
import Control.Applicative ((<$>), (<*>))

import ViperVM.Arch.X86_64.Linux.ErrorCode
import ViperVM.Arch.X86_64.Linux.Syscall
import ViperVM.Arch.X86_64.Linux.FileSystem

sysPipe :: SysRet (FileDescriptor, FileDescriptor)
sysPipe =
   allocaArray 2 $ \ptr -> do
      ret <- syscall1 22 (ptr :: Ptr Word)
      if ret < 0 
         then return (toLeftErrorCode ret)
         else fmap Right . (,)
            <$> (FileDescriptor <$> peekElemOff ptr 0)
            <*> (FileDescriptor <$> peekElemOff ptr 1)
      
