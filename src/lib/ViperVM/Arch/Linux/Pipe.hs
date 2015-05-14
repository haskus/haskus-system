module ViperVM.Arch.Linux.Pipe
   ( createPipe
   )
where

import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (peekElemOff)

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Syscalls

-- | Create a pipe
createPipe :: SysRet (FileDescriptor, FileDescriptor)
createPipe =
   allocaArray 2 $ \ptr ->
      onSuccessIO (syscall_pipe (ptr :: Ptr Word)) 
         (const ((,)
            <$> (FileDescriptor <$> peekElemOff ptr 0)
            <*> (FileDescriptor <$> peekElemOff ptr 1)))
      
