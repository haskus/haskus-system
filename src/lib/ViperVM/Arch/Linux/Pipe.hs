-- | Pipe
module ViperVM.Arch.Linux.Pipe
   ( createPipe
   )
where

import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (peekElemOff)

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Syscalls

-- | Create a pipe
createPipe :: SysRet (Handle, Handle)
createPipe =
   allocaArray 2 $ \ptr ->
      onSuccessIO (syscall_pipe (ptr :: Ptr Word)) 
         (const ((,)
            <$> (Handle <$> peekElemOff ptr 0)
            <*> (Handle <$> peekElemOff ptr 1)))
      
