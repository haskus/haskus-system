-- | Pipe
module ViperVM.Arch.Linux.Pipe
   ( createPipe
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Format.Binary.Ptr (Ptr)
import ViperVM.Format.Binary.Storable

-- | Create a pipe
createPipe :: IOErr (Handle, Handle)
createPipe =
   allocaArray 2 $ \ptr ->
      onSuccessIO (syscall_pipe (ptr :: Ptr Word)) 
         (const ((,)
            <$> (Handle <$> peekElemOff ptr 0)
            <*> (Handle <$> peekElemOff ptr 1)))
      
