module ViperVM.Arch.X86_64.Linux.Network
   ( sysShutdown
   , sysSendFile
   , sysSendFileWithOffset
   )
where

import Foreign.Ptr (nullPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Storable (peek)
import Data.Word

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.X86_64.Linux.Syscall

data ShutFlag =
     ShutRead
   | ShutWrite
   | ShutReadWrite
   deriving (Enum,Show)

-- | Shut down part of a full-duplex connection
sysShutdown :: FileDescriptor -> ShutFlag -> SysRet ()
sysShutdown (FileDescriptor fd) flag =
   onSuccess (syscall2 48 fd (fromEnum flag)) (const ())

-- | Call sendfile using implicit file cursor for input
sysSendFile :: FileDescriptor -> FileDescriptor -> Word64 -> SysRet Word64
sysSendFile (FileDescriptor outfd) (FileDescriptor infd) count =
   onSuccess (syscall4 40 outfd infd nullPtr count) fromIntegral

-- | Call sendFile using explicit input offset, returns new offset
sysSendFileWithOffset :: FileDescriptor -> FileDescriptor -> Word64 -> Word64 -> SysRet (Word64,Word64)
sysSendFileWithOffset (FileDescriptor outfd) (FileDescriptor infd) offset count =
   with offset $ \off ->
      onSuccessIO (syscall4 40 outfd infd off count) $ \x -> do
         newOff <- peek off
         return (fromIntegral x, newOff)
