module ViperVM.Arch.X86_64.Linux.Socket (
   sysSendFile, sysSendFileWithOffset
) where

import Foreign.Ptr (nullPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Storable (peek)
import Data.Word

import ViperVM.Arch.X86_64.Linux.Syscall
import ViperVM.Arch.X86_64.Linux.ErrorCode
import ViperVM.Arch.X86_64.Linux.FileSystem

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
