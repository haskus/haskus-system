-- | File descriptor (used for many things in Linux)
module ViperVM.Arch.Linux.FileDescriptor
   ( FileDescriptor(..)
   , HandleFlag(..)
   , HandleFlags
   , getHandleFlags
   , setHandleFlags
   )
where

import Data.Word

import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.X86_64.Linux.Syscalls
import ViperVM.Format.Binary.BitSet as BitSet

-- | Kernel object handle
--
-- (file descriptor in original terminology)
newtype FileDescriptor = FileDescriptor Word deriving (Show,Eq)


-- | Get descriptor flags
getHandleFlags :: FileDescriptor -> Sys (BitSet Word64 HandleFlag)
getHandleFlags (FileDescriptor fd) =
   sysCallAssert ("Get handle "++show fd++" flags") $ 
      onSuccess (syscall_fcntl fd 1 0) (BitSet.fromBits . fromIntegral) 

-- | Set descriptor flags
setHandleFlags :: FileDescriptor -> BitSet Word64 HandleFlag -> Sys ()
setHandleFlags (FileDescriptor fd) flgs =
   sysCallAssert ("Set handle "++show fd++" flags") $ 
      onSuccessVoid $ syscall_fcntl fd 2 (BitSet.toBits flgs)

-- | Handle flags 
data HandleFlag
   = HandleWriteOnly
   | HandleReadWrite
   | HandleCloseOnExec
   | HandleAppend
   | HandleAsync
   | HandleCreate
   | HandleDirect
   | HandleDirectory
   | HandleExclusive
   | HandleLargeFile
   | HandleWithoutAccessTime
   | HandleNoTTYControl
   | HandleDontFollowSymLinks
   | HandleNonBlocking
   | HandlePath
   | HandleSynchronous
   | HandleTmpFile
   | HandleTruncate
   deriving (Show,Eq,Enum)

type HandleFlags = BitSet Int HandleFlag

instance CBitSet HandleFlag where
   toBitOffset x = case x of
      HandleWriteOnly          -> 0
      HandleReadWrite          -> 1
      HandleCreate             -> 6
      HandleExclusive          -> 7
      HandleNoTTYControl       -> 8
      HandleTruncate           -> 9
      HandleAppend             -> 10
      HandleNonBlocking        -> 11
      HandleSynchronous        -> 12
      HandleAsync              -> 13
      HandleDirect             -> 14
      HandleLargeFile          -> 15
      HandleDirectory          -> 16
      HandleDontFollowSymLinks -> 17
      HandleWithoutAccessTime  -> 18
      HandleCloseOnExec        -> 19
      HandlePath               -> 21
      HandleTmpFile            -> 22

   fromBitOffset x = case x of
      0  -> HandleWriteOnly
      1  -> HandleReadWrite
      6  -> HandleCreate
      7  -> HandleExclusive
      8  -> HandleNoTTYControl
      9  -> HandleTruncate
      10 -> HandleAppend
      11 -> HandleNonBlocking
      12 -> HandleSynchronous
      13 -> HandleAsync
      14 -> HandleDirect
      15 -> HandleLargeFile
      16 -> HandleDirectory
      17 -> HandleDontFollowSymLinks
      18 -> HandleWithoutAccessTime
      19 -> HandleCloseOnExec
      21 -> HandlePath
      22 -> HandleTmpFile
      _  -> error "Unknown handle flag"
