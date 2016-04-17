{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

-- | Kernel object handle
--
-- File descriptor in original terminology
module ViperVM.Arch.Linux.Handle
   ( Handle (..)
   , HandleFlag(..)
   , HandleFlags
   , getHandleFlags
   , setHandleFlags
   , InvalidHandle (..)
   )
where

import ViperVM.System.Sys
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Internals.Handle
import ViperVM.Arch.Linux.Internals.Fcntl
import ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Utils.Flow


-- | Invalid handle error
data InvalidHandle = InvalidHandle Handle deriving (Show,Eq)


-- | Get descriptor flags
getHandleFlags :: Handle -> Flow Sys '[HandleFlags,InvalidHandle]
getHandleFlags hdl =
   sysFlow (sysFcntl hdl FcntlGetFlags (0 :: Int))
      >.-.> (BitSet.fromBits . fromIntegral)
      >..%~#> \case
         EBADF -> flowSet (InvalidHandle hdl)
         e     -> unhdlErr "getHandleFlags" e

-- | Set descriptor flags
setHandleFlags :: Handle -> HandleFlags -> Flow Sys '[(),InvalidHandle]
setHandleFlags hdl flgs =
   sysOnSuccessVoid (sysFcntl hdl FcntlSetFlags (BitSet.toBits flgs))
      >%~#> \case
         EBADF -> flowSet (InvalidHandle hdl)
         e     -> unhdlErr "setHandleFlags" e

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
