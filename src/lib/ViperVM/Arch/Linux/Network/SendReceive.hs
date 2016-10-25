{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module ViperVM.Arch.Linux.Network.SendReceive
   ( SendReceiveFlag(..)
   , SendReceiveFlags
   , sysReceive
   , receiveBuffer
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Storable
import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.Buffer
import ViperVM.Utils.Flow


data SendReceiveFlag
   = FlagOutOfBand         -- ^ Process out-of-band data
   | FlagPeek              -- ^ Peek at incoming messages
   | FlagDontRoute         -- ^ Don't use local routing
   | FlagTruncateControl   -- ^ Control data lost before delivery
   | FlagProxy             -- ^ Supply or ask second address
   | FlagTruncate
   | FlagDontWait          -- ^ Nonblocking IO
   | FlagEndOfRecord       -- ^ End of record
   | FlagWaitAll           -- ^ Wait for a full request
   | FlagFIN
   | FlagSYN
   | FlagConfirm           -- ^ Confirm path validity
   | FlagRST
   | FlagFetchErrorQueue   -- ^ Fetch message from error queue
   | FlagNoSignal          -- ^ Do not generate SIGPIPE
   | FlagMore              -- ^ Sender will send more
   | FlagWaitForOne        -- ^ Wait for at least one packet to return
   | FlagFastOpen          -- ^ Send data in TCP SYN
   | FlagCloseOnExec       -- ^ Set close_on_exit for file descriptor received through SCM_RIGHTS
   deriving (Show,Eq,CBitSet)

instance Enum SendReceiveFlag where
   fromEnum x = case x of
      FlagOutOfBand         -> 0
      FlagPeek              -> 1
      FlagDontRoute         -> 2
      FlagTruncateControl   -> 3
      FlagProxy             -> 4
      FlagTruncate          -> 5
      FlagDontWait          -> 6
      FlagEndOfRecord       -> 7
      FlagWaitAll           -> 8
      FlagFIN               -> 9
      FlagSYN               -> 10
      FlagConfirm           -> 11
      FlagRST               -> 12
      FlagFetchErrorQueue   -> 13
      FlagNoSignal          -> 14
      FlagMore              -> 15
      FlagWaitForOne        -> 16
      FlagFastOpen          -> 29
      FlagCloseOnExec       -> 30
   toEnum x = case x of
      0  -> FlagOutOfBand
      1  -> FlagPeek
      2  -> FlagDontRoute
      3  -> FlagTruncateControl
      4  -> FlagProxy
      5  -> FlagTruncate
      6  -> FlagDontWait
      7  -> FlagEndOfRecord
      8  -> FlagWaitAll
      9  -> FlagFIN
      10 -> FlagSYN
      11 -> FlagConfirm
      12 -> FlagRST
      13 -> FlagFetchErrorQueue
      14 -> FlagNoSignal
      15 -> FlagMore
      16 -> FlagWaitForOne
      29 -> FlagFastOpen
      30 -> FlagCloseOnExec
      _  -> error "Unknown send-receive flag"

type SendReceiveFlags = BitSet Word64 SendReceiveFlag

-- | Receive data from a socket
--
-- recvfrom syscall
sysReceive :: Storable a => Handle -> Ptr () -> Word64 -> SendReceiveFlags -> Maybe a -> IOErr Word64
sysReceive (Handle fd) ptr size flags addr = do
   let
      call :: Ptr a -> Ptr Word64 -> IOErr Word64
      call add len = onSuccess (syscall @"recvfrom" fd ptr size (BitSet.toBits flags) (castPtr add) len) fromIntegral

   case addr of
      Nothing -> call nullPtr nullPtr
      Just a  -> with a $ \a' -> 
         with (fromIntegral (sizeOf a)) $ \sptr -> call a' sptr

receiveBuffer :: Handle -> Int -> SendReceiveFlags -> IOErr Buffer
receiveBuffer fd size flags = do
   b <- mallocBytes (fromIntegral size)
   sysReceive fd b (fromIntegral size) flags (Nothing :: Maybe Int)
      -- free the buffer on error
      >..~=> const (free b)
      -- otherwise make a bytestring
      >.~.> \sz -> bufferPackPtr (fromIntegral sz) (castPtr b)
