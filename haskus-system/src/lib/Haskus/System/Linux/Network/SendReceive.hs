{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskus.System.Linux.Network.SendReceive
   ( SendReceiveFlag(..)
   , SendReceiveFlags
   , sysReceive
   , receiveBuffer
   )
where

import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Syscalls
import Haskus.Format.Binary.BitSet as BitSet
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.Buffer
import Haskus.Utils.Flow

import Foreign.Ptr
import Foreign.Marshal.Alloc(free,mallocBytes)


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
   deriving (Show,Eq,BitOffset)

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
sysReceive :: (MonadInIO m, Storable a) => Handle -> Ptr () -> Word64 -> SendReceiveFlags -> Maybe a -> Excepts '[ErrorCode] m Word64
sysReceive (Handle fd) ptr size flags addr = do
   let
      call add len = do
         r <- liftIO (syscall_recvfrom fd ptr size (BitSet.toBits flags) (castPtr add) len)
         fromIntegral <$> checkErrorCode r

   case addr of
      Nothing -> call nullPtr nullPtr
      Just a  -> with a $ \a' -> 
         with (sizeOf' a) $ \sptr -> call a' sptr

receiveBuffer :: MonadInIO m => Handle -> Int -> SendReceiveFlags -> Excepts '[ErrorCode] m Buffer
receiveBuffer fd size flags = do
   b <- liftIO <| mallocBytes (fromIntegral size)
   sz <- sysReceive fd b (fromIntegral size) flags (Nothing :: Maybe Int)
         -- free the buffer on error
         |> onE_ (liftIO (free b))
   -- otherwise make a bytestring
   bufferPackPtr (fromIntegral sz) (castPtr b)
