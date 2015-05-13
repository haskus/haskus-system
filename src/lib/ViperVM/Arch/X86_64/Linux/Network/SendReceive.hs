module ViperVM.Arch.X86_64.Linux.Network.SendReceive
   ( SendReceiveFlag(..)
   , SendReceiveFlags
   , sysReceive
   )
where


import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable
import Foreign.Marshal.Utils
import Data.Word

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.X86_64.Linux.Syscall
import ViperVM.Utils.BitSet (EnumBitSet, BitSet)
import qualified ViperVM.Utils.BitSet as BitSet


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
   deriving (Show,Eq)

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

instance EnumBitSet SendReceiveFlag

type SendReceiveFlags = BitSet Word64 SendReceiveFlag

-- | Receive data from a socket
--
-- recvfrom syscall
sysReceive :: Storable a => FileDescriptor -> Ptr () -> Word64 -> SendReceiveFlags -> Maybe a -> SysRet Word64
sysReceive (FileDescriptor fd) ptr size flags addr = do
   let
      call :: Ptr a -> Ptr Word64 -> SysRet Word64
      call add len = onSuccess (syscall6 45 fd ptr size (BitSet.toBits flags) add len) fromIntegral

   case addr of
      Nothing -> call nullPtr nullPtr
      Just a  -> with a $ \a' -> 
         with (fromIntegral (sizeOf a)) $ \sptr -> call a' sptr
