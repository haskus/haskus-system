{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Event polling
module ViperVM.Arch.Linux.EventPoll
   ( EventPollFlag(..)
   , sysEventPollCreate
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Arch.Linux.Handle
import ViperVM.Format.Binary.Word (Word64)
import ViperVM.Format.Binary.Bits ((.|.))
import ViperVM.Utils.List (foldl')
import ViperVM.Utils.Flow

-- | Polling flag
data EventPollFlag
   = EventPollCloseOnExec
   deriving (Show,Eq)

fromFlag :: EventPollFlag -> Word64
fromFlag EventPollCloseOnExec = 0x80000

fromFlags :: [EventPollFlag] -> Word64
fromFlags = foldl' (.|.) 0 . fmap fromFlag

-- | Create event poller
sysEventPollCreate :: MonadIO m => [EventPollFlag] -> Flow m '[Handle,ErrorCode]
sysEventPollCreate flags =
   liftIO (syscall @"epoll_create1" (fromFlags flags))
      ||> toErrorCodePure (Handle . fromIntegral)
