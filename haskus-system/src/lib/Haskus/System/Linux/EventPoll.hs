{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Event polling
module Haskus.System.Linux.EventPoll
   ( EventPollFlag(..)
   , sysEventPollCreate
   )
where

import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Syscalls
import Haskus.System.Linux.Handle
import Haskus.Format.Binary.Word (Word64)
import Haskus.Format.Binary.Bits ((.|.))
import Haskus.Utils.List (foldl')
import Haskus.Utils.Flow

-- | Polling flag
data EventPollFlag
   = EventPollCloseOnExec
   deriving (Show,Eq)

fromFlag :: EventPollFlag -> Word64
fromFlag EventPollCloseOnExec = 0x80000

fromFlags :: [EventPollFlag] -> Word64
fromFlags = foldl' (.|.) 0 . fmap fromFlag

-- | Create event poller
sysEventPollCreate :: MonadIO m => [EventPollFlag] -> Flow '[ErrorCode] m Handle
sysEventPollCreate flags = do
   n <- checkErrorCode =<< liftIO (syscall_epoll_create1 (fromFlags flags))
   return (Handle (fromIntegral n))
