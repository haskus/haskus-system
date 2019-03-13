{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Futex (user-space mutex)
module Haskus.System.Linux.Futex
   ( FutexOp(..)
   , sysFutex
   , futexWait
   , futexWake
   , futexRequeue
   , futexCompareRequeue
   )
where

import Foreign.Ptr
import Haskus.Format.Binary.Word
import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Syscalls
import Haskus.System.Linux.Time
import Haskus.Memory.Utils
import Haskus.Utils.Flow

-- | Futex operation
data FutexOp
   = FutexWait
   | FutexWake
   | FutexFD
   | FutexRequeue
   | FutexCmpRequeue
   deriving (Show,Enum)

-- | All the Futex API uses this `futex` syscall
sysFutex :: MonadIO m => Ptr Int64 -> FutexOp -> Int64 -> Ptr TimeSpec -> Ptr Int64 -> Int64 -> Excepts '[ErrorCode] m Int64
sysFutex uaddr op val timeout uaddr2 val3 =
   checkErrorCode =<< liftIO (syscall_futex uaddr (fromEnum op) val (castPtr timeout) uaddr2 val3)

-- | Atomically check that addr contains val and sleep until it is wakened up or until the timeout expires
futexWait :: MonadInIO m => Ptr Int64 -> Int64 -> Maybe TimeSpec -> Excepts '[ErrorCode] m ()
futexWait addr val timeout =
   withMaybeOrNull timeout $ \timeout' ->
      void (sysFutex addr FutexWait val timeout' nullPtr 0)

-- | Wake `count` processes waiting on the futex
--  Return the number of processes woken up
futexWake :: MonadIO m => Ptr Int64 -> Int64 -> Excepts '[ErrorCode] m Int64
futexWake addr count =
   sysFutex addr FutexWake count nullPtr nullPtr 0


-- | Wake `count` processes waiting on the first futex
-- and requeue the other ones on the second futex.
--
-- Return the number of processes woken up
futexRequeue :: MonadIO m => Ptr Int64 -> Int64 -> Ptr Int64 -> Excepts '[ErrorCode] m Int64
futexRequeue addr count addr2 =
   sysFutex addr FutexRequeue count nullPtr addr2 0

-- | Atomically compare the first futex with `val, then
-- wake `count` processes waiting on the first futex
-- and requeue the other ones on the second futex.
--
-- Return the number of processes woken up
futexCompareRequeue :: MonadIO m => Ptr Int64 -> Int64 -> Int64 -> Ptr Int64 -> Excepts '[ErrorCode] m Int64
futexCompareRequeue addr val count addr2 =
   sysFutex addr FutexCmpRequeue count nullPtr addr2 val
