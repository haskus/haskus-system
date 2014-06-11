module ViperVM.Arch.X86_64.Linux.Futex (
   FutexOp(..), sysFutex, sysFutexWait,
   sysFutexWake, sysFutexRequeue, sysFutexCmpRequeue
) where

import Foreign.Ptr
import Data.Int
import Control.Applicative

import ViperVM.Arch.X86_64.Linux.Syscall
import ViperVM.Arch.X86_64.Linux.ErrorCode
import ViperVM.Arch.X86_64.Linux.Time
import ViperVM.Arch.X86_64.Linux.Utils

data FutexOp =
     FutexWait
   | FutexWake
   | FutexFD
   | FutexRequeue
   | FutexCmpRequeue
   deriving (Show,Enum)

-- | All the Futex API uses this `futex` syscall
sysFutex :: Ptr Int64 -> FutexOp -> Int64 -> Ptr TimeSpec -> Ptr Int64 -> Int64 -> SysRet Int64
sysFutex uaddr op val timeout uaddr2 val3 =
   onSuccess (syscall6 202 uaddr (fromEnum op) val timeout uaddr2 val3) id

-- | Atomically check that addr contains val and sleep until it is wakened up or until the timeout expires
sysFutexWait :: Ptr Int64 -> Int64 -> Maybe TimeSpec -> SysRet ()
sysFutexWait addr val timeout =
   withMaybeOrNull timeout $ \timeout' ->
      fmap (const ()) <$> sysFutex addr FutexWait val timeout' nullPtr 0

-- | Wake `count` processes waiting on the futex
--  Return the number of processes woken up
sysFutexWake :: Ptr Int64 -> Int64 -> SysRet Int64
sysFutexWake addr count =
   sysFutex addr FutexWake count nullPtr nullPtr 0


-- | Wake `count` processes waiting on the first futex
-- and requeue the other ones on the second futex.
--
-- Return the number of processes woken up
sysFutexRequeue :: Ptr Int64 -> Int64 -> Ptr Int64 -> SysRet Int64
sysFutexRequeue addr count addr2 =
   sysFutex addr FutexRequeue count nullPtr addr2 0

-- | Atomically compare the first futex with `val, then
-- wake `count` processes waiting on the first futex
-- and requeue the other ones on the second futex.
--
-- Return the number of processes woken up
sysFutexCmpRequeue :: Ptr Int64 -> Int64 -> Int64 -> Ptr Int64 -> SysRet Int64
sysFutexCmpRequeue addr val count addr2 =
   sysFutex addr FutexCmpRequeue count nullPtr addr2 val
