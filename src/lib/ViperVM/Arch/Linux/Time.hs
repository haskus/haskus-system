{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- | Linux time
module ViperVM.Arch.Linux.Time
   ( TimeSpec(..)
   , TimeVal(..)
   , timeValDiff
   , Clock(..)
   , sysClockGetTime
   , sysClockSetTime
   , sysClockGetResolution
   , sysGetTimeOfDay
   , sysSetTimeOfDay
   , SleepResult(..)
   , sysNanoSleep
   , nanoSleep
   )
where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)

import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Ptr (Ptr,nullPtr)
import ViperVM.Format.Binary.Storable
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Utils.Flow
import ViperVM.Utils.Types.Generics (Generic)


-- | Time spec
data TimeSpec = TimeSpec {
   tsSeconds      :: {-# UNPACK #-} !Int64,
   tsNanoSeconds  :: {-# UNPACK #-} !Int64
} deriving (Show,Eq,Ord,Generic,CStorable)

instance Storable TimeSpec where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

-- | Time val
data TimeVal = TimeVal
   { tvSeconds       :: Word64
   , tvMicroSeconds  :: Word64
   } deriving (Show, Eq, Ord, Generic, CStorable)

instance Storable TimeVal where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

-- | Timeval difference in microseconds
timeValDiff :: TimeVal -> TimeVal -> Word64
timeValDiff (TimeVal s1 ms1) (TimeVal s2 ms2) = r
   where
      r = (s1-s2)*1000000 + ms1 - ms2


-- | Clocks
data Clock
   = ClockWall             -- ^ System-wide wall clock
   | ClockMonotonic        -- ^ Monotonic clock from unspecified starting point
   | ClockProcess          -- ^ Per-process CPU-time clock (CPU time consumed by all threads in the process)
   | ClockThread           -- ^ Thread-specific CPU-time clock
   | ClockRawMonotonic     -- ^ Hardware-based monotonic clock
   | ClockCoarseWall       -- ^ Faster but less precise wall clock
   | ClockCoarseMonotonic  -- ^ Faster but less precise monotonic clock
   | ClockBoot             -- ^ Monotonic clock that includes any time that the system is suspended
   | ClockWallAlarm        -- ^ Like wall clock, but timers on this clock can wake-up a suspended system
   | ClockBootAlarm        -- ^ Like boot clock, but timers on this clock can wake-up a suspended system
   | ClockTAI              -- ^ Like wall clock but in International Atomic Time
   deriving (Show,Eq,Ord)

instance Enum Clock where
   fromEnum x = case x of
      ClockWall             -> 0
      ClockMonotonic        -> 1
      ClockProcess          -> 2
      ClockThread           -> 3
      ClockRawMonotonic     -> 4
      ClockCoarseWall       -> 5
      ClockCoarseMonotonic  -> 6
      ClockBoot             -> 7
      ClockWallAlarm        -> 8
      ClockBootAlarm        -> 9
      ClockTAI              -> 11
   toEnum x = case x of
      0  -> ClockWall
      1  -> ClockMonotonic
      2  -> ClockProcess
      3  -> ClockThread
      4  -> ClockRawMonotonic
      5  -> ClockCoarseWall
      6  -> ClockCoarseMonotonic
      7  -> ClockBoot
      8  -> ClockWallAlarm
      9  -> ClockBootAlarm
      11 -> ClockTAI
      _  -> error "Unknown clock"

-- | Retrieve clock time
sysClockGetTime :: Clock -> IOErr TimeSpec
sysClockGetTime clk =
   alloca $ \(t :: Ptr TimeSpec) ->
      onSuccessIO (syscall_clock_gettime (fromEnum clk) t) (const $ peek t)

-- | Set clock time
sysClockSetTime :: Clock -> TimeSpec -> IOErr ()
sysClockSetTime clk time =
   with time $ \(t :: Ptr TimeSpec) ->
      onSuccess (syscall_clock_settime (fromEnum clk) t) (const ())

-- | Retrieve clock resolution
sysClockGetResolution :: Clock -> IOErr TimeSpec
sysClockGetResolution clk =
   alloca $ \(t :: Ptr TimeSpec) ->
      onSuccessIO (syscall_clock_getres (fromEnum clk) t) (const $ peek t)

-- | Retrieve time of day
sysGetTimeOfDay :: IOErr TimeVal
sysGetTimeOfDay =
   alloca $ \(tv :: Ptr TimeVal) ->
      -- timezone arg is deprecated (NULL passed instead)
      onSuccessIO (syscall_gettimeofday tv nullPtr) (const $ peek tv)

-- | Set time of day
sysSetTimeOfDay :: TimeVal -> IOErr ()
sysSetTimeOfDay tv =
   with tv $ \ptv ->
      -- timezone arg is deprecated (NULL passed instead)
      onSuccessVoid (syscall_settimeofday ptv nullPtr)

-- | Result of a sleep
data SleepResult
   = WokenUp TimeSpec   -- ^ Woken up by a signal, returns the remaining time to sleep
   | CompleteSleep      -- ^ Sleep completed
   deriving (Show,Eq,Ord)

-- | Suspend the calling thread for the specified amount of time
--
-- Can be interrupted by a signal (in this case it returns the remaining time)
sysNanoSleep :: TimeSpec -> IOErr SleepResult
sysNanoSleep ts =
   with ts $ \ts' ->
      alloca $ \(rem' :: Ptr TimeSpec) -> do
         ret <- syscall_nanosleep ts' rem'
         case defaultCheck ret of
            Nothing    -> flowRet0 CompleteSleep
            Just EINTR -> flowRet0 =<< (WokenUp <$> peek rem')
            Just err   -> flowRet1 err

-- | Suspend the calling thread for the specified amount of time
--
-- When interrupted by a signal, suspend again for the remaining amount of time
nanoSleep :: TimeSpec -> IOErr ()
nanoSleep ts = sysNanoSleep ts
   >.~^> \case
      CompleteSleep -> flowRet0 ()
      (WokenUp r)   -> nanoSleep r
