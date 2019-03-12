{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Linux time
module Haskus.System.Linux.Time
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

import Haskus.Format.Binary.Word
import Foreign.Ptr
import Haskus.Format.Binary.Storable
import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Syscalls
import Haskus.Utils.Flow
import Haskus.Utils.Types.Generics (Generic)


-- | Time spec
data TimeSpec = TimeSpec
   { tsSeconds      :: {-# UNPACK #-} !Int64
   , tsNanoSeconds  :: {-# UNPACK #-} !Int64
   } deriving (Show,Eq,Ord,Generic,Storable)

-- | Time val
data TimeVal = TimeVal
   { tvSeconds       :: Word64
   , tvMicroSeconds  :: Word64
   } deriving (Show,Eq,Ord,Generic,Storable)

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
sysClockGetTime :: MonadInIO m => Clock -> Flow '[ErrorCode] m TimeSpec
sysClockGetTime clk =
   alloca $ \(t :: Ptr TimeSpec) -> do
      checkErrorCode_ =<< liftIO (syscall_clock_gettime (fromEnum clk) (castPtr t))
      peek t

-- | Set clock time
sysClockSetTime :: MonadInIO m => Clock -> TimeSpec -> Flow '[ErrorCode] m ()
sysClockSetTime clk time =
   with time $ \(t :: Ptr TimeSpec) -> do
      checkErrorCode_ =<< liftIO (syscall_clock_settime (fromEnum clk) (castPtr t))

-- | Retrieve clock resolution
sysClockGetResolution :: MonadInIO m => Clock -> Flow '[ErrorCode] m TimeSpec
sysClockGetResolution clk =
   alloca $ \(t :: Ptr TimeSpec) -> do
      checkErrorCode_ =<< liftIO (syscall_clock_getres (fromEnum clk) (castPtr t))
      peek t

-- | Retrieve time of day
sysGetTimeOfDay :: MonadInIO m => Flow '[ErrorCode] m TimeVal
sysGetTimeOfDay =
   alloca $ \(tv :: Ptr TimeVal) -> do
      -- timezone arg is deprecated (NULL passed instead)
      checkErrorCode_ =<< liftIO (syscall_gettimeofday (castPtr tv) nullPtr)
      peek tv

-- | Set time of day
sysSetTimeOfDay :: MonadInIO m => TimeVal -> Flow '[ErrorCode] m ()
sysSetTimeOfDay tv =
   with tv $ \ptv -> do
      -- timezone arg is deprecated (NULL passed instead)
      checkErrorCode_ =<< liftIO (syscall_settimeofday (castPtr ptv) nullPtr)

-- | Result of a sleep
data SleepResult
   = WokenUp TimeSpec   -- ^ Woken up by a signal, returns the remaining time to sleep
   | CompleteSleep      -- ^ Sleep completed
   deriving (Show,Eq,Ord)

-- | Suspend the calling thread for the specified amount of time
--
-- Can be interrupted by a signal (in this case it returns the remaining time)
sysNanoSleep :: MonadInIO m => TimeSpec -> Flow '[ErrorCode] m SleepResult
sysNanoSleep ts =
   with ts $ \ts' ->
      alloca $ \(rem' :: Ptr TimeSpec) -> do
         (liftIO (syscall_nanosleep (castPtr ts') (castPtr rem'))
            >>= checkErrorCode_
            >> return CompleteSleep)
               `catchLiftLeft` \case
                  EINTR -> WokenUp <$> peek rem'
                  err   -> throwE err

-- | Suspend the calling thread for the specified amount of time
--
-- When interrupted by a signal, suspend again for the remaining amount of time
nanoSleep :: MonadInIO m => TimeSpec -> Flow '[ErrorCode] m ()
nanoSleep ts = sysNanoSleep ts >>= \case
      CompleteSleep -> return ()
      WokenUp r     -> nanoSleep r
