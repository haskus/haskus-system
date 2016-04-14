module ViperVM.System.Process
   ( threadDelaySec
   , threadDelayMilliSec
   , threadDelayMicroSec
   , sysFork
   )
where

import ViperVM.System.Sys

import Control.Concurrent
import Control.Monad

-- | Delay the thread (seconds)
threadDelaySec :: Word -> Sys ()
threadDelaySec = threadDelayMicroSec . (*1000000)

-- | Delay the thread (milliseconds)
threadDelayMilliSec :: Word -> Sys ()
threadDelayMilliSec = threadDelayMicroSec . (*1000)

-- | Delay the thread (microseconds)
threadDelayMicroSec :: Word -> Sys ()
threadDelayMicroSec = sysIO . threadDelay . fromIntegral

-- | Fork a thread
-- TODO: fork log
sysFork :: Sys () -> Sys ()
sysFork = void . sysIO . forkIO . runSys
