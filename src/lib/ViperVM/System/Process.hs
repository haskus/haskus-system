module ViperVM.System.Process
   ( threadDelaySec
   , threadDelayMilliSec
   , threadDelayMicroSec
   )
where

import ViperVM.System.Sys

import Control.Concurrent

-- | Delay the thread (seconds)
threadDelaySec :: Word -> Sys ()
threadDelaySec = threadDelayMicroSec . (*1000000)

-- | Delay the thread (milliseconds)
threadDelayMilliSec :: Word -> Sys ()
threadDelayMilliSec = threadDelayMicroSec . (*1000)

-- | Delay the thread (microseconds)
threadDelayMicroSec :: Word -> Sys ()
threadDelayMicroSec = sysIO . threadDelay . fromIntegral
