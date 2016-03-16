module ViperVM.System.Process
   ( threadDelaySec
   , threadDelayMilliSec
   )
where

import ViperVM.Arch.Linux.Error

import Control.Concurrent

-- | Delay the thread (seconds)
threadDelaySec :: Word -> Sys ()
threadDelaySec = threadDelayMilliSec . (*1000)

-- | Delay the thread (milliseconds)
threadDelayMilliSec :: Word -> Sys ()
threadDelayMilliSec = sysIO . threadDelay . fromIntegral
