-- | Process utilities
module ViperVM.System.Process
   ( threadDelaySec
   , threadDelayMilliSec
   , threadDelayMicroSec
   , sysFork
   )
where

import ViperVM.System.Sys
import ViperVM.Utils.Flow

import Control.Concurrent

-- | Delay the thread (seconds)
threadDelaySec :: Word -> Sys ()
threadDelaySec = threadDelayMicroSec . (*1000000)

-- | Delay the thread (milliseconds)
threadDelayMilliSec :: Word -> Sys ()
threadDelayMilliSec = threadDelayMicroSec . (*1000)

-- | Delay the thread (microseconds)
threadDelayMicroSec :: Word -> Sys ()
threadDelayMicroSec = liftIO . threadDelay . fromIntegral

-- | Fork a thread
sysFork :: String -> Sys () -> Sys ()
sysFork name f = do
   act <- forkSys name f
   void $ liftIO $ forkIO act
