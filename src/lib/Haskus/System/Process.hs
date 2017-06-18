-- | Process utilities
module Haskus.System.Process
   ( threadDelaySec
   , threadDelayMilliSec
   , threadDelayMicroSec
   , threadWaitRead
   , threadWaitWrite
   , yield
   , sysFork
   )
where

import Haskus.System.Sys
import Haskus.System.Linux.Handle
import Haskus.Utils.Flow

import System.Posix.Types (Fd(..))
import qualified Control.Concurrent as CC

-- | Delay the thread (seconds)
threadDelaySec :: MonadIO m => Word -> m ()
threadDelaySec = threadDelayMicroSec . (*1000000)

-- | Delay the thread (milliseconds)
threadDelayMilliSec :: MonadIO m => Word -> m ()
threadDelayMilliSec = threadDelayMicroSec . (*1000)

-- | Delay the thread (microseconds)
threadDelayMicroSec :: MonadIO m => Word -> m ()
threadDelayMicroSec = liftIO . CC.threadDelay . fromIntegral

-- | Wait until a handle is readable
threadWaitRead :: MonadIO m => Handle -> m ()
threadWaitRead h = liftIO (CC.threadWaitRead (handleToFd h))

-- | Wait until a handle is writeable
threadWaitWrite :: MonadIO m => Handle -> m ()
threadWaitWrite h = liftIO (CC.threadWaitWrite (handleToFd h))

-- | Convert a handle into an Fd
handleToFd :: Handle -> Fd
handleToFd (Handle fd) = Fd (fromIntegral fd)

-- | Switch to another thread cooperatively
yield :: MonadIO m => m ()
yield = liftIO CC.yield

-- | Fork a thread
sysFork :: String -> Sys () -> Sys ()
sysFork name f = do
   act <- forkSys name f
   void $ liftIO $ CC.forkIO act
