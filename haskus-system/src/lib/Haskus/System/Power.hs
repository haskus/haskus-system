-- | Power management
module Haskus.System.Power
   ( disableRebootKeys
   , enableRebootKeys
   , halt
   , executeLoadedKernel
   , powerOff
   , powerOff_
   , restart
   , restart_
   , restartWithCommand
   , softSuspend
   )
where

import Haskus.Utils.Flow
import qualified Haskus.System.Linux.Power as L
import Haskus.System.Linux.Error

-- | Ctrl-Alt-Del sequence sends SIGINT to init task.
disableRebootKeys :: MonadInIO m => m (VEither '[NotAllowed] ())
disableRebootKeys = runFlow L.disableRebootKeys

-- | Ctrl-Alt-Del sequence causes RESTART command.
enableRebootKeys :: MonadInIO m => m (VEither '[NotAllowed] ())
enableRebootKeys = runFlow L.enableRebootKeys

-- | Stop OS and give system control to ROM monitor, if any.
halt :: MonadInIO m => m (VEither '[NotAllowed] ())
halt = runFlow L.halt

-- | Restart system using a previously loaded Linux kernel
executeLoadedKernel :: MonadInIO m => m (VEither '[NotAllowed] ())
executeLoadedKernel = runFlow L.executeLoadedKernel

-- | Stop OS and remove all power from system, if possible.
powerOff :: MonadInIO m => m (VEither '[NotAllowed] ())
powerOff = runFlow L.powerOff

-- | Stop OS and remove all power from system, if possible.
powerOff_ :: MonadInIO m => m ()
powerOff_ = void powerOff

-- | Restart system using default command and mode.
restart :: MonadInIO m => m (VEither '[NotAllowed] ())
restart = runFlow L.restart

-- | Restart system using default command and mode.
restart_ :: MonadInIO m => m ()
restart_ = void restart

-- | Restart system using given command string.
restartWithCommand :: MonadInIO m => String -> m (VEither '[NotAllowed,MemoryError,InvalidRestartCommand] ())
restartWithCommand cmd = runFlow (L.restartWithCommand cmd)

-- | Suspend system using software suspend if compiled in.
softSuspend :: MonadInIO m => m (VEither '[NotAllowed] ())
softSuspend = runFlow L.softSuspend
