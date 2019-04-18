{-# LANGUAGE DataKinds #-}

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
disableRebootKeys = runE L.disableRebootKeys

-- | Ctrl-Alt-Del sequence causes RESTART command.
enableRebootKeys :: MonadInIO m => m (VEither '[NotAllowed] ())
enableRebootKeys = runE L.enableRebootKeys

-- | Stop OS and give system control to ROM monitor, if any.
halt :: MonadInIO m => m (VEither '[NotAllowed] ())
halt = runE L.halt

-- | Restart system using a previously loaded Linux kernel
executeLoadedKernel :: MonadInIO m => m (VEither '[NotAllowed] ())
executeLoadedKernel = runE L.executeLoadedKernel

-- | Stop OS and remove all power from system, if possible.
powerOff :: MonadInIO m => m (VEither '[NotAllowed] ())
powerOff = runE L.powerOff

-- | Stop OS and remove all power from system, if possible.
powerOff_ :: MonadInIO m => m ()
powerOff_ = void powerOff

-- | Restart system using default command and mode.
restart :: MonadInIO m => m (VEither '[NotAllowed] ())
restart = runE L.restart

-- | Restart system using default command and mode.
restart_ :: MonadInIO m => m ()
restart_ = void restart

-- | Restart system using given command string.
restartWithCommand :: MonadInIO m => String -> m (VEither '[NotAllowed,MemoryError,InvalidRestartCommand] ())
restartWithCommand cmd = runE (L.restartWithCommand cmd)

-- | Suspend system using software suspend if compiled in.
softSuspend :: MonadInIO m => m (VEither '[NotAllowed] ())
softSuspend = runE L.softSuspend
