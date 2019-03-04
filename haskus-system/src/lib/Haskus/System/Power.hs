-- | Power management
module Haskus.System.Power
   ( disableRebootKeys
   , enableRebootKeys
   , halt
   , executeLoadedKernel
   , powerOff
   , restart
   , restartWithCommand
   , softSuspend
   )
where

import Haskus.Utils.Flow
import qualified Haskus.System.Linux.Power as L
import Haskus.System.Linux.Error

-- | Ctrl-Alt-Del sequence sends SIGINT to init task.
disableRebootKeys :: MonadInIO m => m (V '[(),NotAllowed])
disableRebootKeys = runFlowT L.disableRebootKeys

-- | Ctrl-Alt-Del sequence causes RESTART command.
enableRebootKeys :: MonadInIO m => m (V '[(),NotAllowed])
enableRebootKeys = runFlowT L.enableRebootKeys

-- | Stop OS and give system control to ROM monitor, if any.
halt :: MonadInIO m => m (V '[(),NotAllowed])
halt = runFlowT L.halt

-- | Restart system using a previously loaded Linux kernel
executeLoadedKernel :: MonadInIO m => m (V '[(),NotAllowed])
executeLoadedKernel = runFlowT L.executeLoadedKernel

-- | Stop OS and remove all power from system, if possible.
powerOff :: MonadInIO m => m (V '[(),NotAllowed])
powerOff = runFlowT L.powerOff

-- | Restart system using default command and mode.
restart :: MonadInIO m => m (V '[(),NotAllowed])
restart = runFlowT L.restart

-- | Restart system using given command string.
restartWithCommand :: MonadInIO m => String -> m (V '[(),NotAllowed,MemoryError,InvalidRestartCommand])
restartWithCommand cmd = runFlowT (L.restartWithCommand cmd)

-- | Suspend system using software suspend if compiled in.
softSuspend :: MonadInIO m => m (V '[(),NotAllowed])
softSuspend = runFlowT L.softSuspend
