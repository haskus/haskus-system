-- | Kernel events are sent by the kernel to indicate that something
-- changed in the device tree (e.g. device (un)plugged, moved, etc.)
module ViperVM.Arch.Linux.System.KernelEvent
   ( KernelEvent(..)
   , KernelEventAction(..)
   , createKernelEventSocket
   , receiveKernelEvent
   )
where

import ViperVM.Arch.X86_64.Linux.Network
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Network.SendReceive

import Control.Monad.Trans.Either
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.ByteString.UTF8 as UTF8
import Data.List.Split

-- | A kernel event
data KernelEvent = KernelEvent
   { kernelEventAction    :: KernelEventAction     -- ^ What happened
   , kernelEventDevPath   :: String                -- ^ Concerned device
   , kernelEventSubSystem :: String                -- ^ Device subsystem
   , kernelEventDetails   :: Map.Map String String -- ^ Event details
   } deriving Show

-- | Kernel event type of action
data KernelEventAction
   = ActionAdd             -- ^ A device has been added
   | ActionRemove          -- ^ A device has been removed
   | ActionChange          -- ^ A device state has been modified
   | ActionOnline          -- ^ A device is now on-line
   | ActionOffline         -- ^ A device is now off-line
   | ActionMove            -- ^ A device has been moved
   | ActionOther String    -- ^ Other action
   deriving (Show)

-- | Create a socket for kernel events
createKernelEventSocket :: SysRet FileDescriptor
createKernelEventSocket = runEitherT $ do
   -- internally the socket is a Netlink socket dedicated to kernel events
   fd <- EitherT $ sysSocket (SockTypeNetlink NetlinkTypeKernelEvent) []
   -- bind the socket to any port (i.e. port 0), listen to all multicast groups
   EitherT $ sysBindNetlink fd 0 0xFFFFFFFF
   return fd


-- | Block until a kernel event is received
receiveKernelEvent :: FileDescriptor -> SysRet KernelEvent
receiveKernelEvent fd = runEitherT $ do
   msg <- parseKernelEvent <$> EitherT (receiveByteString fd 2048 [])
   case msg of
      Nothing -> EitherT (receiveKernelEvent fd)
      Just m  -> return m


-- | Parse a kernel event
--
-- Kernel events are received as several zero-terminal strings. The first line
-- isn't very useful because it is redundant with the content of the following
-- lines. The following lines have the "key=value" format.
--
-- Note: when kernel event sockets are used with a classic Linux distribution
-- using udev, libudev injects its own events with their own syntax. Hence we
-- discard them (they all begin with "libudev" characters).
parseKernelEvent :: BS.ByteString -> Maybe KernelEvent
parseKernelEvent bs = r
   where
      bss = fmap UTF8.toString (BS.split 0 bs)
      r = case bss of
            -- filter out injected libudev events
            ("libudev":_) -> Nothing
            _             -> Just (KernelEvent action devpath subsys details)

      -- parse fields
      fields = Map.fromList                  -- create Map from (key,value) tuples
             . fmap (toTuple . splitOn "=")  -- split "key=value"
             . filter (/= "")                -- drop empty lines
             $ tail bss                      -- drop the first line (it contains redundant info)

      action = case fields Map.! "ACTION" of
         "add"    -> ActionAdd
         "remove" -> ActionRemove
         "change" -> ActionChange
         "online" -> ActionOnline
         "offline"-> ActionOffline
         "move"   -> ActionMove
         x        -> ActionOther x

      devpath = fields Map.! "DEVPATH"

      subsys  = fields Map.! "SUBSYSTEM"

      -- remove mandatory fields for the "detais" field
      details = Map.delete "SUBSYSTEM" 
              $ Map.delete "ACTION" 
              $ Map.delete "DEVPATH" fields

      toTuple (x:y:_) = (x,y)
      toTuple x = error $ "Invalid tuple: " ++ show x
