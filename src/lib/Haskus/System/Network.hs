-- | Networking
module Haskus.System.Network
   ( createKernelEventSocket
   , receiveKernelEvent
   )
where

import qualified Haskus.Format.Binary.BitSet as BitSet
import Haskus.Arch.Linux.KernelEvent
import Haskus.Arch.Linux.Network
import Haskus.Arch.Linux.Handle
import Haskus.Arch.Linux.Network.SendReceive
import Haskus.Utils.Flow
import Haskus.System.Sys

-- | Create a socket for kernel events
createKernelEventSocket :: Sys Handle
createKernelEventSocket = sysLogSequence "Create kernel event socket" $ do
   -- internally the socket is a Netlink socket dedicated to kernel events
   fd <- sysSocket (SockTypeNetlink NetlinkTypeKernelEvent) []
         |> flowAssert "Create NetLink socket"
   -- bind the socket to any port (i.e. port 0), listen to all multicast groups
   sysBindNetlink fd 0 0xFFFFFFFF
      |> flowAssert "Bind NetLink socket"
   return fd


-- | Block until a kernel event is received
receiveKernelEvent :: Handle -> Sys KernelEvent
receiveKernelEvent fd = go
   where
      go = do
         msg <- receiveBuffer fd 2048 BitSet.empty
                  |> flowAssertQuiet "Receive kernel event"
         case parseKernelEvent msg of
            Just m  -> return m
            Nothing -> go -- invalid event received


