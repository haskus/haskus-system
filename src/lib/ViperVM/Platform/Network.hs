-- | Network
module ViperVM.Platform.Network
   ( NetworkUID
   , networkUID
   )
where

import ViperVM.Platform.Topology
import qualified ViperVM.Platform.Drivers as Peer
import qualified ViperVM.Platform.Drivers.OpenCL as OpenCL

newtype NetworkUID = NetworkUID String deriving (Read, Show, Eq, Ord)

-- | Network unique identifier (not stable between different program executions)
networkUID :: Network -> NetworkUID
networkUID mem = NetworkUID $ case networkPeer mem of
   Peer.OpenCLNetwork m -> OpenCL.clNetUID m
