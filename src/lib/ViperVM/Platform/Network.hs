-- | Network
module ViperVM.Platform.Network
   ( networkUID
   )
where

import ViperVM.Platform.Topology
import qualified ViperVM.Platform.Drivers as Peer
import qualified ViperVM.Platform.Drivers.OpenCL as OpenCL

-- | Network unique identifier (not stable between different program executions)
networkUID :: Network -> String
networkUID mem = case networkPeer mem of
   Peer.OpenCLNetwork m -> OpenCL.clNetUID m
