module ViperVM.Platform.NetworkPeer (
   PPPLinkPeer(..)
) where

import qualified ViperVM.Arch.OpenCL.All as CL

-- | Backend specific fields for point-to-point links
data PPPLinkPeer = 
     OpenCLLink {
         clLinkDevice :: CL.Device,
         clLinkContext :: CL.Context,
         clLinkQueue :: CL.CommandQueue
     }

