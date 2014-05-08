module ViperVM.Platform.Platform (
   Platform(..)
) where

import ViperVM.Platform.Topology
import qualified ViperVM.Arch.OpenCL.All as CL

-- | Platform
data Platform = Platform {
   platformMemories :: [Memory],
   platformNetworks :: [Network],
   platformProcs :: [Proc],

   -- OpenCL specific
   platformOpenCLPlatforms :: [CL.Platform]
}
