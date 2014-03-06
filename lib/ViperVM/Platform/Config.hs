-- | Platform configuration
module ViperVM.Platform.Config (
   Platform(..), PlatformConfig(..), defaultConfig
) where

import qualified ViperVM.Platform.OpenCL as CL
import ViperVM.Platform.Types

-- | Platform configuration
data PlatformConfig = PlatformConfig {
   libraryOpenCL :: String,                        -- ^ OpenCL library to use
   filterOpenCLDevices :: CL.Device -> IO Bool,    -- ^ Function to filter out OpenCL devices
   sysfsPath :: String                             -- ^ Path to SysFS mount point
}

-- | Default platform configuration
defaultConfig :: PlatformConfig
defaultConfig = PlatformConfig {
   libraryOpenCL = "libOpenCL.so",
   filterOpenCLDevices = const (return True),
   sysfsPath = "/sys"
}

-- | Platform
data Platform = Platform {
   platformMemories :: [Memory],
   platformNetworks :: [Network],
   platformProcs :: [Proc],
   -- OpenCL specific
   platformOpenCLPlatforms :: [CL.Platform]
}

