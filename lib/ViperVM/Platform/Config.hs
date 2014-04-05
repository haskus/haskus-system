-- | Platform configuration
module ViperVM.Platform.Config (
   PlatformConfig(..), defaultConfig
) where

import qualified ViperVM.Arch.OpenCL as CL

-- | Platform configuration
data PlatformConfig = PlatformConfig {

   -- OpenCL Configuration
   enableOpenCL :: Bool,                           -- ^ Enable OpenCL backend
   libraryOpenCL :: String,                        -- ^ OpenCL library to use
   filterOpenCLDevices :: CL.Device -> IO Bool,    -- ^ Function to filter out OpenCL devices

   -- Host configuration
   sysfsPath :: String                             -- ^ Path to SysFS mount point
}

-- | Default platform configuration
defaultConfig :: PlatformConfig
defaultConfig = PlatformConfig {

   -- OpenCL Configuration
   enableOpenCL = True,
   libraryOpenCL = "libOpenCL.so",
   filterOpenCLDevices = const (return True),

   -- Host configuration
   sysfsPath = "/sys"
}

