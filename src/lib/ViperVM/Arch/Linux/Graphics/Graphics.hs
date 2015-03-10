-- | Interface to Linux graphics API
--
-- Linux currently uses KMS/DRM interface
module ViperVM.Arch.Linux.Graphics.Graphics
   ( drmIoctl
   )
where

import ViperVM.Arch.Linux.Ioctl

-- | IOCTL for DRM is restarted on interruption
-- Apply this function to your preferred ioctl function
drmIoctl :: IOCTL -> IOCTL
drmIoctl = repeatIoctl
