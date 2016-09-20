-- | Linux device handling
module ViperVM.Arch.Linux.Devices
   ( Device(..)
   , makeDevice
   , DeviceType(..)
   , DeviceID(..)
   , createDeviceFile
   )
where

import ViperVM.Format.Binary.Word

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.FileSystem

-- | Device
--
-- Devices in the kernel are identified with two numbers (major and minor) and
-- their type (character or block).
--
-- For each device, there is a 1-1 correspondance with a path in sysfs's
-- /devices. The correspondance can be obtained by looking into sysfs's
-- /dev/{block,char} directories or by looking into "dev" files in sysfs's
-- directory for each device.
data Device = Device
   { deviceType :: !DeviceType
   , deviceID   :: {-# UNPACK #-} !DeviceID
   }
   deriving (Show,Eq,Ord)

-- | Create a device identigier
makeDevice :: DeviceType -> Word32 -> Word32 -> Device
makeDevice typ major minor = Device typ (DeviceID major minor)

-- | Device type
data DeviceType
   = CharDevice   -- ^ Character device
   | BlockDevice  -- ^ Block device
   deriving (Show,Eq,Ord)

-- | Create a device special file
createDeviceFile :: Maybe Handle -> FilePath -> Device -> FilePermissions -> SysRet ()
createDeviceFile hdl path dev perm = sysCreateSpecialFile hdl path typ perm (Just devid)
   where
      devid = deviceID dev
      typ   = case deviceType dev of
                  CharDevice  -> FileTypeCharDevice
                  BlockDevice -> FileTypeBlockDevice

