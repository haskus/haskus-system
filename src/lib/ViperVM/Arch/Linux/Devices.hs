-- | Linux device handling
--
-- Devices in the kernel are identified with two numbers (major and minor) and
-- their type (character or block).
--
-- For each device, there is a 1-1 correspondance with some paths in sysfs's
-- /devices directory:
--
--    * type/major/minor -> sysfs path
--       Look at target of symbolic link /dev/{block,char}/MAJOR:MINOR
--
--    * sysfs path -> type/major/minor
--       In the sysfs's device directory (/devices/**):
--          * type: if basename of "subsystem" link is "block" then block else
--          character
--          * major/minor: read contents of "dev" file
--
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
data Device = Device
   { deviceType :: !DeviceType               -- ^ Device type
   , deviceID   :: {-# UNPACK #-} !DeviceID  -- ^ Device major and minor
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

