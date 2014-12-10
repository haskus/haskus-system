-- | Linux Input management
--
-- Bindings from linux/input.h
module ViperVM.Arch.Linux.Input
   ( TimeVal (..)
   , Event (..)
   , getDriverVersion
   )
where

import Data.Word
import Data.Int
import Data.Bits
import Foreign.Storable
import Foreign.Ptr

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Ioctl

data TimeVal = TimeVal
   { seconds  :: Word64
   , useconds :: Word64
   }

-- | Input event
data Event = Event
   { eventTime  :: TimeVal
   , eventType  :: Word16
   , eventCode  :: Word16
   , eventValue :: Int32
   }

-- | Protocol version
protocolVersion :: Int
protocolVersion = 0x010001

-- | Device info
--
-- `struct input_id` in C header file
data DeviceInfo = DeviceInfo
   { infoBusType :: Word16
   , infoVendor  :: Word16
   , infoProduct :: Word16
   , infoVersion :: Word16
   }

-- | Absolute info
--
-- `struct input_absinfo` in C header file
data AbsoluteInfo = AbsoluteInfo
   { absValue      :: Int32   -- ^ Latest reported value for the axis
   , absMinimum    :: Int32   -- ^ Minimum value for the axis
   , absMAximum    :: Int32   -- ^ Maximum value for the axis
   , absFuzz       :: Int32   -- ^ Fuzz value used to filter noise from the event stream
   , absFlat       :: Int32   -- ^ Values that are within this value will be discarded and reported as 0 instead
   , absResolution :: Int32   -- ^ Resolution for the values reported for the axis
   }


-- | Query or modify keymap data
data KeymapEntry = KeymapEntry
   { keymapEntryFlags    :: Word8    -- ^ Indicate how kernel should handle the request
   , keymapEntryLength   :: Word8    -- ^ Length of the scancode (TODO: remove this in the Storable instance)
   , keymapEntryIndex    :: Word16   -- ^ Index in the keymap (may be used instead of the scancode)
   , keymapEntryKeyCode  :: Word32   -- ^ Key code assigned to this scancode
   , keymapEntryScanCode :: [Word8]  -- ^ Scan in machine-endian form (up to 32 bytes)
   }

-- | A flag for KeymapEntry
inputKeymapByIndexFlag :: Word8
inputKeymapByIndexFlag = 1


getDriverVersion :: IOCTL -> FileDescriptor -> SysRet Int
getDriverVersion ioctl = ioctlRead ioctl 0x45 0x01 (const Nothing)


