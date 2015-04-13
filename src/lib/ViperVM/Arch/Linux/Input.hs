{-# LANGUAGE DeriveGeneric #-}

-- We need this one to use type literal numbers (S (S .. Z)) of size 32
{-# OPTIONS -fcontext-stack=50 #-}

-- | Linux Input management
--
-- Bindings from linux/input.h
module ViperVM.Arch.Linux.Input
   ( TimeVal (..)
   , Event (..)
   , DeviceInfo(..)
   , AbsoluteInfo(..)
   , KeymapEntry(..)
   , RepeatSettings(..)
   , protocolVersion
   , inputKeymapByIndexFlag
   , getDriverVersion
   , getDeviceInfo
   , getRepeatSettings
   , setRepeatSettings
   , getKeyCode
   , setKeyCode
   , getDeviceName
   , getDevicePhysicalLocation
   , getDeviceUniqueID
   , getDeviceProperties
   )
where

import Data.Word
import Data.Int
import Foreign.Storable
import Foreign.CStorable
import Foreign.C.String (peekCString)
import GHC.Generics (Generic)

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Ioctl

import Data.Vector.Fixed.Cont (S,Z)
import Data.Vector.Fixed.Storable (Vec)

type N32 = -- 32 
   S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (
   S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z
   )))))))))))))))))))))))))))))))

data TimeVal = TimeVal
   { seconds  :: Word64
   , useconds :: Word64
   } deriving (Show,Eq)

-- | Input event
data Event = Event
   { eventTime  :: TimeVal
   , eventType  :: Word16
   , eventCode  :: Word16
   , eventValue :: Int32
   } deriving (Show,Eq)

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
   } deriving (Show,Eq,Generic)

instance CStorable DeviceInfo
instance Storable DeviceInfo where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke


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
   } deriving (Show, Eq)


-- | Query or modify keymap data
--
-- `struct input_keymap_entry` in C header file
data KeymapEntry = KeymapEntry
   { keymapEntryFlags    :: Word8    -- ^ Indicate how kernel should handle the request
   , keymapEntryLength   :: Word8    -- ^ Length of the scancode (TODO: remove this in the Storable instance)
   , keymapEntryIndex    :: Word16   -- ^ Index in the keymap (may be used instead of the scancode)
   , keymapEntryKeyCode  :: Word32   -- ^ Key code assigned to this scancode
   , keymapEntryScanCode :: StorableWrap (Vec N32 Word8) -- ^ Scan in machine-endian form (up to 32 bytes)
   } deriving (Generic)


instance CStorable KeymapEntry
instance Storable KeymapEntry where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- | A flag for KeymapEntry
inputKeymapByIndexFlag :: Word8
inputKeymapByIndexFlag = 1

-- | Get driver version
--
-- EVIOCGVERSION
getDriverVersion :: IOCTL -> FileDescriptor -> SysRet Int32
getDriverVersion ioctl = ioctlRead ioctl 0x45 0x01 defaultCheck


-- | Get device info
--
-- EVIOCGID
getDeviceInfo :: IOCTL -> FileDescriptor -> SysRet DeviceInfo
getDeviceInfo ioctl = ioctlRead ioctl 0x45 0x02 defaultCheck

data RepeatSettings = RepeatSettings
   { repeatDelay  :: Int32
   , repeatPeriod :: Int32
   }
   deriving (Show,Eq,Generic)

instance CStorable RepeatSettings
instance Storable RepeatSettings where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- | Get repeat settings
--
-- EVIOCGREP
getRepeatSettings :: IOCTL -> FileDescriptor -> SysRet RepeatSettings
getRepeatSettings ioctl = ioctlRead ioctl 0x45 0x03 defaultCheck

-- | Set repeat settings
--
-- EVIOCSREP
setRepeatSettings :: IOCTL -> FileDescriptor -> RepeatSettings -> SysRet ()
setRepeatSettings ioctl = ioctlWrite ioctl 0x45 0x03 defaultCheckRet


-- | Get key code
--
-- EVIOCGKEYCODE_V2
getKeyCode :: IOCTL -> FileDescriptor -> SysRet KeymapEntry
getKeyCode ioctl = ioctlRead ioctl 0x45 0x04 defaultCheck

-- | Set key code
--
-- EVIOCSKEYCODE_V2
setKeyCode :: IOCTL -> FileDescriptor -> KeymapEntry -> SysRet ()
setKeyCode ioctl = ioctlWrite ioctl 0x45 0x04 defaultCheckRet


-- | Get device name
--
-- EVIOCGNAME
getDeviceName :: IOCTL -> FileDescriptor -> SysRet String
getDeviceName ioctl = ioctlReadBuffer ioctl 0x45 0x06 defaultCheck (const peekCString) 256

-- | Get physical location
--
-- EVIOCGPHYS
getDevicePhysicalLocation :: IOCTL -> FileDescriptor -> SysRet String
getDevicePhysicalLocation ioctl = ioctlReadBuffer ioctl 0x45 0x07 defaultCheck (const peekCString) 256

-- | Get unique identifier
--
-- EVIOCGUNIQ
getDeviceUniqueID :: IOCTL -> FileDescriptor -> SysRet String
getDeviceUniqueID ioctl = ioctlReadBuffer ioctl 0x45 0x08 defaultCheck (const peekCString) 256

-- | Get device properties
--
-- EVIOCGPROP
getDeviceProperties :: IOCTL -> FileDescriptor -> SysRet String
getDeviceProperties ioctl = ioctlReadBuffer ioctl 0x45 0x09 defaultCheck (const peekCString) 256
