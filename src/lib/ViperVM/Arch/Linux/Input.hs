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
   , getDriverVersion
   , getDeviceInfo
   , getRepeatSettings
   )
where

import Control.Applicative ((<$>), (<*>))
import Data.Word
import Data.Int
import Foreign.Storable
import Foreign.Ptr

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Ioctl

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
   } deriving (Show,Eq)

instance Storable DeviceInfo where
   sizeOf _ = 8
   alignment _ = 8
   peek ptr = DeviceInfo
         <$> peekElemOff p 0
         <*> peekElemOff p 1
         <*> peekElemOff p 2
         <*> peekElemOff p 3
      where p = castPtr ptr :: Ptr Word16
   poke ptr di = do
      let p = castPtr ptr :: Ptr Word16
      pokeElemOff p 0 (infoBusType di)
      pokeElemOff p 1 (infoVendor di)
      pokeElemOff p 2 (infoProduct di)
      pokeElemOff p 3 (infoVersion di)


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
data KeymapEntry = KeymapEntry
   { keymapEntryFlags    :: Word8    -- ^ Indicate how kernel should handle the request
   , keymapEntryLength   :: Word8    -- ^ Length of the scancode (TODO: remove this in the Storable instance)
   , keymapEntryIndex    :: Word16   -- ^ Index in the keymap (may be used instead of the scancode)
   , keymapEntryKeyCode  :: Word32   -- ^ Key code assigned to this scancode
   , keymapEntryScanCode :: [Word8]  -- ^ Scan in machine-endian form (up to 32 bytes)
   } deriving (Show,Eq)

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
   deriving (Show,Eq)

instance Storable RepeatSettings where
   sizeOf _ = 8
   alignment _ = 8
   peek ptr = let p = castPtr ptr :: Ptr Int32 in
      RepeatSettings <$> peekElemOff p 0 
                     <*> peekElemOff p 1

   poke ptr (RepeatSettings x y) = do
      let p = castPtr ptr :: Ptr Int32
      pokeElemOff p 0 x
      pokeElemOff p 1 y
            

-- | Get repeat settings
--
-- EVIOCGREP
getRepeatSettings :: IOCTL -> FileDescriptor -> SysRet RepeatSettings
getRepeatSettings ioctl = ioctlRead ioctl 0x45 0x03 defaultCheck
