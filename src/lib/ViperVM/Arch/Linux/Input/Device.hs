{-# LANGUAGE DeriveGeneric #-}

-- | Linux Input management
module ViperVM.Arch.Linux.Input.Device
   ( DeviceInfo(..)
   , Property(..)
   , DeviceID(..)
   , DeviceBus(..)
   , protocolVersion
   , getDriverVersion
   , getDeviceInfo
   , getDeviceName
   , getDevicePhysicalLocation
   , getDeviceUniqueID
   , getDeviceProperties
   , getDeviceBits
   , grabDevice
   , releaseDevice
   , revokeDevice
   , setDeviceClock
   )
where

import Data.Word
import qualified Data.ByteString as BS
import Data.Int
import Foreign.Storable
import Foreign.CStorable
import Foreign.C.String (peekCString)
import GHC.Generics (Generic)

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.Time (Clock)

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


-- | Get bits that can be set by the given event type
--
-- EVIOCGBIT
getDeviceBits :: IOCTL -> Word8 -> Int -> FileDescriptor -> SysRet BS.ByteString
getDeviceBits ioctl code n fd = fmap snd <$> ioctlReadByteString ioctl 0x45 (0x20 + code) defaultCheck ((n `div` 8) + 1) fd

-- | Grab/release device
--
-- EVIOCGRAB
grabReleaseDevice :: IOCTL -> Bool -> FileDescriptor -> SysRet ()
grabReleaseDevice ioctl grab fd = ioctlWrite ioctl 0x45 0x90 defaultCheckRet fd value
   where
      value :: Int
      value = if grab then 1 else 0

-- | Grab device
grabDevice :: IOCTL -> FileDescriptor -> SysRet ()
grabDevice ioctl = grabReleaseDevice ioctl True

-- | Release device
releaseDevice :: IOCTL -> FileDescriptor -> SysRet ()
releaseDevice ioctl = grabReleaseDevice ioctl False

-- | Revoke device access
--
-- EVIOCREVOKE
revokeDevice :: IOCTL -> FileDescriptor -> SysRet ()
revokeDevice ioctl fd = ioctlWrite ioctl 0x45 0x91 defaultCheckRet fd value
   where
      value :: Int
      value = 0

-- | Set clock to use for timestamps
--
-- EVIOCCLOCKID
setDeviceClock :: IOCTL -> Clock -> FileDescriptor -> SysRet ()
setDeviceClock ioctl clk fd = ioctlWrite ioctl 0x45 0x91 defaultCheckRet fd (fromEnum clk)

-- | Device properties and quirks
data Property
   = PropertyNeedPointer      -- ^ needs a pointer
   | PropertyDirect           -- ^ direct input devices
   | PropertyButtonpad        -- ^ has button(s) under pad
   | PropertySemiMultiTouch   -- ^ touch rectangle only
   | PropertyTopButtonPad     -- ^ softbuttons at top of pad
   | PropertyPointingStick    -- ^ is a pointing stick
   deriving (Eq,Show,Enum)



data DeviceID
   = DeviceBusID
   | DeviceVendorID
   | DeviceProductID
   | DeviceVersionID
   deriving (Show,Eq,Enum)

data DeviceBus
   = BusPCI
   | BusISAPNP
   | BusUSB
   | BusHIL
   | BusBlueTooth
   | BusVirtual
   | BusISA
   | BusI8042
   | BusXTKBD
   | BusRS232
   | BusGamePort
   | BusParallelPort
   | BusAmiga
   | BusADB
   | BusI2C
   | BusHost
   | BusGSC
   | BusAtari
   | BusSPI
   deriving (Show,Eq)

instance Enum DeviceBus where
   fromEnum x = case x of
      BusPCI            -> 0x01
      BusISAPNP         -> 0x02
      BusUSB            -> 0x03
      BusHIL            -> 0x04
      BusBlueTooth      -> 0x05
      BusVirtual        -> 0x06
      BusISA            -> 0x10
      BusI8042          -> 0x11
      BusXTKBD          -> 0x12
      BusRS232          -> 0x13
      BusGamePort       -> 0x14
      BusParallelPort   -> 0x15
      BusAmiga          -> 0x16
      BusADB            -> 0x17
      BusI2C            -> 0x18
      BusHost           -> 0x19
      BusGSC            -> 0x1A
      BusAtari          -> 0x1B
      BusSPI            -> 0x1C
   toEnum x = case x of
      0x01 -> BusPCI
      0x02 -> BusISAPNP
      0x03 -> BusUSB
      0x04 -> BusHIL
      0x05 -> BusBlueTooth
      0x06 -> BusVirtual
      0x10 -> BusISA
      0x11 -> BusI8042
      0x12 -> BusXTKBD
      0x13 -> BusRS232
      0x14 -> BusGamePort
      0x15 -> BusParallelPort
      0x16 -> BusAmiga
      0x17 -> BusADB
      0x18 -> BusI2C
      0x19 -> BusHost
      0x1A -> BusGSC
      0x1B -> BusAtari
      0x1C -> BusSPI
      _    -> error "Unknown bus identifier"

