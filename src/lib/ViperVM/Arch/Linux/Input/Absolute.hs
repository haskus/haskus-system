{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ViperVM.Arch.Linux.Input.Absolute
   ( AbsoluteInfo(..)
   , AbsoluteEvent(..)
   , getDeviceAbsInfo
   , setDeviceAbsInfo
   )
where

import Data.Word
import Data.Int
import Foreign.Storable
import Foreign.CStorable
import GHC.Generics (Generic)

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Ioctl

-- | Absolute info
--
-- `struct input_absinfo` in C header file
data AbsoluteInfo = AbsoluteInfo
   { absValue      :: Int32   -- ^ Latest reported value for the axis
   , absMinimum    :: Int32   -- ^ Minimum value for the axis
   , absMaximum    :: Int32   -- ^ Maximum value for the axis
   , absFuzz       :: Int32   -- ^ Fuzz value used to filter noise from the event stream
   , absFlat       :: Int32   -- ^ Values that are within this value will be discarded and reported as 0 instead
   , absResolution :: Int32   -- ^ Resolution for the values reported for the axis
   } deriving (Show, Eq, Generic, CStorable)

instance Storable AbsoluteInfo where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- | Get absolute info
--
-- EVIOCGABS
getDeviceAbsInfo :: IOCTL -> Word8 -> FileDescriptor -> SysRet AbsoluteInfo
getDeviceAbsInfo ioctl code = ioctlRead ioctl 0x45 (0x40 + code) defaultCheck

-- | Set absolute info
--
-- EVIOCSABS
setDeviceAbsInfo :: IOCTL -> Word8 -> AbsoluteInfo -> FileDescriptor -> SysRet ()
setDeviceAbsInfo ioctl code value fd = ioctlWrite ioctl 0x45 (0xc0 + code) defaultCheck fd value


data AbsoluteEvent
   = AbsoluteX
   | AbsoluteY
   | AbsoluteZ
   | AbsoluteRX
   | AbsoluteRY
   | AbsoluteRZ
   | AbsoluteThrottle
   | AbsoluteRudder
   | AbsoluteWheel
   | AbsoluteGas
   | AbsoluteBrake
   | AbsoluteHat0X
   | AbsoluteHat0Y
   | AbsoluteHat1X
   | AbsoluteHat1Y
   | AbsoluteHat2X
   | AbsoluteHat2Y
   | AbsoluteHat3X
   | AbsoluteHat3Y
   | AbsolutePressure
   | AbsoluteDistance
   | AbsoluteTiltX
   | AbsoluteTiltY
   | AbsoluteToolWidth
   | AbsoluteVolume
   | AbsoluteMisc
   | AbsoluteMultiTouchSlot
   | AbsoluteMultiTouchTouchMajor
   | AbsoluteMultiTouchTouchMinor
   | AbsoluteMultiTouchWidthMajor
   | AbsoluteMultiTouchWidthMinor
   | AbsoluteMultiTouchOrientation
   | AbsoluteMultiTouchPositionX
   | AbsoluteMultiTouchPositionY
   | AbsoluteMultiTouchToolType
   | AbsoluteMultiTouchBlobID
   | AbsoluteMultiTouchTrackingID
   | AbsoluteMultiTouchPressure
   | AbsoluteMultiTouchDistance
   | AbsoluteMultiTouchToolX
   | AbsoluteMultiTouchToolY
   deriving (Show,Eq)

instance Enum AbsoluteEvent where
   fromEnum x = case x of
      AbsoluteX                        -> 0x00
      AbsoluteY                        -> 0x01
      AbsoluteZ                        -> 0x02
      AbsoluteRX                       -> 0x03
      AbsoluteRY                       -> 0x04
      AbsoluteRZ                       -> 0x05
      AbsoluteThrottle                 -> 0x06
      AbsoluteRudder                   -> 0x07
      AbsoluteWheel                    -> 0x08
      AbsoluteGas                      -> 0x09
      AbsoluteBrake                    -> 0x0a
      AbsoluteHat0X                    -> 0x10
      AbsoluteHat0Y                    -> 0x11
      AbsoluteHat1X                    -> 0x12
      AbsoluteHat1Y                    -> 0x13
      AbsoluteHat2X                    -> 0x14
      AbsoluteHat2Y                    -> 0x15
      AbsoluteHat3X                    -> 0x16
      AbsoluteHat3Y                    -> 0x17
      AbsolutePressure                 -> 0x18
      AbsoluteDistance                 -> 0x19
      AbsoluteTiltX                    -> 0x1a
      AbsoluteTiltY                    -> 0x1b
      AbsoluteToolWidth                -> 0x1c
      AbsoluteVolume                   -> 0x20
      AbsoluteMisc                     -> 0x28
      AbsoluteMultiTouchSlot           -> 0x2f
      AbsoluteMultiTouchTouchMajor     -> 0x30
      AbsoluteMultiTouchTouchMinor     -> 0x31
      AbsoluteMultiTouchWidthMajor     -> 0x32
      AbsoluteMultiTouchWidthMinor     -> 0x33
      AbsoluteMultiTouchOrientation    -> 0x34
      AbsoluteMultiTouchPositionX      -> 0x35
      AbsoluteMultiTouchPositionY      -> 0x36
      AbsoluteMultiTouchToolType       -> 0x37
      AbsoluteMultiTouchBlobID         -> 0x38
      AbsoluteMultiTouchTrackingID     -> 0x39
      AbsoluteMultiTouchPressure       -> 0x3a
      AbsoluteMultiTouchDistance       -> 0x3b
      AbsoluteMultiTouchToolX          -> 0x3c
      AbsoluteMultiTouchToolY          -> 0x3d
   toEnum x = case x of
      0x00 -> AbsoluteX
      0x01 -> AbsoluteY
      0x02 -> AbsoluteZ
      0x03 -> AbsoluteRX
      0x04 -> AbsoluteRY
      0x05 -> AbsoluteRZ
      0x06 -> AbsoluteThrottle
      0x07 -> AbsoluteRudder
      0x08 -> AbsoluteWheel
      0x09 -> AbsoluteGas
      0x0a -> AbsoluteBrake
      0x10 -> AbsoluteHat0X
      0x11 -> AbsoluteHat0Y
      0x12 -> AbsoluteHat1X
      0x13 -> AbsoluteHat1Y
      0x14 -> AbsoluteHat2X
      0x15 -> AbsoluteHat2Y
      0x16 -> AbsoluteHat3X
      0x17 -> AbsoluteHat3Y
      0x18 -> AbsolutePressure
      0x19 -> AbsoluteDistance
      0x1a -> AbsoluteTiltX
      0x1b -> AbsoluteTiltY
      0x1c -> AbsoluteToolWidth
      0x20 -> AbsoluteVolume
      0x28 -> AbsoluteMisc
      0x2f -> AbsoluteMultiTouchSlot
      0x30 -> AbsoluteMultiTouchTouchMajor
      0x31 -> AbsoluteMultiTouchTouchMinor
      0x32 -> AbsoluteMultiTouchWidthMajor
      0x33 -> AbsoluteMultiTouchWidthMinor
      0x34 -> AbsoluteMultiTouchOrientation
      0x35 -> AbsoluteMultiTouchPositionX
      0x36 -> AbsoluteMultiTouchPositionY
      0x37 -> AbsoluteMultiTouchToolType
      0x38 -> AbsoluteMultiTouchBlobID
      0x39 -> AbsoluteMultiTouchTrackingID
      0x3a -> AbsoluteMultiTouchPressure
      0x3b -> AbsoluteMultiTouchDistance
      0x3c -> AbsoluteMultiTouchToolX
      0x3d -> AbsoluteMultiTouchToolY
      _    -> error "Unknown absolute axe"
