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
   , EventType(..)
   , SyncEvent(..)
   , RelativeEvent(..)
   , AbsoluteEvent(..)
   , SwitchEvent(..)
   , MiscEvent(..)
   , LED(..)
   , AutoRepeat(..)
   , Sound(..)
   , DeviceID(..)
   , DeviceBus(..)
   , MultiTouchToolType(..)
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
   , getDeviceMultiTouchSlots
   , getDeviceKeys
   , getDeviceLEDs
   , getDeviceSoundStatus
   , getDeviceSwitchStatus
   , getDeviceBits
   , getDeviceAbsInfo
   , setDeviceAbsInfo
   , ForceFeedbackDirection(..)
   , ForceFeedbackType(..)
   , ForceFeedbackPeriodicType(..)
   , ForceFeedbackDeviceProperties(..)
   , ForceFeedbackEffect(..)
   , ForceFeedbackReplay(..)
   , ForceFeedbackTrigger(..)
   , ForceFeedbackEnvelope(..)
   , ForceFeedbackConstantEffect(..)
   , ForceFeedbackRampEffect(..)
   , ForceFeedbackConditionEffect(..)
   , ForceFeedbackPeriodicEffect(..)
   , ForceFeedbackRumbleEffect(..)
   )
where

import Data.Word
import qualified Data.ByteString as BS
import Data.Int
import Foreign.Ptr (Ptr)
import Foreign.Storable
import Foreign.CStorable
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray)
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
   } deriving (Show, Eq, Generic)

instance CStorable AbsoluteInfo
instance Storable AbsoluteInfo where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

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

-- | Get keys (one bit per pressed key)
--
-- EVIOCGKEY
getDeviceKeys :: IOCTL -> Int -> FileDescriptor -> SysRet BS.ByteString
getDeviceKeys ioctl n fd = fmap snd <$> ioctlReadByteString ioctl 0x45 0x18 defaultCheck ((n `div` 8) + 1) fd

-- | Get leds (one bit per led)
--
-- EVIOCGLED
getDeviceLEDs :: IOCTL -> FileDescriptor -> SysRet BS.ByteString
getDeviceLEDs ioctl fd = fmap snd <$> ioctlReadByteString ioctl 0x45 0x19 defaultCheck ((n `div` 8) + 1) fd
   where
      n = fromEnum (maxBound :: LED) + 1

-- | Get sound status (one bit per sound)
--
-- EVIOCGSND
getDeviceSoundStatus :: IOCTL -> FileDescriptor -> SysRet BS.ByteString
getDeviceSoundStatus ioctl fd = fmap snd <$> ioctlReadByteString ioctl 0x45 0x1a defaultCheck ((n `div` 8) + 1) fd
   where
      n = fromEnum (maxBound :: Sound) + 1

-- | Get switch status (one bit per switch)
--
-- EVIOCGSW
getDeviceSwitchStatus :: IOCTL -> Int -> FileDescriptor -> SysRet BS.ByteString
getDeviceSwitchStatus ioctl n fd = fmap snd <$> ioctlReadByteString ioctl 0x45 0x1b defaultCheck ((n `div` 8) + 1) fd

-- | Get bits that can be set by the given event type
--
-- EVIOCGBIT
getDeviceBits :: IOCTL -> Word8 -> Int -> FileDescriptor -> SysRet BS.ByteString
getDeviceBits ioctl code n fd = fmap snd <$> ioctlReadByteString ioctl 0x45 (0x20 + code) defaultCheck ((n `div` 8) + 1) fd

-- | Get absolute info
--
-- EVIOCGABS
getDeviceAbsInfo :: IOCTL -> Word8 -> FileDescriptor -> SysRet AbsoluteInfo
getDeviceAbsInfo ioctl code = ioctlRead ioctl 0x45 (0x40 + code) defaultCheck

-- | Set absolute info
--
-- EVIOCSABS
setDeviceAbsInfo :: IOCTL -> Word8 -> AbsoluteInfo -> FileDescriptor -> SysRet ()
setDeviceAbsInfo ioctl code value fd = ioctlWrite ioctl 0x45 (0xc0 + code) defaultCheckRet fd value

data EventType
   = EventTypeSync
   | EventTypeKey
   | EventTypeRelative
   | EventTypeAbsolute
   | EventTypeMisc
   | EventTypeSwitch
   | EventTypeLED
   | EventTypeSound
   | EventTypeReplay
   | EventTypeForceFeedback
   | EventTypePower
   | EventTypeForceFeedbackStatus
   deriving (Show,Eq)

instance Enum EventType where
   fromEnum x = case x of
      EventTypeSync                 -> 0x00
      EventTypeKey                  -> 0x01
      EventTypeRelative             -> 0x02
      EventTypeAbsolute             -> 0x03
      EventTypeMisc                 -> 0x04
      EventTypeSwitch               -> 0x05
      EventTypeLED                  -> 0x11
      EventTypeSound                -> 0x12
      EventTypeReplay               -> 0x14
      EventTypeForceFeedback        -> 0x15
      EventTypePower                -> 0x16
      EventTypeForceFeedbackStatus  -> 0x17
   toEnum x = case x of
      0x00 -> EventTypeSync
      0x01 -> EventTypeKey
      0x02 -> EventTypeRelative
      0x03 -> EventTypeAbsolute
      0x04 -> EventTypeMisc
      0x05 -> EventTypeSwitch
      0x11 -> EventTypeLED
      0x12 -> EventTypeSound
      0x14 -> EventTypeReplay
      0x15 -> EventTypeForceFeedback
      0x16 -> EventTypePower
      0x17 -> EventTypeForceFeedbackStatus
      _    -> error "Unknown event type"

data SyncEvent
   = SyncReport
   | SyncConfig
   | SyncMultiTouchReport
   | SyncDropped
   deriving (Show,Eq,Enum)


data RelativeEvent
   = RelativeX
   | RelativeY
   | RelativeRX
   | RelativeRY
   | RelativeHorizontalWheel
   | RelativeDial
   | RelativeWheel
   | RelativeMisc
   deriving (Show,Eq,Enum,Bounded)


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


data SwitchEvent
   = SwitchLID                -- ^ set = LID shut
   | SwitchTabletMode         -- ^ set = tablet mode
   | SwitchHeadphoneInsert    -- ^ set = inserted
   | SwitchRFKillAll          -- ^ rfkill master switch, type "any" set = radio enabled
   | SwitchMicroPhoneInsert   -- ^ set = inserted
   | SwitchDock               -- ^ set = plugged into dock
   | SwitchLineOutInsert      -- ^ set = inserted
   | SwitchJackPhysicalInsert -- ^ set = mechanical switch set
   | SwitchVideoOutInsert     -- ^ set = inserted
   | SwitchCameraLensCover    -- ^ set = lens covered
   | SwitchKeypadSlide        -- ^ set = keypad slide out
   | SwitchFrontProximity     -- ^ set = front proximity sensor active
   | SwitchRotateLock         -- ^ set = rotate locked/disabled
   | SwitchLineInInsert       -- ^ set = inserted
   | SwitchMuteDevice         -- ^ set = device disabled
   deriving (Show,Eq,Enum)


data MiscEvent
   = MiscSerial
   | MiscPulseLED
   | MiscGesture
   | MiscRaw
   | MiscScan
   | MiscTimeStamp
   deriving (Eq,Show,Enum)

data LED
   = LEDNumLock
   | LEDCapsLock
   | LEDScrollLock
   | LEDCompose
   | LEDKana
   | LEDSleep
   | LEDSuspend
   | LEDMute
   | LEDMisc
   | LEDMail
   | LEDCharging
   deriving (Eq,Show,Enum,Bounded)

data AutoRepeat
   = RepeatDelay
   | RepeatPeriod
   deriving (Eq,Show,Enum)

data Sound
   = SoundClick
   | SoundBell
   | SoundTone
   deriving (Eq,Show,Bounded,Enum)

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

------------------------------------------------
-- Multi-touch
------------------------------------------------

-- | Get multi-touch slots
--
-- EVIOCGMTSLOTS
getDeviceMultiTouchSlots :: IOCTL -> Word32 -> Int -> FileDescriptor -> SysRet [Int32]
getDeviceMultiTouchSlots ioctl code nSlots fd = do
   let sz = 4 * (nSlots + 1)
   allocaBytes (fromIntegral sz) $ \ptr -> do
      pokeByteOff ptr 0 code
      ret <- ioctlReadBytes ioctl 0x45 0x0a defaultCheck (fromIntegral sz) ptr fd
      case ret of
         Left err -> return (Left err)
         Right _  -> Right <$> peekArray nSlots ptr


data MultiTouchToolType
   = MultiTouchFinder
   | MultiTouchPen
   deriving (Show,Eq,Enum)

------------------------------------------------
-- Force feedback
------------------------------------------------

data ForceFeedbackStatus
   = StatusStopped
   | StatusPlaying
   deriving (Show,Eq,Enum)

data ForceFeedbackDirection
   = FFDown 
   | FFLeft 
   | FFUp 
   | FFRight

fromDirection :: ForceFeedbackDirection -> Word16
fromDirection x = case x of
   FFDown   -> 0x0000
   FFLeft   -> 0x4000
   FFUp     -> 0x8000
   FFRight  -> 0xC000

data ForceFeedbackType
   = Rumble
   | Periodic
   | Constant
   | Spring
   | Friction
   | Damper
   | Inertia
   | Ramp
   deriving (Show,Eq)

instance Enum ForceFeedbackType where
   fromEnum x = case x of
      Rumble    -> 0x50
      Periodic  -> 0x51
      Constant  -> 0x52
      Spring    -> 0x53
      Friction  -> 0x54
      Damper    -> 0x55
      Inertia   -> 0x56
      Ramp      -> 0x57
   toEnum x = case x of
      0x50 -> Rumble
      0x51 -> Periodic
      0x52 -> Constant
      0x53 -> Spring
      0x54 -> Friction
      0x55 -> Damper
      0x56 -> Inertia
      0x57 -> Ramp
      _    -> error "Unknown force feedback type"

data ForceFeedbackPeriodicType
   = Square
   | Triangle
   | Sine
   | SawUp
   | SawDown
   | Custom
   deriving (Show,Eq)

instance Enum ForceFeedbackPeriodicType where
   fromEnum x = case x of
      Square   -> 0x58
      Triangle -> 0x59
      Sine     -> 0x5a
      SawUp    -> 0x5b
      SawDown  -> 0x5c
      Custom   -> 0x5d
   toEnum x = case x of
      0x58 -> Square
      0x59 -> Triangle
      0x5a -> Sine
      0x5b -> SawUp
      0x5c -> SawDown
      0x5d -> Custom
      _    -> error "Unknown force feedback periodic type"


data ForceFeedbackDeviceProperties
   = FFGain
   | FFAutoCenter

instance Enum ForceFeedbackDeviceProperties where
   fromEnum x = case x of
      FFGain       -> 0x60
      FFAutoCenter -> 0x61
   toEnum x = case x of
      0x60 -> FFGain
      0x61 -> FFAutoCenter
      _    -> error "Unknown force feedback device property"

data ForceFeedbackEffect a = ForceFeedbackEffect
   { ffType              :: Word16
   , ffID                :: Int16
   , ffDirection         :: ForceFeedbackDirection
   , ffTrigger           :: ForceFeedbackTrigger
   , ffReplay            :: ForceFeedbackReplay
   , ffEffectProperties  :: a
   }

-- | Defines scheduling of the force-feedback effect
-- @length: duration of the effect
-- @delay: delay before effect should start playing
data ForceFeedbackReplay = ForceFeedbackReplay
   { ffReplayLength :: Word16
   , ffReplayDelay  :: Word16
   }


-- | Defines what triggers the force-feedback effect
-- @button: number of the button triggering the effect
-- @interval: controls how soon the effect can be re-triggered
data ForceFeedbackTrigger = ForceFeedbackTrigger
   { ffTriggerButton   :: Word16
   , ffTriggerInterval :: Word16
   }


-- | Generic force-feedback effect envelope
-- @attack_length: duration of the attack (ms)
-- @attack_level: level at the beginning of the attack
-- @fade_length: duration of fade (ms)
-- @fade_level: level at the end of fade
--
-- The @attack_level and @fade_level are absolute values; when applying
-- envelope force-feedback core will convert to positive/negative
-- value based on polarity of the default level of the effect.
-- Valid range for the attack and fade levels is 0x0000 - 0x7fff
data ForceFeedbackEnvelope = ForceFeedbackEnvelope
   { ffEnvelopeAttackLength :: Word16
   , ffEnvelopeAttackLevel  :: Word16
   , ffEnvelopeFadeLength   :: Word16
   , ffEnvelopeFadeLevel    :: Word16
   }


-- | Defines parameters of a constant force-feedback effect
-- @level: strength of the effect; may be negative
-- @envelope: envelope data
data ForceFeedbackConstantEffect = ForceFeedbackConstantEffect
   { ffConstantEffectLevel    :: Int16
   , ffConstantEffectEnvelope :: ForceFeedbackEnvelope
   }


-- | Defines parameters of a ramp force-feedback effect
-- @start_level: beginning strength of the effect; may be negative
-- @end_level: final strength of the effect; may be negative
-- @envelope: envelope data
data ForceFeedbackRampEffect = ForceFeedbackRampEffect
   { ffRampEffectStartLevel :: Int16
   , ffRampEffectEndLevel   :: Int16
   , ffRampEffectEnvelope   :: ForceFeedbackEnvelope
   }


-- | Defines a spring or friction force-feedback effect
-- @right_saturation: maximum level when joystick moved all way to the right
-- @left_saturation: same for the left side
-- @right_coeff: controls how fast the force grows when the joystick moves
--	to the right
-- @left_coeff: same for the left side
-- @deadband: size of the dead zone, where no force is produced
-- @center: position of the dead zone
data ForceFeedbackConditionEffect = ForceFeedbackConditionEffect
   { ffConditionEffectRightSaturation :: Word16
   , ffConditionEffectLeftSaturation  :: Word16
   , ffConditionEffectRightCoeff      :: Int16
   , ffConditionEffectLeftCoeff       :: Int16
   , ffConditionEffectDeadBand        :: Word16
   , ffConditionEffectCenter          :: Int16
   }


-- | Defines parameters of a periodic force-feedback effect
-- @waveform: kind of the effect (wave)
-- @period: period of the wave (ms)
-- @magnitude: peak value
-- @offset: mean value of the wave (roughly)
-- @phase: 'horizontal' shift
-- @envelope: envelope data
-- @custom_len: number of samples (FF_CUSTOM only)
-- @custom_data: buffer of samples (FF_CUSTOM only)
--
-- Known waveforms - FF_SQUARE, FF_TRIANGLE, FF_SINE, FF_SAW_UP,
-- FF_SAW_DOWN, FF_CUSTOM. The exact syntax FF_CUSTOM is undefined
-- for the time being as no driver supports it yet.
--
-- Note: the data pointed by custom_data is copied by the driver.
-- You can therefore dispose of the memory after the upload/update.
data ForceFeedbackPeriodicEffect = ForceFeedbackPeriodicEffect
   { ffPeriodicEffectWaveform   :: Word16
   , ffPeriodicEffectPeriod     :: Word16
   , ffPeriodicEffectMagnitude  :: Int16
   , ffPeriodicEffectOffset     :: Int16
   , ffPeriodicEffectPhase      :: Word16
   , ffPeriodicEffectEnvelope   :: ForceFeedbackEnvelope
   , ffPeriodicEffectCustomLen  :: Word32
   , ffPeriodicEffectCustomData :: Ptr Int16
   }


-- | Defines parameters of a periodic force-feedback effect
-- @strong_magnitude: magnitude of the heavy motor
-- @weak_magnitude: magnitude of the light one
--
-- Some rumble pads have two motors of different weight. Strong_magnitude
-- represents the magnitude of the vibration generated by the heavy one.
data ForceFeedbackRumbleEffect = ForceFeedbackRumbleEffect
   { ffRumbleEffectStrongMagnitude :: Word16
   , ffRumbleEffectWeakMagnitude   :: Word16
   }
