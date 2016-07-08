{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module ViperVM.Arch.Linux.Internals.Input
   ( Property (..)
   , EventType (..)
   , SyncEvent (..)
   , Key (..)
   , RelativeAxe (..)
   , AbsoluteAxe (..)
   , SwitchEvent (..)
   , MiscEvent (..)
   , LED (..)
   , AutoRepeat (..)
   , Sound (..)
   , Event (..)
   , protocolVersion
   , DeviceInfo (..)
   , AbsoluteInfo (..)
   , KeymapEntry (..)
   , KeymapFlag (..)
   , Mask (..)
   , getVersion
   , getDeviceInfo
   , RepeatSettings (..)
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
   , getDeviceAbsoluteInfo
   , setDeviceAbsoluteInfo
   , sendForceFeedback
   , removeForceFeedback
   , supportedSimultaneousEffects
   , grabDevice
   , releaseDevice
   , revokeDevice
   , getMask
   , setMask
   , setDeviceClock
   , DeviceID (..)
   , BusType (..)
   , MultiTouchToolType (..)
   , ForceFeedbackStatus (..)
   , ForceFeedbackReplay (..)
   , ForceFeedbackTrigger (..)
   , ForceFeedbackEnvelope (..)
   , ForceFeedbackConstantEffect (..)
   , ForceFeedbackRampEffect (..)
   , ForceFeedbackConditionEffect (..)
   , ForceFeedbackPeriodicEffect (..)
   , ForceFeedbackRumbleEffect (..)
   , ForceFeedbackEffect (..)
   , ForceFeedbackEffectType (..)
   , ForceFeedbackPeriodicEffectType (..)
   , ForceFeedbackDeviceProperties (..)
   , ForceFeedbackDirection (..)
   )
where

import ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Format.Binary.Enum
import ViperVM.Format.Binary.Union
import ViperVM.Format.Binary.Vector (Vector)
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Bits
import ViperVM.Format.String (peekCString)
import ViperVM.Arch.Linux.Time (TimeVal,Clock)
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Ioctl
import ViperVM.Utils.Flow

import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import Foreign.Storable
import Foreign.CStorable
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils (fromBool)


-- =============================================================
--    From linux/include/uapi/linux/input-event-codes.h
-- =============================================================

-- | Device properties and quirks
data Property
   = PropertyNeedPointer      -- ^ needs a pointer
   | PropertyDirect           -- ^ direct input devices
   | PropertyButtonpad        -- ^ has button(s) under pad
   | PropertySemiMultiTouch   -- ^ touch rectangle only
   | PropertyTopButtonPad     -- ^ softbuttons at top of pad
   | PropertyPointingStick    -- ^ is a pointing stick
   | PropertyAccelerometer    -- ^ has ccelerometer
   deriving (Eq,Show,Enum,CEnum)

-- | Event types
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
   deriving (Show,Eq,Enum)

instance CEnum EventType where
   fromCEnum x = fromIntegral $ case fromEnum x of
      y  | y <= 5    -> y
         | y <= 7    -> y + 11
         | otherwise -> y + 12
   toCEnum x = toEnum $ fromIntegral $ case x of
      _ | x >= 20   -> x - 12
        | x >= 17   -> x - 11
        | otherwise -> x

-- | Synchronization events
data SyncEvent
   = SyncReport
   | SyncConfig
   | SyncMultiTouchReport
   | SyncDropped
   deriving (Show,Eq,Enum,CEnum)


data Key
   = KeyReserved
   | KeyEsc
   | Key1
   | Key2
   | Key3
   | Key4
   | Key5
   | Key6
   | Key7
   | Key8
   | Key9
   | Key0
   | KeyMinus
   | KeyEqual
   | KeyBackSpace
   | KeyTab
   | KeyQ
   | KeyW
   | KeyE
   | KeyR
   | KeyT
   | KeyY
   | KeyU
   | KeyI
   | KeyO
   | KeyP
   | KeyLeftBrace
   | KeyRightBrace
   | KeyEnter
   | KeyLeftCtrl
   | KeyA
   | KeyS
   | KeyD
   | KeyF
   | KeyG
   | KeyH
   | KeyJ
   | KeyK
   | KeyL
   | KeySemiColon
   | KeyApostrophe
   | KeyGrave
   | KeyLeftShift
   | KeyBackSlash
   | KeyZ
   | KeyX
   | KeyC
   | KeyV
   | KeyB
   | KeyN
   | KeyM
   | KeyComma
   | KeyDot
   | KeySlash
   | KeyRightShift
   | KeyKeyPadAsterisk
   | KeyLeftAlt
   | KeySpace
   | KeyCapsLock
   | KeyF1
   | KeyF2
   | KeyF3
   | KeyF4
   | KeyF5
   | KeyF6
   | KeyF7
   | KeyF8
   | KeyF9
   | KeyF10
   | KeyNumLock
   | KeySCrollLock
   | KeyKeyPad7
   | KeyKeyPad8
   | KeyKeyPad9
   | KeyKeyPadMinus
   | KeyKeyPad4
   | KeyKeyPad5
   | KeyKeyPad6
   | KeyKeyPadPlus
   | KeyKeyPad1
   | KeyKeyPad2
   | KeyKeyPad3
   | KeyKeyPad0
   | KeyKeyPadDot
   | Key84
   | KeyZenkakuhankaku
   | Key102ND
   | KeyF11
   | KeyF12
   | KeyRO
   | KeyKatakana
   | KeyHiragana
   | KeyHenkan
   | KeyKatakanaHiragana
   | KeyMuhenkan
   | KeyKeyPadJPComma
   | KeyKeyPadEnter
   | KeyRightCtrl
   | KeyKeyPadSlash
   | KeySysRq
   | KeyRightAlt
   | KeyLineFeed
   | KeyHome
   | KeyUp
   | KeyPageUp
   | KeyLeft
   | KeyRIGHT
   | KeyEnd
   | KeyDown
   | KeyPageDown
   | KeyInsert
   | KeyDelete
   | KeyMacro
   | KeyMute
   | KeyVolumeDown
   | KeyVolumeUp
   | KeyPower            -- ^ System Power Down
   | KeyKeyPadEqual
   | KeyKeyPadPlusMinus
   | KeyPause
   | KeyScale            -- ^ Compiz Scale (Expose)
   | KeyKeyPadComma
   | KeyHangeul
   | KeyHanja
   | KeyYen
   | KeyLeftMeta
   | KeyRightMeta
   | KeyCompose
   | KeyStop
   | KeyAgain
   | KeyProperties
   | KeyUndo
   | KeyFront
   | KeyCopy
   | KeyOpen
   | KeyPaste
   | KeyFind
   | KeyCut
   | KeyHelp
   | KeyMenu
   | KeyCalc
   | KeySetup
   | KeySleep
   | KeyWakeUp
   | KeyFile
   | KeySendFile
   | KeyDeleteFile
   | KeyTransfer
   | KeyProg1
   | KeyProg2
   | KeyWeb
   | KeyMsDos
   | KeyScreenLock
   | KeyRotateDisplay    -- ^ Display orientation for e.g. tablets
   | KeyCycleWindows
   | KeyMail
   | KeyBookmarks
   | KeyComputer
   | KeyBack
   | KeyForward
   | KeyCloseCD
   | KeyEjectCD
   | KeyEjectCloseCD
   | KeyNextSong
   | KeyPlayPause
   | KeyPreviousSong
   | KeyStopCD
   | KeyRecord
   | KeyRewind
   | KeyPhone            -- ^ Media Select Telephone
   | KeyISO
   | KeyConfig           -- ^ Consumer Control Configuration
   | KeyHomePage
   | KeyRefresh
   | KeyExit
   | KeyMove
   | KeyEdit
   | KeyScrollUp
   | KeyScrollDown
   | KeyKeyPadLeftParen
   | KeyKeypadRightParen
   | KeyNew
   | KeyRedo
   | KeyF13
   | KeyF14
   | KeyF15
   | KeyF16
   | KeyF17
   | KeyF18
   | KeyF19
   | KeyF20
   | KeyF21
   | KeyF22
   | KeyF23
   | KeyF24
   | Key195
   | Key196
   | Key197
   | Key198
   | Key199
   | KeyPlayCD
   | KeyPauseCD
   | KeyProg3
   | KeyProg4
   | KeyDashBoard
   | KeySuspend
   | KeyClose
   | KeyPlay
   | KeyFastForward
   | KeyBassBoost
   | KeyPrint
   | KeyHP
   | KeyCamera
   | KeySound
   | KeyQuestion
   | KeyEmail
   | KeyChat
   | KeySearch
   | KeyConnect
   | KeyFinance
   | KeySport
   | KeyShop
   | KeyAltErase
   | KeyCancel
   | KeyBrightnessDown
   | KeyBrightnessUp
   | KeyMedia
   | KeySwitchVideoMode  -- ^ Cycle between available video outputs (Monitor/LCD/TV-out/etc)
   | KeyKbdIllumToggle
   | KeyKbdIllumDown
   | KeyKbdIllumUp
   | KeySend
   | KeyReply
   | KeyForwardMail
   | KeySave
   | KeyDocuments
   | KeyBattery
   | KeyBlueTooth
   | KeyWLAN
   | KeyUWB
   | KeyUnknown
   | KeyVideoNext        -- ^ drive next video source
   | KeyVideoPrev        -- ^ drive previous video source
   | KeyBrightnessCycle  -- ^ brightness up, after max is min
   | KeyBrightnessAuto   -- ^ Set Auto Brightness: manual brightness control is off, rely on ambient
   | KeyDisplayOff       -- ^ display device to off state
   | KeyWWAN             -- ^ Wireless WAN (LTE, UMTS, GSM, etc.)
   | KeyRfKill           -- ^ Key that controls all radios
   | KeyMicMute          -- ^ Mute / unmute the microphone
   | Key249
   | Key250
   | Key251
   | Key252
   | Key253
   | Key254
   | Key255
   deriving (Show,Eq,Enum,CEnum)


-- I haven't made the enum for the BTN_* defines. It seems a bit random...


-- | Relative axes
data RelativeAxe
   = RelativeX
   | RelativeY
   | RelativeRX
   | RelativeRY
   | RelativeHorizontalWheel
   | RelativeDial
   | RelativeWheel
   | RelativeMisc
   deriving (Show,Eq,Enum,CEnum)

-- | Absolute axes
data AbsoluteAxe
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
   | AbsoluteMultiTouchSlot        -- ^ MT slot being modified
   | AbsoluteMultiTouchTouchMajor  -- ^ Major axis of touching ellipse
   | AbsoluteMultiTouchTouchMinor  -- ^ Minor axis (omit if circular)
   | AbsoluteMultiTouchWidthMajor  -- ^ Major axis of approaching ellipse
   | AbsoluteMultiTouchWidthMinor  -- ^ Minor axis (omit if circular)
   | AbsoluteMultiTouchOrientation -- ^ Ellipse orientation
   | AbsoluteMultiTouchPositionX   -- ^ Center X touch position
   | AbsoluteMultiTouchPositionY   -- ^ Center Y touch position
   | AbsoluteMultiTouchToolType    -- ^ Type of touching device
   | AbsoluteMultiTouchBlobID      -- ^ Group a set of packets as a blob
   | AbsoluteMultiTouchTrackingID  -- ^ Unique ID of initiated contact
   | AbsoluteMultiTouchPressure    -- ^ Pressure on contact area
   | AbsoluteMultiTouchDistance    -- ^ Contact hover distance
   | AbsoluteMultiTouchToolX       -- ^ Center X tool position
   | AbsoluteMultiTouchToolY       -- ^ Center Y tool position
   deriving (Show,Eq,Enum)

instance CEnum AbsoluteAxe where
   fromCEnum x = fromIntegral $ case x of
      AbsoluteVolume -> 0x20
      AbsoluteMisc   -> 0x28
      y | fromEnum y <= fromEnum AbsoluteToolWidth -> fromEnum y
        | otherwise  -> 0x2f + fromEnum y - fromEnum AbsoluteMultiTouchSlot

   toCEnum x = case fromIntegral x of
      0x20 -> AbsoluteVolume
      0x28 -> AbsoluteMisc
      y | y <= fromEnum AbsoluteToolWidth -> toEnum y
        | y >= 0x2f -> toEnum (y - 0x2f + fromEnum AbsoluteMultiTouchSlot)
        | otherwise -> error ("Unknown absolute axe "++show y)

-- | Switch events
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
   deriving (Show,Eq,Enum,CEnum)

-- | Misc events
data MiscEvent
   = MiscSerial
   | MiscPulseLED
   | MiscGesture
   | MiscRaw
   | MiscScan
   | MiscTimeStamp
   deriving (Eq,Show,Enum,CEnum)

-- | LEDs
data LED
   = LedNumLock
   | LedCapsLock
   | LedScrollLock
   | LedCompose
   | LedKana
   | LedSleep
   | LedSuspend
   | LedMute
   | LedMisc
   | LedMail
   | LedCharging
   deriving (Eq,Show,Enum,CEnum)

-- | Autorepeat values
data AutoRepeat
   = RepeatDelay
   | RepeatPeriod
   deriving (Eq,Show,Enum,CEnum)

-- | Sounds
data Sound
   = SoundClick
   | SoundBell
   | SoundTone
   deriving (Eq,Show,Enum,CEnum)

-- =============================================================
--    From linux/include/uapi/linux/input.h
-- =============================================================

-- | Input event
data Event = Event
   { eventTime  :: TimeVal
   , eventType  :: EnumField Word16 EventType
   , eventCode  :: Word16
   , eventValue :: Int32
   } deriving (Show,Eq,Generic,CStorable)

instance Storable Event where
   alignment = cAlignment
   sizeOf    = cSizeOf
   peek      = cPeek
   poke      = cPoke


-- | Protocol version
protocolVersion :: Word
protocolVersion = 0x010001

-- IOCTLs (0x00 - 0x7f)

-- | Device info
--
-- `struct input_id`
data DeviceInfo = DeviceInfo
   { infoBusType :: EnumField Word16 BusType
   , infoVendor  :: Word16
   , infoProduct :: Word16
   , infoVersion :: Word16
   } deriving (Show,Eq,Generic,CStorable)

instance Storable DeviceInfo where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke



-- | Absolute info
--
-- struct input_absinfo - used by EVIOCGABS/EVIOCSABS ioctls
--
-- @value: latest reported value for the axis.
-- @minimum: specifies minimum value for the axis.
-- @maximum: specifies maximum value for the axis.
-- @fuzz: specifies fuzz value that is used to filter noise from the event
-- stream.
-- @flat: values that are within this value will be discarded by joydev
-- interface and reported as 0 instead.
-- @resolution: specifies resolution for the values reported for the axis.
-- 
-- Note that input core does not clamp reported values to the [minimum, maximum]
-- limits, such task is left to userspace.
-- 
-- Resolution for main axes (ABS_X, ABS_Y, ABS_Z) is reported in units per
-- millimeter (units/mm), resolution for rotational axes (ABS_RX, ABS_RY,
-- ABS_RZ) is reported in units per radian.
--
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

-- | Query or modify keymap data
--
-- struct input_keymap_entry - used by EVIOCGKEYCODE/EVIOCSKEYCODE ioctls
--
-- @scancode: scancode represented in machine-endian form.
-- @len: length of the scancode that resides in @scancode buffer.
-- @index: index in the keymap, may be used instead of scancode
-- @flags: allows to specify how kernel should handle the request. For example,
-- setting INPUT_KEYMAP_BY_INDEX flag indicates that kernel should perform
-- lookup in keymap by @index instead of @scancode
-- @keycode: key code assigned to this scancode
-- 
-- The structure is used to retrieve and modify keymap data. Users have option
-- of performing lookup either by @scancode itself or by @index in keymap entry.
-- EVIOCGKEYCODE will also return scancode or index (depending on which element
-- was used to perform lookup).
data KeymapEntry = KeymapEntry
   { keymapEntryFlags    :: BitSet Word8 KeymapFlag -- ^ Indicate how kernel should handle the request
   , keymapEntryLength   :: Word8                   -- ^ Length of the scancode
   , keymapEntryIndex    :: Word16                  -- ^ Index in the keymap (may be used instead of the scancode)
   , keymapEntryKeyCode  :: Word32                  -- ^ Key code assigned to this scancode
   , keymapEntryScanCode :: Vector 32 Word8         -- ^ Scan in machine-endian form (up to 32 bytes)
   } deriving (Show,Generic,CStorable)


instance Storable KeymapEntry where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

data KeymapFlag
   = KeymapByIndex
   deriving (Eq,Show,Enum,CBitSet)


data Mask = Mask
   { maskType      :: Word32
   , maskCodesSize :: Word32
   , maskCodesPtr  :: Word64
   }
   deriving (Show,Eq,Generic,CStorable)

instance Storable Mask where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- | Get version
--
-- EVIOCGVERSION
getVersion :: Handle -> SysRet Int
getVersion = ioctlRead 0x45 0x01

-- | Get device info
--
-- EVIOCGID
getDeviceInfo :: Handle -> SysRet DeviceInfo
getDeviceInfo = ioctlRead 0x45 0x02

-- | Repeat settings
--
-- We use a structure instead of Vector 2 Word
data RepeatSettings = RepeatSettings
   { repeatDelay  :: Word
   , repeatPeriod :: Word
   }
   deriving (Show,Eq,Generic,CStorable)

instance Storable RepeatSettings where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke


-- | Get repeat settings
--
-- EVIOCGREP
getRepeatSettings :: Handle -> SysRet RepeatSettings
getRepeatSettings = ioctlRead 0x45 0x03

-- | Set repeat settings
--
-- EVIOCSREP
setRepeatSettings :: RepeatSettings -> Handle -> SysRet ()
setRepeatSettings = ioctlWrite 0x45 0x03


-- | Get key code
--
-- EVIOCGKEYCODE_V2
getKeyCode :: Handle -> SysRet KeymapEntry
getKeyCode = ioctlRead 0x45 0x04

-- | Set key code
--
-- EVIOCSKEYCODE_V2
setKeyCode :: KeymapEntry -> Handle -> SysRet ()
setKeyCode = ioctlWrite 0x45 0x04

-- | Get device name
--
-- EVIOCGNAME
getDeviceName :: Handle -> SysRet String
getDeviceName = ioctlReadBuffer 0x45 0x06 (const peekCString) 256

-- | Get physical location
--
-- EVIOCGPHYS
getDevicePhysicalLocation :: Handle -> SysRet String
getDevicePhysicalLocation = ioctlReadBuffer 0x45 0x07 (const peekCString) 256

-- | Get unique identifier
--
-- EVIOCGUNIQ
getDeviceUniqueID :: Handle -> SysRet String
getDeviceUniqueID = ioctlReadBuffer 0x45 0x08 (const peekCString) 256

-- | Get device properties
--
-- EVIOCGPROP
getDeviceProperties :: Handle -> SysRet String
getDeviceProperties = ioctlReadBuffer 0x45 0x09 (const peekCString) 256

-- | Get multi-touch slots
--
-- EVIOCGMTSLOTS(len) - get MT slot values
-- @len: size of the data buffer in bytes
-- 
-- The ioctl buffer argument should be binary equivalent to
-- 
-- struct input_mt_request_layout {
-- __u32 code;
-- __s32 values[num_slots];
-- };
-- 
-- where num_slots is the (arbitrary) number of MT slots to extract.
-- 
-- The ioctl size argument (len) is the size of the buffer, which
-- should satisfy len = (num_slots + 1) * sizeof(__s32).  If len is
-- too small to fit all available slots, the first num_slots are
-- returned.
-- 
-- Before the call, code is set to the wanted ABS_MT event type. On
-- return, values[] is filled with the slot values for the specified
-- ABS_MT code.
-- 
-- If the request code is not an ABS_MT value, -EINVAL is returned.
getDeviceMultiTouchSlots :: Word32 -> Int -> Handle -> SysRet [Int32]
getDeviceMultiTouchSlots code nSlots fd = do
   let sz = 4 * (nSlots + 1)
   allocaBytes (fromIntegral sz) $ \ptr -> do
      pokeByteOff ptr 0 code
      ioctlReadBytes 0x45 0x0a (fromIntegral sz) ptr fd
         >.~.> const (peekArray nSlots (ptr `plusPtr` 4))

-- | Get global key state (one bit per pressed key)
--
-- EVIOCGKEY
getDeviceKeys :: Int -> Handle -> SysRet BS.ByteString
getDeviceKeys n fd = ioctlReadByteString 0x45 0x18 ((n `div` 8) + 1) fd >.-.> snd

-- | Get all leds (one bit per led)
--
-- EVIOCGLED
getDeviceLEDs :: Int -> Handle -> SysRet BS.ByteString
getDeviceLEDs n fd = ioctlReadByteString 0x45 0x19 ((n `div` 8) + 1) fd >.-.> snd

-- | Get sound status (one bit per sound)
--
-- EVIOCGSND
getDeviceSoundStatus :: Int -> Handle -> SysRet BS.ByteString
getDeviceSoundStatus n fd = ioctlReadByteString 0x45 0x1a ((n `div` 8) + 1) fd >.-.> snd

-- | Get switch status (one bit per switch)
--
-- EVIOCGSW
getDeviceSwitchStatus :: Int -> Handle -> SysRet BS.ByteString
getDeviceSwitchStatus n fd = ioctlReadByteString 0x45 0x1b ((n `div` 8) + 1) fd >.-.> snd

-- | Get the number of bits that can be set by the given event type
--
-- EVIOCGBIT
getDeviceBits :: EventType -> Int -> Handle -> SysRet BS.ByteString
getDeviceBits ev n fd = do
   let code = fromCEnum ev
   ioctlReadByteString 0x45 (0x20 + code) ((n `div` 8) + 1) fd >.-.> snd

-- | Get absolute info
--
-- EVIOCGABS
getDeviceAbsoluteInfo :: Word8 -> Handle -> SysRet AbsoluteInfo
getDeviceAbsoluteInfo code = ioctlRead 0x45 (0x40 + code)

-- | Set absolute info
--
-- EVIOCSABS
setDeviceAbsoluteInfo :: Word8 -> AbsoluteInfo -> Handle -> SysRet ()
setDeviceAbsoluteInfo code = ioctlWrite 0x45 (0xc0 + code)

-- | Send a force effect to a force feedback device
--
-- TODO: we should return the effect ID
--
-- EVIOCSFF
sendForceFeedback :: ForceFeedbackEffect -> Handle -> SysRet ()
sendForceFeedback = ioctlWrite 0x45 0x80

-- | Erase a force effect
--
-- EVIOCRMFF
removeForceFeedback :: Int64 -> Handle -> SysRet ()
removeForceFeedback = ioctlWriteValue 0x45 0x81

-- | Report the number of effects playable at the same time
--
-- EVIOCGEFFECTS
supportedSimultaneousEffects :: Handle -> SysRet Int
supportedSimultaneousEffects = ioctlRead 0x45 0x84

-- | Grab/release device
--
-- EVIOCGRAB
grabReleaseDevice :: Bool -> Handle -> SysRet ()
grabReleaseDevice grab = ioctlWriteValue 0x45 0x90 (fromBool grab :: Int)

-- | Grab device
grabDevice :: Handle -> SysRet ()
grabDevice = grabReleaseDevice True

-- | Release device
releaseDevice :: Handle -> SysRet ()
releaseDevice = grabReleaseDevice False

-- | Revoke device access
--
-- EVIOCREVOKE
revokeDevice :: Handle -> SysRet ()
revokeDevice = ioctlWriteValue 0x45 0x91 (0 :: Int)

-- | Get event mask (filter by type)
--
-- EVIOCGMASK - Retrieve current event mask
-- 
-- This ioctl allows user to retrieve the current event mask for specific
-- event type. The argument must be of type "struct input_mask" and
-- specifies the event type to query, the address of the receive buffer and
-- the size of the receive buffer.
-- 
-- The event mask is a per-client mask that specifies which events are
-- forwarded to the client. Each event code is represented by a single bit
-- in the event mask. If the bit is set, the event is passed to the client
-- normally. Otherwise, the event is filtered and will never be queued on
-- the client's receive buffer.
-- 
-- Event masks do not affect global state of the input device. They only
-- affect the file descriptor they are applied to.
-- 
-- The default event mask for a client has all bits set, i.e. all events
-- are forwarded to the client. If the kernel is queried for an unknown
-- event type or if the receive buffer is larger than the number of
-- event codes known to the kernel, the kernel returns all zeroes for those
-- codes.
-- 
-- At maximum, codes_size bytes are copied.
-- 
-- This ioctl may fail with ENODEV in case the file is revoked, EFAULT
-- if the receive-buffer points to invalid memory, or EINVAL if the kernel
-- does not implement the ioctl.
getMask :: Handle -> SysRet Mask
getMask = ioctlRead 0x45 0x92


-- | Set event mask (event filter by type)
--
-- EVIOCSMASK - Set event mask
-- 
-- This ioctl is the counterpart to EVIOCGMASK. Instead of receiving the
-- current event mask, this changes the client's event mask for a specific
-- type.  See EVIOCGMASK for a description of event-masks and the
-- argument-type.
-- 
-- This ioctl provides full forward compatibility. If the passed event type
-- is unknown to the kernel, or if the number of event codes specified in
-- the mask is bigger than what is known to the kernel, the ioctl is still
-- accepted and applied. However, any unknown codes are left untouched and
-- stay cleared. That means, the kernel always filters unknown codes
-- regardless of what the client requests.  If the new mask doesn't cover
-- all known event-codes, all remaining codes are automatically cleared and
-- thus filtered.
-- 
-- This ioctl may fail with ENODEV in case the file is revoked. EFAULT is
-- returned if the receive-buffer points to invalid memory. EINVAL is returned
-- if the kernel does not implement the ioctl.
setMask :: Mask -> Handle -> SysRet ()
setMask = ioctlWrite 0x45 0x93

-- | Set clock to use for timestamps
--
-- EVIOCCLOCKID
setDeviceClock :: Clock -> Handle -> SysRet ()
setDeviceClock clk = ioctlWrite 0x45 0xa0 (fromEnum clk :: Int)


-- | IDs
data DeviceID
   = DeviceBusID
   | DeviceVendorID
   | DeviceProductID
   | DeviceVersionID
   deriving (Show,Eq,Enum,CEnum)

-- | Bus type
data BusType
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
   deriving (Show,Eq,Enum)

instance CEnum BusType where
   fromCEnum x = fromIntegral $ if fromEnum x <= 0x05
      then fromEnum x + 0x01
      else fromEnum x - 0x06 + 0x10

   toCEnum x = case fromIntegral x of
      y | y == 0    -> error "Unknown bus type"
        | y <= 0x06 -> toEnum (y - 0x01)
        | otherwise -> toEnum (y - 0x10 + 0x06)
      
-- | Multi-touch tool types
data MultiTouchToolType
   = MultiTouchFinder
   | MultiTouchPen
   | MultiTouchPalm
   deriving (Show,Eq,Enum,CEnum)

-- | Force feedback effect status
data ForceFeedbackStatus
   = StatusStopped
   | StatusPlaying
   deriving (Show,Eq,Enum,CEnum)


-- Structures used in ioctls to upload effects to a device
-- They are pieces of a bigger structure (called ff_effect)
-- 
-- 
-- All duration values are expressed in ms. Values above 32767 ms (0x7fff)
-- should not be used and have unspecified results.


-- | Defines scheduling of the force-feedback effect
data ForceFeedbackReplay = ForceFeedbackReplay
   { ffReplayLength :: Word16 -- ^ Duration of the effect
   , ffReplayDelay  :: Word16 -- ^ Delay before effect should start playing
   } deriving (Show,Eq,Generic,CStorable)

instance Storable ForceFeedbackReplay where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke

-- | Defines what triggers the force-feedback effect
data ForceFeedbackTrigger = ForceFeedbackTrigger
   { ffTriggerButton   :: Word16 -- ^ number of the button triggering the effect
   , ffTriggerInterval :: Word16 -- ^ controls how soon the effect can be re-triggered
   } deriving (Show,Eq,Generic,CStorable)

instance Storable ForceFeedbackTrigger where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke

-- | Generic force-feedback effect envelope
--
-- The @attack_level and @fade_level are absolute values; when applying
-- envelope force-feedback core will convert to positive/negative
-- value based on polarity of the default level of the effect.
-- Valid range for the attack and fade levels is 0x0000 - 0x7fff
data ForceFeedbackEnvelope = ForceFeedbackEnvelope
   { ffEnvelopeAttackLength :: Word16 -- ^ duration of the attack (ms)
   , ffEnvelopeAttackLevel  :: Word16 -- ^ level at the beginning of the attack
   , ffEnvelopeFadeLength   :: Word16 -- ^ duration of fade (ms)
   , ffEnvelopeFadeLevel    :: Word16 -- ^ level at the end of fade
   } deriving (Eq,Show,Generic,CStorable)

instance Storable  ForceFeedbackEnvelope where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke

-- | Defines parameters of a constant force-feedback effect
data ForceFeedbackConstantEffect = ForceFeedbackConstantEffect
   { ffConstantEffectLevel    :: Int16                 -- ^ strength of the effect; may be negative
   , ffConstantEffectEnvelope :: ForceFeedbackEnvelope -- ^ envelope data
   } deriving (Eq,Show,Generic,CStorable)

instance Storable  ForceFeedbackConstantEffect where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke


-- | Defines parameters of a ramp force-feedback effect
data ForceFeedbackRampEffect = ForceFeedbackRampEffect
   { ffRampEffectStartLevel :: Int16                 -- ^ beginning strength of the effect; may be negative
   , ffRampEffectEndLevel   :: Int16                 -- ^ final strength of the effect; may be negative
   , ffRampEffectEnvelope   :: ForceFeedbackEnvelope -- ^ envelope data
   } deriving (Eq,Show,Generic,CStorable)

instance Storable  ForceFeedbackRampEffect where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke

-- | Defines a spring or friction force-feedback effect
data ForceFeedbackConditionEffect = ForceFeedbackConditionEffect
   { ffConditionEffectRightSaturation :: Word16 -- ^ maximum level when joystick moved all way to the right
   , ffConditionEffectLeftSaturation  :: Word16 -- ^ same for the left side
   , ffConditionEffectRightCoeff      :: Int16  -- ^ controls how fast the force grows when the joystick moves to the right
   , ffConditionEffectLeftCoeff       :: Int16  -- ^ same for the left side
   , ffConditionEffectDeadBand        :: Word16 -- ^ size of the dead zone, where no force is produced
   , ffConditionEffectCenter          :: Int16  -- ^ position of the dead zone
   } deriving (Eq,Show,Generic,CStorable)

instance Storable  ForceFeedbackConditionEffect where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke

-- | Defines parameters of a periodic force-feedback effect
-- Known waveforms - FF_SQUARE, FF_TRIANGLE, FF_SINE, FF_SAW_UP,
-- FF_SAW_DOWN, FF_CUSTOM. The exact syntax FF_CUSTOM is undefined
-- for the time being as no driver supports it yet.
--
-- Note: the data pointed by custom_data is copied by the driver.
-- You can therefore dispose of the memory after the upload/update.
data ForceFeedbackPeriodicEffect = ForceFeedbackPeriodicEffect
   { ffPeriodicEffectWaveform   :: EnumField Word16 ForceFeedbackPeriodicEffectType -- ^ kind of the effect (wave)
   , ffPeriodicEffectPeriod     :: Word16                -- ^ period of the wave (ms)
   , ffPeriodicEffectMagnitude  :: Int16                 -- ^ peak value
   , ffPeriodicEffectOffset     :: Int16                 -- ^ mean value of the wave (roughly)
   , ffPeriodicEffectPhase      :: Word16                -- ^ 'horizontal' shift
   , ffPeriodicEffectEnvelope   :: ForceFeedbackEnvelope -- ^ envelope data
   , ffPeriodicEffectCustomLen  :: Word32                -- ^ number of samples (FF_CUSTOM only)
   , ffPeriodicEffectCustomData :: Ptr Int16             -- ^ buffer of samples (FF_CUSTOM only)
   } deriving (Eq,Show,Generic,CStorable)

instance Storable  ForceFeedbackPeriodicEffect where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke

-- | Defines parameters of a periodic force-feedback effect
--
-- Some rumble pads have two motors of different weight. Strong_magnitude
-- represents the magnitude of the vibration generated by the heavy one.
data ForceFeedbackRumbleEffect = ForceFeedbackRumbleEffect
   { ffRumbleEffectStrongMagnitude :: Word16 -- ^ magnitude of the heavy motor
   , ffRumbleEffectWeakMagnitude   :: Word16 -- ^ magnitude of the light one
   } deriving (Eq,Show,Generic,CStorable)

instance Storable  ForceFeedbackRumbleEffect where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke


-- | Force feedback effect
--
-- struct ff_effect - defines force feedback effect
-- @type: type of the effect (FF_CONSTANT, FF_PERIODIC, FF_RAMP, FF_SPRING,
-- FF_FRICTION, FF_DAMPER, FF_RUMBLE, FF_INERTIA, or FF_CUSTOM)
-- @id: an unique id assigned to an effect
-- @direction: direction of the effect
-- @trigger: trigger conditions (struct ff_trigger)
-- @replay: scheduling of the effect (struct ff_replay)
-- @u: effect-specific structure (one of ff_constant_effect, ff_ramp_effect,
-- ff_periodic_effect, ff_condition_effect, ff_rumble_effect) further
-- defining effect parameters
-- 
-- This structure is sent through ioctl from the application to the driver.
-- To create a new effect application should set its @id to -1; the kernel
-- will return assigned @id which can later be used to update or delete
-- this effect.
-- 
-- Direction of the effect is encoded as follows:
-- 0 deg -> 0x0000 (down)
-- 90 deg -> 0x4000 (left)
-- 180 deg -> 0x8000 (up)
-- 270 deg -> 0xC000 (right)
data ForceFeedbackEffect = ForceFeedbackEffect
   { ffEffectType       :: EnumField Word16 ForceFeedbackEffectType
   , ffEffectID         :: Int16
   , ffEffectDirection  :: EnumField Word16 ForceFeedbackDirection
   , ffEffectTrigger    :: ForceFeedbackTrigger
   , ffEffectReplay     :: ForceFeedbackReplay
   , ffEffectParams     :: Union '[ ForceFeedbackConstantEffect
                                  , ForceFeedbackRampEffect
                                  , ForceFeedbackPeriodicEffect
                                  , Vector 2 ForceFeedbackConditionEffect -- one for each axis
                                  , ForceFeedbackRumbleEffect
                                  ]
   } deriving (Show,Generic,CStorable)

instance Storable ForceFeedbackEffect where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke


-- | Force feedback effect type
data ForceFeedbackEffectType
   = Rumble
   | Periodic
   | Constant
   | Spring
   | Friction
   | Damper
   | Inertia
   | Ramp
   deriving (Show,Eq,Enum)

instance CEnum ForceFeedbackEffectType where
   fromCEnum x = fromIntegral $ fromEnum x + 0x50
   toCEnum x   = toEnum (fromIntegral x - 0x50)

-- | Force feedback periodic effect types
data ForceFeedbackPeriodicEffectType
   = Square
   | Triangle
   | Sine
   | SawUp
   | SawDown
   | Custom
   deriving (Show,Eq,Enum)

instance CEnum ForceFeedbackPeriodicEffectType where
   fromCEnum x = fromIntegral $ fromEnum x + 0x58
   toCEnum x   = toEnum (fromIntegral x - 0x58)

-- | Set force feedback device properties
data ForceFeedbackDeviceProperties
   = FFGain
   | FFAutoCenter
   deriving (Show,Eq,Enum)

instance CEnum ForceFeedbackDeviceProperties where
   fromCEnum x = fromIntegral $ fromEnum x + 0x60
   toCEnum x   = toEnum (fromIntegral x - 0x60)


-- | Direction of the effect
data ForceFeedbackDirection
   = FFDown 
   | FFLeft 
   | FFUp 
   | FFRight
   deriving (Show,Eq,Enum)


-- Direction of the effect is encoded as follows:
-- 0 deg -> 0x0000 (down)
-- 90 deg -> 0x4000 (left)
-- 180 deg -> 0x8000 (up)
-- 270 deg -> 0xC000 (right)
instance CEnum ForceFeedbackDirection where
   fromCEnum x = fromIntegral $ fromEnum x `shiftL` 14
   toCEnum x   = toEnum (fromIntegral x `shiftR` 14)
