{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskus.System.Linux.Internals.Input
   ( Property (..)
   , EventType (..)
   , SyncEventType (..)
   , KeyEventType (..)
   , Key (..)
   , RelativeAxe (..)
   , AbsoluteAxe (..)
   , SwitchEventType (..)
   , MiscEventType (..)
   , LED (..)
   , AutoRepeat (..)
   , Sound (..)
   , Event (..)
   , protocolVersion
   , DeviceInfo (..)
   , AbsoluteInfo (..)
   , KeymapEntry (..)
   , KeymapFlag (..)
   , EventMask (..)
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
   , ioctlGetDeviceBits
   , getDeviceAbsoluteInfo
   , setDeviceAbsoluteInfo
   , sendForceFeedback
   , removeForceFeedback
   , supportedSimultaneousEffects
   , grabDevice
   , releaseDevice
   , revokeDevice
   , getEventMask
   , setEventMask
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

import Haskus.Format.Binary.BitSet as BitSet
import Haskus.Format.Binary.Enum
import Haskus.Format.Binary.Union
import Haskus.Format.Binary.Vector (Vector)
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Bits
import Haskus.Format.Binary.Buffer
import Haskus.Format.Binary.Ptr
import Haskus.Format.Binary.Storable
import Haskus.Format.String (peekCString)
import Haskus.System.Linux.Internals.Tables
import Haskus.System.Linux.Internals.Error
import Haskus.System.Linux.Internals.Handle
import Haskus.System.Linux.Time (TimeVal,Clock)
import Haskus.System.Linux.Ioctl
import Haskus.Utils.Flow
import Haskus.Utils.Maybe
import Haskus.Utils.Embed
import Haskus.Utils.Types.Generics (Generic)

import Data.Data
import System.IO.Unsafe (unsafePerformIO)


-- =============================================================
--    From linux/include/uapi/linux/input-event-codes.h
--    and  linux/Documentation/input/event-codes.txt
-- =============================================================

-- | Device properties and quirks
--
-- PropertyDirect + PropertyPointer
-- --------------------------------
-- The PropertyDirect property indicates that device coordinates should be
-- directly mapped to screen coordinates (not taking into account trivial
-- transformations, such as scaling, flipping and rotating). Non-direct input
-- devices require non-trivial transformation, such as absolute to relative
-- transformation for touchpads. Typical direct input devices: touchscreens,
-- drawing tablets; non-direct devices: touchpads, mice.
--
-- The PropertyPointer property indicates that the device is not transposed on
-- the screen and thus requires use of an on-screen pointer to trace user's
-- movements.  Typical pointer devices: touchpads, tablets, mice; non-pointer
-- device: touchscreen.
--
-- If neither PropertyDirect or PropertyPointer are set, the property is
-- considered undefined and the device type should be deduced in the traditional
-- way, using emitted event types.
--
-- PropertyButtonPad
-- -----------------
-- For touchpads where the button is placed beneath the surface, such that
-- pressing down on the pad causes a button click, this property should be set.
-- Common in clickpad notebooks and macbooks from 2009 and onwards.
--
-- Originally, the buttonpad property was coded into the bcm5974 driver version
-- field under the name integrated button. For backwards compatibility, both
-- methods need to be checked in userspace.
--
-- PropertySemiMultiTouch
-- ----------------------
-- Some touchpads, most common between 2008 and 2011, can detect the presence of
-- multiple contacts without resolving the individual positions; only the number
-- of contacts and a rectangular shape is known. For such touchpads, the semi-mt
-- property should be set.
--
-- Depending on the device, the rectangle may enclose all touches, like a
-- bounding box, or just some of them, for instance the two most recent touches.
-- The diversity makes the rectangle of limited use, but some gestures can
-- normally be extracted from it.
--
-- If PropertySemiMultiTouch is not set, the device is assumed to be a true MT
-- device.
--
-- PropertyTopButtonPad
-- --------------------
-- Some laptops, most notably the Lenovo *40 series provide a trackstick device
-- but do not have physical buttons associated with the trackstick device.
-- Instead, the top area of the touchpad is marked to show visual/haptic areas
-- for left, middle, right buttons intended to be used with the trackstick.
--
-- If PropertyTopButtonPad is set, userspace should emulate buttons accordingly.
-- This property does not affect kernel behavior.
-- The kernel does not provide button emulation for such devices but treats them
-- as any other PropertyButtonPad device.
--
-- PropertyAccelerometer
-- ---------------------
-- Directional axes on this device (absolute and/or relative x, y, z) represent
-- accelerometer data. All other axes retain their meaning. A device must not
-- mix regular directional axes and accelerometer axes on the same event node.
--
data Property
   = PropertyNeedPointer      -- ^ needs a pointer
   | PropertyDirect           -- ^ direct input devices
   | PropertyButtonPad        -- ^ has button(s) under pad
   | PropertySemiMultiTouch   -- ^ touch rectangle only
   | PropertyTopButtonPad     -- ^ softbuttons at top of pad
   | PropertyPointingStick    -- ^ is a pointing stick
   | PropertyAccelerometer    -- ^ has accelerometer
   deriving (Eq,Show,Enum,CEnum)

-- | Event types
data EventType
   = EventTypeSync                -- ^ Marker to separate events
   | EventTypeKey                 -- ^ State changes of keyboards, button, etc.
   | EventTypeRelative            -- ^ Relative axis changes
   | EventTypeAbsolute            -- ^ Absolute axis changes
   | EventTypeMisc                -- ^ Miscellaneous
   | EventTypeSwitch              -- ^ Binary state switch
   | EventTypeLED                 -- ^ Turn LEDs on and off
   | EventTypeSound               -- ^ Output sound to devices
   | EventTypeRepeat              -- ^ Auto-repeating devices
   | EventTypeForceFeedback       -- ^ Send force-feedback to an input device
   | EventTypePower               -- ^ Power button and switch
   | EventTypeForceFeedbackStatus -- ^ Receive force-feedback status
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
data SyncEventType
   = SyncReport           -- ^ Separate events into packets of events occuring at the same time
   | SyncConfig
   | SyncMultiTouchReport -- ^ Separate touch events
   | SyncDropped          -- ^ Buffer overrun: client should ignore all events up to the next SyncReport and query the device to obtain its current state
   deriving (Show,Eq,Enum,CEnum)

-- | Key event type
data KeyEventType
   = KeyRelease   -- ^ Key released
   | KeyPress     -- ^ Key pressed
   | KeyRepeat    -- ^ Key repeated (generated kernel event)
   deriving (Show,Eq,Enum,CEnum)


-- DO NOT CHANGE KEY ORDER OR ADD NEW KEYS WITHOUT CHANGING THE KEY TABLE IN
-- "Keys" SUBMODULE

-- | Keys
data Key
   = ReservedKey
   | Esc
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
   | Minus
   | Equal
   | BackSpace
   | Tab
   | Q
   | W
   | E
   | R
   | T
   | Y
   | U
   | I
   | O
   | P
   | LeftBrace
   | RightBrace
   | Enter
   | LeftCtrl
   | A
   | S
   | D
   | F
   | G
   | H
   | J
   | K
   | L
   | SemiColon
   | Apostrophe
   | Grave
   | LeftShift
   | BackSlash
   | Z
   | X
   | C
   | V
   | B
   | N
   | M
   | Comma
   | Dot
   | Slash
   | RightShift
   | KeyPadAsterisk
   | LeftAlt
   | Space
   | CapsLock
   | F1
   | F2
   | F3
   | F4
   | F5
   | F6
   | F7
   | F8
   | F9
   | F10
   | NumLock
   | SCrollLock
   | KeyPad7
   | KeyPad8
   | KeyPad9
   | KeyPadMinus
   | KeyPad4
   | KeyPad5
   | KeyPad6
   | KeyPadPlus
   | KeyPad1
   | KeyPad2
   | KeyPad3
   | KeyPad0
   | KeyPadDot
   | Zenkakuhankaku
   | Key102ND
   | F11
   | F12
   | RO
   | Katakana
   | Hiragana
   | Henkan
   | KatakanaHiragana
   | Muhenkan
   | KeyPadJPComma
   | KeyPadEnter
   | RightCtrl
   | KeyPadSlash
   | SysRq
   | RightAlt
   | LineFeed
   | Home
   | Up
   | PageUp
   | KeyLeft
   | KeyRight
   | End
   | Down
   | PageDown
   | Insert
   | Delete
   | Macro
   | Mute
   | VolumeDown
   | VolumeUp
   | Power            -- ^ System Power Down
   | KeyPadEqual
   | KeyPadPlusMinus
   | Pause
   | Scale            -- ^ Compiz Scale (Expose)
   | KeyPadComma
   | Hangeul
   | Hanja
   | Yen
   | LeftMeta
   | RightMeta
   | Compose
   | Stop
   | Again
   | Properties
   | Undo
   | Front
   | Copy
   | Open
   | Paste
   | Find
   | Cut
   | Help
   | Menu
   | Calc
   | Setup
   | Sleep
   | WakeUp
   | File
   | SendFile
   | DeleteFile
   | Transfer
   | Prog1
   | Prog2
   | Web
   | MsDos
   | ScreenLock
   | RotateDisplay    -- ^ Display orientation for e.g. tablets
   | CycleWindows
   | Mail
   | Bookmarks
   | Computer
   | Back
   | Forward
   | CloseCD
   | EjectCD
   | EjectCloseCD
   | NextSong
   | PlayPause
   | PreviousSong
   | StopCD
   | Record
   | Rewind
   | Phone            -- ^ Media Select Telephone
   | ISO
   | Config           -- ^ Consumer Control Configuration
   | HomePage
   | Refresh
   | Exit
   | Move
   | Edit
   | ScrollUp
   | ScrollDown
   | KeyPadLeftParen
   | KeypadRightParen
   | New
   | Redo
   | F13
   | F14
   | F15
   | F16
   | F17
   | F18
   | F19
   | F20
   | F21
   | F22
   | F23
   | F24
   | PlayCD
   | PauseCD
   | Prog3
   | Prog4
   | DashBoard
   | Suspend
   | Close
   | Play
   | FastForward
   | BassBoost
   | Print
   | HP
   | Camera
   | Sound
   | Question
   | Email
   | Chat
   | Search
   | Connect
   | Finance
   | Sport
   | Shop
   | AltErase
   | Cancel
   | BrightnessDown
   | BrightnessUp
   | Media
   | SwitchVideoMode  -- ^ Cycle between available video outputs (Monitor/LCD/TV-out/etc)
   | KbdIllumToggle
   | KbdIllumDown
   | KbdIllumUp
   | Send
   | Reply
   | ForwardMail
   | Save
   | Documents
   | Battery
   | BlueTooth
   | WLAN
   | UWB
   | Unknown
   | VideoNext        -- ^ drive next video source
   | VideoPrev        -- ^ drive previous video source
   | BrightnessCycle  -- ^ brightness up, after max is min
   | BrightnessAuto   -- ^ Set Auto Brightness: manual brightness control is off, rely on ambient
   | DisplayOff       -- ^ display device to off state
   | WWAN             -- ^ Wireless WAN (LTE, UMTS, GSM, etc.)
   | RfKill           -- ^ Key that controls all radios
   | MicMute          -- ^ Mute / unmute the microphone
   | Button0
   | Button1
   | Button2
   | Button3
   | Button4
   | Button5
   | Button6
   | Button7
   | Button8
   | Button9
   -- Mouse
   | MouseLeft
   | MouseRight
   | MouseMiddle
   | MouseSide
   | MouseExtra
   | MouseForward
   | MouseBack
   | MouseTask
   -- Joystick
   | JoystickTrigger
   | JoystickThumb
   | JoystickThumb2
   | JoystickTop
   | JoystickTop2
   | JoystickPinkie
   | JoystickBase
   | JoystickBase2
   | JoystickBase3
   | JoystickBase4
   | JoystickBase5
   | JoystickBase6
   | JoystickDead
   -- GamePad
   | GamePadA
   | GamePadB
   | GamePadC
   | GamePadX
   | GamePadY
   | GamePadZ
   | GamePadTL
   | GamePadTR
   | GamePadTL2
   | GamePadTR2
   | GamePadSelect
   | GamePadStart
   | GamePadMode
   | GamePadThumbL
   | GamePadThumbR
   -- Digital
   | DigitalToolPen
   | DigitalToolRubber
   | DigitalToolBrush
   | DigitalToolPencil
   | DigitalToolAirbrush
   | DigitalToolFinger
   | DigitalToolMouse
   | DigitalToolLens
   | DigitalToolQuintTap
   | DigitalTouch
   | DigitalStylus
   | DigitalStylus2
   | DigitalToolDoubleTap
   | DigitalToolTripleTap
   | DigitalToolQuadTap
   -- Wheel
   | WheelDown
   | WheelUp

   | Ok
   | Select
   | Goto
   | Clear
   | Power2
   | Option
   | Info
   | Time
   | Vendor
   | Archive
   | Program
   | Channel
   | Favorites
   | EPG
   | PVR
   | MHP
   | Language
   | Title
   | Subtitle
   | Angle
   | Zoom
   | Mode
   | Appboard
   | Screen
   | PC
   | TV
   | TV2
   | VCR
   | VCR2
   | SAT
   | SAT2
   | CD
   | Tape
   | Radio
   | Tuner
   | Player
   | Text
   | DVD
   | Aux
   | MP3
   | Audio
   | Video
   | Directory
   | List
   | Memo
   | Calendar
   | Red
   | Green
   | Yellow
   | Blue
   | ChannelUp
   | ChannelDown
   | First
   | Last
   | AB
   | Next
   | Restart
   | Slow
   | Shuffle
   | Break
   | Previous
   | Digits
   | Teen
   | Twen
   | VideoPhone
   | Games
   | ZoomIn
   | ZoomOut
   | ZoomReset
   | WordProcessor
   | Editor
   | Spreadsheet
   | GraphicsEditor
   | Presentation
   | DataBase
   | News
   | VoiceMail
   | AddressBook
   | Messenger
   | DisplayToggle
   | SpellCheck
   | LogOff
   | Dollar
   | Euro
   | FrameBack
   | FrameForward
   | ContextMenu
   | MediaRepeat
   | Key10ChannelsUp
   | Key10ChennelsDown
   | Images
   -- Insert delete
   | DeleteEndOfLine
   | DeleteEOS
   | InsertLine
   | DeleteLine
   -- FN key
   | FN
   | FNEsc
   | FNF1
   | FNF2
   | FNF3
   | FNF4
   | FNF5
   | FNF6
   | FNF7
   | FNF8
   | FNF9
   | FNF10
   | FNF11
   | FNF12
   | FN1
   | FN2
   | FND
   | FNE
   | FNF
   | FNS
   | FNB
   -- Braille
   | BrailleDot1
   | BrailleDot2
   | BrailleDot3
   | BrailleDot4
   | BrailleDot5
   | BrailleDot6
   | BrailleDot7
   | BrailleDot8
   | BrailleDot9
   | BrailleDot10
   -- Numeric
   | Numeric0
   | Numeric1
   | Numeric2
   | Numeric3
   | Numeric4
   | Numeric5
   | Numeric6
   | Numeric7
   | Numeric8
   | Numeric9
   | NumericStar
   | NumericPound
   | NumericA
   | NumericB
   | NumericC
   | NumericD

   | KeyCameraFocus
   | KeyWifiProtectedSetup

   | KeyTouchPadToggle
   | KeyTouchPadOn
   | KeyTouchPadOff

   | CameraZoomIn
   | CameraZoomOut
   | CameraUp
   | CameraDown
   | CameraLeft
   | CameraRight

   | AttendantOn
   | AttendantOff
   | AttendantToggle
   | LightsToggle

   | DPadUp
   | DPadDown
   | DPadLeft
   | DPadRight

   | AmbientLightSensorToggle

   | ButtonConfig
   | TaskManager
   | Journal
   | ControlPanel
   | AppsSelect
   | ScreenSaver
   | VoiceCommand

   | BrightnessMin
   | BrightnessMax

   | KBDInputAssistPrev
   | KBDInputAssistNext
   | KBDInputAssistPrevGroup
   | KBDInputAssistNextGroup
   | KBDInputAssistAccept
   | KBDInputAssistCancel

   -- Diagonal movement keys
   | RightUp
   | RightDown
   | LeftUp
   | LeftDown

   | RootMenu
   | MediaTopMenu

   | Numeric11
   | Numeric12

   | ToggleAudioDesc -- ^ Toggle Audio Description: refers to an audio service
                     -- that helps blind and visually impaired consumers understand the action in a
                     -- program. Note: in some countries this is referred to as "Video Description".
   | Toggle3DMode
   | NextFavorite
   | StopRecord
   | PauseRecord
   | VideoOnDemand
   | UnMute
   | FastReverse
   | SlowReverse

   | TriggerHappy1
   | TriggerHappy2
   | TriggerHappy3
   | TriggerHappy4
   | TriggerHappy5
   | TriggerHappy6
   | TriggerHappy7
   | TriggerHappy8
   | TriggerHappy9
   | TriggerHappy10
   | TriggerHappy11
   | TriggerHappy12
   | TriggerHappy13
   | TriggerHappy14
   | TriggerHappy15
   | TriggerHappy16
   | TriggerHappy17
   | TriggerHappy18
   | TriggerHappy19
   | TriggerHappy20
   | TriggerHappy21
   | TriggerHappy22
   | TriggerHappy23
   | TriggerHappy24
   | TriggerHappy25
   | TriggerHappy26
   | TriggerHappy27
   | TriggerHappy28
   | TriggerHappy29
   | TriggerHappy30
   | TriggerHappy31
   | TriggerHappy32
   | TriggerHappy33
   | TriggerHappy34
   | TriggerHappy35
   | TriggerHappy36
   | TriggerHappy37
   | TriggerHappy38
   | TriggerHappy39
   | TriggerHappy40

   | CustomKey Word16
   deriving (Data,Show,Eq)

-- Keys are 16-bit numbers. Some keys have special meaning associated by the
-- kernel. There are defined as constants in the C header file.
--
-- We want to associate a Key data-type constructor to each meaningful key:
--    data Key = KeyA | KeyB | ...
-- Because some key number haven't any special meaning, we would need to define
-- "reserved" keys to fill the holes if we want constructor tags to be equal to
-- the C constants:
--    data Key = KeyA | KeyReserved0 | KeyB | ...
-- As this isn't pretty, we want a single KeyCustom instead:
--    data Key = KeyA | KeyB | KeyCustom Word16
-- To do that, we use a permutation table that maps key numbers with a specified
-- meaning to the constructor tag number and "hole" numbers accordingly.
-- The permutation table is generated at compile time and conversions are in
-- O(1).
--
-- DO NOT CHANGE THE CONSTRUCTOR ORDER WITHOUT CHANGING THE PERMUTATION TABLE

keyTablePtr :: Ptr Word16
keyTablePtr = Ptr $(embedBytes keyTable)

peekKeyTable :: Integral a => a -> Word16
peekKeyTable = unsafePerformIO . peekElemOff keyTablePtr . fromIntegral

instance CEnum Key where
   toCEnum x
      | x <= fromIntegral keyTableMax
      , Just r <- makeEnumMaybe @Key (peekKeyTable x) = r
      | otherwise = CustomKey (fromIntegral x)

   fromCEnum = error "fromCEnumm not implemented for Key" --TODO

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
data SwitchEventType
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
data MiscEventType
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
   { eventTime  :: {-# UNPACK #-} !TimeVal
   , eventType  :: {-# UNPACK #-} !(EnumField Word16 EventType)
   , eventCode  :: {-# UNPACK #-} !Word16
   , eventValue :: {-# UNPACK #-} !Int32
   } deriving (Show,Eq,Generic,Storable)

-- | Protocol version
protocolVersion :: Word
protocolVersion = 0x010001

-- IOCTLs (0x00 - 0x7f)

-- | Device info
--
-- `struct input_id`
data DeviceInfo = DeviceInfo
   { infoBusType :: {-# UNPACK #-} !(EnumField Word16 BusType)
   , infoVendor  :: {-# UNPACK #-} !Word16
   , infoProduct :: {-# UNPACK #-} !Word16
   , infoVersion :: {-# UNPACK #-} !Word16
   } deriving (Show,Eq,Generic,Storable)

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
   { absValue      :: {-# UNPACK #-} !Int32   -- ^ Latest reported value for the axis
   , absMinimum    :: {-# UNPACK #-} !Int32   -- ^ Minimum value for the axis
   , absMaximum    :: {-# UNPACK #-} !Int32   -- ^ Maximum value for the axis
   , absFuzz       :: {-# UNPACK #-} !Int32   -- ^ Fuzz value used to filter noise from the event stream
   , absFlat       :: {-# UNPACK #-} !Int32   -- ^ Values that are within this value will be discarded and reported as 0 instead
   , absResolution :: {-# UNPACK #-} !Int32   -- ^ Resolution for the values reported for the axis
   } deriving (Show, Eq, Generic, Storable)

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
   { keymapEntryFlags    :: {-# UNPACK #-} !(BitSet Word8 KeymapFlag) -- ^ Indicate how kernel should handle the request
   , keymapEntryLength   :: {-# UNPACK #-} !Word8                     -- ^ Length of the scancode
   , keymapEntryIndex    :: {-# UNPACK #-} !Word16                    -- ^ Index in the keymap (may be used instead of the scancode)
   , keymapEntryKeyCode  :: {-# UNPACK #-} !Word32                    -- ^ Key code assigned to this scancode
   , keymapEntryScanCode :: {-# UNPACK #-} !(Vector 32 Word8)         -- ^ Scan in machine-endian form (up to 32 bytes)
   } deriving (Show,Generic,Storable)


data KeymapFlag
   = KeymapByIndex
   deriving (Eq,Show,Enum,CBitSet)

-- | Mask of events that are supported by the device
data EventMask = EventMask
   { maskType      :: {-# UNPACK #-} !Word32
   , maskCodesSize :: {-# UNPACK #-} !Word32
   , maskCodesPtr  :: {-# UNPACK #-} !Word64
   }
   deriving (Show,Eq,Generic,Storable)

-- | Get version
--
-- EVIOCGVERSION
getVersion :: MonadInIO m => Handle -> FlowT '[ErrorCode] m Int
getVersion = ioctlRead 0x45 0x01

-- | Get device info
--
-- EVIOCGID
getDeviceInfo :: MonadInIO m => Handle -> FlowT '[ErrorCode] m DeviceInfo
getDeviceInfo = ioctlRead 0x45 0x02

-- | Repeat settings
--
-- We use a structure instead of Vector 2 Word
data RepeatSettings = RepeatSettings
   { repeatDelay  :: {-# UNPACK #-} !Word
   , repeatPeriod :: {-# UNPACK #-} !Word
   }
   deriving (Show,Eq,Generic,Storable)

-- | Get repeat settings
--
-- EVIOCGREP
getRepeatSettings :: MonadInIO m => Handle -> FlowT '[ErrorCode] m RepeatSettings
getRepeatSettings = ioctlRead 0x45 0x03

-- | Set repeat settings
--
-- EVIOCSREP
setRepeatSettings :: MonadInIO m => RepeatSettings -> Handle -> FlowT '[ErrorCode] m ()
setRepeatSettings = ioctlWrite 0x45 0x03


-- | Get key code
--
-- EVIOCGKEYCODE_V2
getKeyCode :: MonadInIO m => Handle -> FlowT '[ErrorCode] m KeymapEntry
getKeyCode = ioctlRead 0x45 0x04

-- | Set key code
--
-- EVIOCSKEYCODE_V2
setKeyCode :: MonadInIO m => KeymapEntry -> Handle -> FlowT '[ErrorCode] m ()
setKeyCode = ioctlWrite 0x45 0x04

-- | Get device name
--
-- EVIOCGNAME
getDeviceName :: MonadInIO m => Handle -> FlowT '[ErrorCode] m String
getDeviceName = ioctlReadVariableBuffer 0x45 0x06 (const peekCString) 256

-- | Get physical location
--
-- EVIOCGPHYS
getDevicePhysicalLocation :: MonadInIO m => Handle -> FlowT '[ErrorCode] m String
getDevicePhysicalLocation = ioctlReadVariableBuffer 0x45 0x07 (const peekCString) 256

-- | Get unique identifier
--
-- EVIOCGUNIQ
getDeviceUniqueID :: MonadInIO m => Handle -> FlowT '[ErrorCode] m String
getDeviceUniqueID = ioctlReadVariableBuffer 0x45 0x08 (const peekCString) 256

-- | Get device properties
--
-- EVIOCGPROP
getDeviceProperties :: MonadInIO m => Handle -> FlowT '[ErrorCode] m String
getDeviceProperties = ioctlReadVariableBuffer 0x45 0x09 (const peekCString) 256

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
getDeviceMultiTouchSlots :: MonadInIO m => Word32 -> Word -> Handle -> FlowT '[ErrorCode] m [Int32]
getDeviceMultiTouchSlots code nSlots fd = do
   let sz = 4 * (nSlots + 1)
   allocaBytes (fromIntegral sz) $ \ptr -> do
      pokeByteOff ptr 0 code
      void <| ioctlReadBytes 0x45 0x0a (fromIntegral sz) ptr fd
      peekArray nSlots (castPtr ptr `indexPtr` 4)

-- | Get global key state (one bit per pressed key)
--
-- EVIOCGKEY
getDeviceKeys :: MonadInIO m => Word -> Handle -> FlowT '[ErrorCode] m Buffer
getDeviceKeys n fd = ioctlReadBuffer 0x45 0x18 ((n `div` 8) + 1) fd ||> snd

-- | Get all leds (one bit per led)
--
-- EVIOCGLED
getDeviceLEDs :: MonadInIO m => Word -> Handle -> FlowT '[ErrorCode] m Buffer
getDeviceLEDs n fd = ioctlReadBuffer 0x45 0x19 ((n `div` 8) + 1) fd ||> snd

-- | Get sound status (one bit per sound)
--
-- EVIOCGSND
getDeviceSoundStatus :: MonadInIO m => Word -> Handle -> FlowT '[ErrorCode] m Buffer
getDeviceSoundStatus n fd = ioctlReadBuffer 0x45 0x1a ((n `div` 8) + 1) fd ||> snd

-- | Get switch status (one bit per switch)
--
-- EVIOCGSW
getDeviceSwitchStatus :: MonadInIO m => Word -> Handle -> FlowT '[ErrorCode] m Buffer
getDeviceSwitchStatus n fd = ioctlReadBuffer 0x45 0x1b ((n `div` 8) + 1) fd ||> snd

-- | Return a bitset of the supported event codes for the given event type.
-- If no event type is given, a bitset of the supported event types is returned
-- instead.
--
-- Return the size of the written *bytes*
--
-- EVIOCGBIT
ioctlGetDeviceBits :: MonadInIO m => Maybe EventType -> Word -> Handle -> FlowT '[ErrorCode] m (Int64, Buffer)
ioctlGetDeviceBits ev n fd = do
   let code = fromMaybe 0 (fromCEnum <$> ev )
   ioctlReadBuffer 0x45 (0x20 + code) ((n `div` 8) + 1) fd

-- | Get absolute info
--
-- EVIOCGABS
getDeviceAbsoluteInfo :: MonadInIO m => Word8 -> Handle -> FlowT '[ErrorCode] m AbsoluteInfo
getDeviceAbsoluteInfo code = ioctlRead 0x45 (0x40 + code)

-- | Set absolute info
--
-- EVIOCSABS
setDeviceAbsoluteInfo :: MonadInIO m => Word8 -> AbsoluteInfo -> Handle -> FlowT '[ErrorCode] m ()
setDeviceAbsoluteInfo code = ioctlWrite 0x45 (0xc0 + code)

-- | Send a force effect to a force feedback device
--
-- TODO: we should return the effect ID
--
-- EVIOCSFF
sendForceFeedback :: MonadInIO m => ForceFeedbackEffect -> Handle -> FlowT '[ErrorCode] m ()
sendForceFeedback = ioctlWrite 0x45 0x80

-- | Erase a force effect
--
-- EVIOCRMFF
removeForceFeedback :: MonadInIO m => Int64 -> Handle -> FlowT '[ErrorCode] m ()
removeForceFeedback = ioctlWriteValue 0x45 0x81

-- | Report the number of effects playable at the same time
--
-- EVIOCGEFFECTS
supportedSimultaneousEffects :: MonadInIO m => Handle -> FlowT '[ErrorCode] m Int
supportedSimultaneousEffects = ioctlRead 0x45 0x84

-- | Grab/release device
--
-- EVIOCGRAB
grabReleaseDevice :: MonadInIO m => Bool -> Handle -> FlowT '[ErrorCode] m ()
grabReleaseDevice grab = ioctlWriteValue 0x45 0x90 (if grab then 1 else 0:: Int)

-- | Grab device
grabDevice :: MonadInIO m => Handle -> FlowT '[ErrorCode] m ()
grabDevice = grabReleaseDevice True

-- | Release device
releaseDevice :: MonadInIO m => Handle -> FlowT '[ErrorCode] m ()
releaseDevice = grabReleaseDevice False

-- | Revoke device access
--
-- EVIOCREVOKE
revokeDevice :: MonadInIO m => Handle -> FlowT '[ErrorCode] m ()
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
getEventMask :: MonadInIO m => Handle -> FlowT '[ErrorCode] m EventMask
getEventMask = ioctlRead 0x45 0x92


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
setEventMask :: MonadInIO m => EventMask -> Handle -> FlowT '[ErrorCode] m ()
setEventMask = ioctlWrite 0x45 0x93

-- | Set clock to use for timestamps
--
-- EVIOCCLOCKID
setDeviceClock :: MonadInIO m => Clock -> Handle -> FlowT '[ErrorCode] m ()
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
   { ffReplayLength :: {-# UNPACK #-} !Word16 -- ^ Duration of the effect
   , ffReplayDelay  :: {-# UNPACK #-} !Word16 -- ^ Delay before effect should start playing
   } deriving (Show,Eq,Generic,Storable)

-- | Defines what triggers the force-feedback effect
data ForceFeedbackTrigger = ForceFeedbackTrigger
   { ffTriggerButton   :: {-# UNPACK #-} !Word16 -- ^ number of the button triggering the effect
   , ffTriggerInterval :: {-# UNPACK #-} !Word16 -- ^ controls how soon the effect can be re-triggered
   } deriving (Show,Eq,Generic,Storable)

-- | Generic force-feedback effect envelope
--
-- The @attack_level and @fade_level are absolute values; when applying
-- envelope force-feedback core will convert to positive/negative
-- value based on polarity of the default level of the effect.
-- Valid range for the attack and fade levels is 0x0000 - 0x7fff
data ForceFeedbackEnvelope = ForceFeedbackEnvelope
   { ffEnvelopeAttackLength :: {-# UNPACK #-} !Word16 -- ^ duration of the attack (ms)
   , ffEnvelopeAttackLevel  :: {-# UNPACK #-} !Word16 -- ^ level at the beginning of the attack
   , ffEnvelopeFadeLength   :: {-# UNPACK #-} !Word16 -- ^ duration of fade (ms)
   , ffEnvelopeFadeLevel    :: {-# UNPACK #-} !Word16 -- ^ level at the end of fade
   } deriving (Eq,Show,Generic,Storable)

-- | Defines parameters of a constant force-feedback effect
data ForceFeedbackConstantEffect = ForceFeedbackConstantEffect
   { ffConstantEffectLevel    :: {-# UNPACK #-} !Int16                 -- ^ strength of the effect; may be negative
   , ffConstantEffectEnvelope :: {-# UNPACK #-} !ForceFeedbackEnvelope -- ^ envelope data
   } deriving (Eq,Show,Generic,Storable)

-- | Defines parameters of a ramp force-feedback effect
data ForceFeedbackRampEffect = ForceFeedbackRampEffect
   { ffRampEffectStartLevel :: {-# UNPACK #-} !Int16                 -- ^ beginning strength of the effect; may be negative
   , ffRampEffectEndLevel   :: {-# UNPACK #-} !Int16                 -- ^ final strength of the effect; may be negative
   , ffRampEffectEnvelope   :: {-# UNPACK #-} !ForceFeedbackEnvelope -- ^ envelope data
   } deriving (Eq,Show,Generic,Storable)

-- | Defines a spring or friction force-feedback effect
data ForceFeedbackConditionEffect = ForceFeedbackConditionEffect
   { ffConditionEffectRightSaturation :: {-# UNPACK #-} !Word16 -- ^ maximum level when joystick moved all way to the right
   , ffConditionEffectLeftSaturation  :: {-# UNPACK #-} !Word16 -- ^ same for the left side
   , ffConditionEffectRightCoeff      :: {-# UNPACK #-} !Int16  -- ^ controls how fast the force grows when the joystick moves to the right
   , ffConditionEffectLeftCoeff       :: {-# UNPACK #-} !Int16  -- ^ same for the left side
   , ffConditionEffectDeadBand        :: {-# UNPACK #-} !Word16 -- ^ size of the dead zone, where no force is produced
   , ffConditionEffectCenter          :: {-# UNPACK #-} !Int16  -- ^ position of the dead zone
   } deriving (Eq,Show,Generic,Storable)

-- | Defines parameters of a periodic force-feedback effect
-- Known waveforms - FF_SQUARE, FF_TRIANGLE, FF_SINE, FF_SAW_UP,
-- FF_SAW_DOWN, FF_CUSTOM. The exact syntax FF_CUSTOM is undefined
-- for the time being as no driver supports it yet.
--
-- Note: the data pointed by custom_data is copied by the driver.
-- You can therefore dispose of the memory after the upload/update.
data ForceFeedbackPeriodicEffect = ForceFeedbackPeriodicEffect
   { ffPeriodicEffectWaveform   :: {-# UNPACK #-} !(EnumField Word16 ForceFeedbackPeriodicEffectType) -- ^ kind of the effect (wave)
   , ffPeriodicEffectPeriod     :: {-# UNPACK #-} !Word16                -- ^ period of the wave (ms)
   , ffPeriodicEffectMagnitude  :: {-# UNPACK #-} !Int16                 -- ^ peak value
   , ffPeriodicEffectOffset     :: {-# UNPACK #-} !Int16                 -- ^ mean value of the wave (roughly)
   , ffPeriodicEffectPhase      :: {-# UNPACK #-} !Word16                -- ^ 'horizontal' shift
   , ffPeriodicEffectEnvelope   :: {-# UNPACK #-} !ForceFeedbackEnvelope -- ^ envelope data
   , ffPeriodicEffectCustomLen  :: {-# UNPACK #-} !Word32                -- ^ number of samples (FF_CUSTOM only)
   , ffPeriodicEffectCustomData :: {-# UNPACK #-} !(Ptr Int16)           -- ^ buffer of samples (FF_CUSTOM only)
   } deriving (Eq,Show,Generic,Storable)

-- | Defines parameters of a periodic force-feedback effect
--
-- Some rumble pads have two motors of different weight. Strong_magnitude
-- represents the magnitude of the vibration generated by the heavy one.
data ForceFeedbackRumbleEffect = ForceFeedbackRumbleEffect
   { ffRumbleEffectStrongMagnitude :: {-# UNPACK #-} !Word16 -- ^ magnitude of the heavy motor
   , ffRumbleEffectWeakMagnitude   :: {-# UNPACK #-} !Word16 -- ^ magnitude of the light one
   } deriving (Eq,Show,Generic,Storable)


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
   { ffEffectType       :: {-# UNPACK #-} !(EnumField Word16 ForceFeedbackEffectType)
   , ffEffectID         :: {-# UNPACK #-} !Int16
   , ffEffectDirection  :: {-# UNPACK #-} !(EnumField Word16 ForceFeedbackDirection)
   , ffEffectTrigger    :: {-# UNPACK #-} !ForceFeedbackTrigger
   , ffEffectReplay     :: {-# UNPACK #-} !ForceFeedbackReplay
   , ffEffectParams     :: {-# UNPACK #-} !(Union 
                                 '[ ForceFeedbackConstantEffect
                                  , ForceFeedbackRampEffect
                                  , ForceFeedbackPeriodicEffect
                                  , Vector 2 ForceFeedbackConditionEffect -- one for each axis
                                  , ForceFeedbackRumbleEffect
                                  ])
   } deriving (Show,Generic,Storable)

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
