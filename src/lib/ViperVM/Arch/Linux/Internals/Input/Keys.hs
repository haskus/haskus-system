{-# LANGUAGE DeriveAnyClass #-}

-- | Input keys
module ViperVM.Arch.Linux.Internals.Input.Keys
   ( Key (..)
   )
where

import ViperVM.Format.Binary.Enum

-- =============================================================
--    From linux/include/uapi/linux/input-event-codes.h
-- =============================================================

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

{-
#define BTN_MISC		0x100
#define BTN_0			0x100
#define BTN_1			0x101
#define BTN_2			0x102
#define BTN_3			0x103
#define BTN_4			0x104
#define BTN_5			0x105
#define BTN_6			0x106
#define BTN_7			0x107
#define BTN_8			0x108
#define BTN_9			0x109

#define BTN_MOUSE		0x110
#define BTN_LEFT		0x110
#define BTN_RIGHT		0x111
#define BTN_MIDDLE		0x112
#define BTN_SIDE		0x113
#define BTN_EXTRA		0x114
#define BTN_FORWARD		0x115
#define BTN_BACK		0x116
#define BTN_TASK		0x117

#define BTN_JOYSTICK		0x120
#define BTN_TRIGGER		0x120
#define BTN_THUMB		0x121
#define BTN_THUMB2		0x122
#define BTN_TOP			0x123
#define BTN_TOP2		0x124
#define BTN_PINKIE		0x125
#define BTN_BASE		0x126
#define BTN_BASE2		0x127
#define BTN_BASE3		0x128
#define BTN_BASE4		0x129
#define BTN_BASE5		0x12a
#define BTN_BASE6		0x12b
#define BTN_DEAD		0x12f

#define BTN_GAMEPAD		0x130
#define BTN_SOUTH		0x130
#define BTN_A			BTN_SOUTH
#define BTN_EAST		0x131
#define BTN_B			BTN_EAST
#define BTN_C			0x132
#define BTN_NORTH		0x133
#define BTN_X			BTN_NORTH
#define BTN_WEST		0x134
#define BTN_Y			BTN_WEST
#define BTN_Z			0x135
#define BTN_TL			0x136
#define BTN_TR			0x137
#define BTN_TL2			0x138
#define BTN_TR2			0x139
#define BTN_SELECT		0x13a
#define BTN_START		0x13b
#define BTN_MODE		0x13c
#define BTN_THUMBL		0x13d
#define BTN_THUMBR		0x13e

#define BTN_DIGI		0x140
#define BTN_TOOL_PEN		0x140
#define BTN_TOOL_RUBBER		0x141
#define BTN_TOOL_BRUSH		0x142
#define BTN_TOOL_PENCIL		0x143
#define BTN_TOOL_AIRBRUSH	0x144
#define BTN_TOOL_FINGER		0x145
#define BTN_TOOL_MOUSE		0x146
#define BTN_TOOL_LENS		0x147
#define BTN_TOOL_QUINTTAP	0x148	/* Five fingers on trackpad */
#define BTN_TOUCH		0x14a
#define BTN_STYLUS		0x14b
#define BTN_STYLUS2		0x14c
#define BTN_TOOL_DOUBLETAP	0x14d
#define BTN_TOOL_TRIPLETAP	0x14e
#define BTN_TOOL_QUADTAP	0x14f	/* Four fingers on trackpad */

#define BTN_WHEEL		0x150
#define BTN_GEAR_DOWN		0x150
#define BTN_GEAR_UP		0x151

#define KEY_OK			0x160
#define KEY_SELECT		0x161
#define KEY_GOTO		0x162
#define KEY_CLEAR		0x163
#define KEY_POWER2		0x164
#define KEY_OPTION		0x165
#define KEY_INFO		0x166	/* AL OEM Features/Tips/Tutorial */
#define KEY_TIME		0x167
#define KEY_VENDOR		0x168
#define KEY_ARCHIVE		0x169
#define KEY_PROGRAM		0x16a	/* Media Select Program Guide */
#define KEY_CHANNEL		0x16b
#define KEY_FAVORITES		0x16c
#define KEY_EPG			0x16d
#define KEY_PVR			0x16e	/* Media Select Home */
#define KEY_MHP			0x16f
#define KEY_LANGUAGE		0x170
#define KEY_TITLE		0x171
#define KEY_SUBTITLE		0x172
#define KEY_ANGLE		0x173
#define KEY_ZOOM		0x174
#define KEY_MODE		0x175
#define KEY_KEYBOARD		0x176
#define KEY_SCREEN		0x177
#define KEY_PC			0x178	/* Media Select Computer */
#define KEY_TV			0x179	/* Media Select TV */
#define KEY_TV2			0x17a	/* Media Select Cable */
#define KEY_VCR			0x17b	/* Media Select VCR */
#define KEY_VCR2		0x17c	/* VCR Plus */
#define KEY_SAT			0x17d	/* Media Select Satellite */
#define KEY_SAT2		0x17e
#define KEY_CD			0x17f	/* Media Select CD */
#define KEY_TAPE		0x180	/* Media Select Tape */
#define KEY_RADIO		0x181
#define KEY_TUNER		0x182	/* Media Select Tuner */
#define KEY_PLAYER		0x183
#define KEY_TEXT		0x184
#define KEY_DVD			0x185	/* Media Select DVD */
#define KEY_AUX			0x186
#define KEY_MP3			0x187
#define KEY_AUDIO		0x188	/* AL Audio Browser */
#define KEY_VIDEO		0x189	/* AL Movie Browser */
#define KEY_DIRECTORY		0x18a
#define KEY_LIST		0x18b
#define KEY_MEMO		0x18c	/* Media Select Messages */
#define KEY_CALENDAR		0x18d
#define KEY_RED			0x18e
#define KEY_GREEN		0x18f
#define KEY_YELLOW		0x190
#define KEY_BLUE		0x191
#define KEY_CHANNELUP		0x192	/* Channel Increment */
#define KEY_CHANNELDOWN		0x193	/* Channel Decrement */
#define KEY_FIRST		0x194
#define KEY_LAST		0x195	/* Recall Last */
#define KEY_AB			0x196
#define KEY_NEXT		0x197
#define KEY_RESTART		0x198
#define KEY_SLOW		0x199
#define KEY_SHUFFLE		0x19a
#define KEY_BREAK		0x19b
#define KEY_PREVIOUS		0x19c
#define KEY_DIGITS		0x19d
#define KEY_TEEN		0x19e
#define KEY_TWEN		0x19f
#define KEY_VIDEOPHONE		0x1a0	/* Media Select Video Phone */
#define KEY_GAMES		0x1a1	/* Media Select Games */
#define KEY_ZOOMIN		0x1a2	/* AC Zoom In */
#define KEY_ZOOMOUT		0x1a3	/* AC Zoom Out */
#define KEY_ZOOMRESET		0x1a4	/* AC Zoom */
#define KEY_WORDPROCESSOR	0x1a5	/* AL Word Processor */
#define KEY_EDITOR		0x1a6	/* AL Text Editor */
#define KEY_SPREADSHEET		0x1a7	/* AL Spreadsheet */
#define KEY_GRAPHICSEDITOR	0x1a8	/* AL Graphics Editor */
#define KEY_PRESENTATION	0x1a9	/* AL Presentation App */
#define KEY_DATABASE		0x1aa	/* AL Database App */
#define KEY_NEWS		0x1ab	/* AL Newsreader */
#define KEY_VOICEMAIL		0x1ac	/* AL Voicemail */
#define KEY_ADDRESSBOOK		0x1ad	/* AL Contacts/Address Book */
#define KEY_MESSENGER		0x1ae	/* AL Instant Messaging */
#define KEY_DISPLAYTOGGLE	0x1af	/* Turn display (LCD) on and off */
#define KEY_BRIGHTNESS_TOGGLE	KEY_DISPLAYTOGGLE
#define KEY_SPELLCHECK		0x1b0   /* AL Spell Check */
#define KEY_LOGOFF		0x1b1   /* AL Logoff */

#define KEY_DOLLAR		0x1b2
#define KEY_EURO		0x1b3

#define KEY_FRAMEBACK		0x1b4	/* Consumer - transport controls */
#define KEY_FRAMEFORWARD	0x1b5
#define KEY_CONTEXT_MENU	0x1b6	/* GenDesc - system context menu */
#define KEY_MEDIA_REPEAT	0x1b7	/* Consumer - transport control */
#define KEY_10CHANNELSUP	0x1b8	/* 10 channels up (10+) */
#define KEY_10CHANNELSDOWN	0x1b9	/* 10 channels down (10-) */
#define KEY_IMAGES		0x1ba	/* AL Image Browser */

#define KEY_DEL_EOL		0x1c0
#define KEY_DEL_EOS		0x1c1
#define KEY_INS_LINE		0x1c2
#define KEY_DEL_LINE		0x1c3

#define KEY_FN			0x1d0
#define KEY_FN_ESC		0x1d1
#define KEY_FN_F1		0x1d2
#define KEY_FN_F2		0x1d3
#define KEY_FN_F3		0x1d4
#define KEY_FN_F4		0x1d5
#define KEY_FN_F5		0x1d6
#define KEY_FN_F6		0x1d7
#define KEY_FN_F7		0x1d8
#define KEY_FN_F8		0x1d9
#define KEY_FN_F9		0x1da
#define KEY_FN_F10		0x1db
#define KEY_FN_F11		0x1dc
#define KEY_FN_F12		0x1dd
#define KEY_FN_1		0x1de
#define KEY_FN_2		0x1df
#define KEY_FN_D		0x1e0
#define KEY_FN_E		0x1e1
#define KEY_FN_F		0x1e2
#define KEY_FN_S		0x1e3
#define KEY_FN_B		0x1e4

#define KEY_BRL_DOT1		0x1f1
#define KEY_BRL_DOT2		0x1f2
#define KEY_BRL_DOT3		0x1f3
#define KEY_BRL_DOT4		0x1f4
#define KEY_BRL_DOT5		0x1f5
#define KEY_BRL_DOT6		0x1f6
#define KEY_BRL_DOT7		0x1f7
#define KEY_BRL_DOT8		0x1f8
#define KEY_BRL_DOT9		0x1f9
#define KEY_BRL_DOT10		0x1fa

#define KEY_NUMERIC_0		0x200	/* used by phones, remote controls, */
#define KEY_NUMERIC_1		0x201	/* and other keypads */
#define KEY_NUMERIC_2		0x202
#define KEY_NUMERIC_3		0x203
#define KEY_NUMERIC_4		0x204
#define KEY_NUMERIC_5		0x205
#define KEY_NUMERIC_6		0x206
#define KEY_NUMERIC_7		0x207
#define KEY_NUMERIC_8		0x208
#define KEY_NUMERIC_9		0x209
#define KEY_NUMERIC_STAR	0x20a
#define KEY_NUMERIC_POUND	0x20b
#define KEY_NUMERIC_A		0x20c	/* Phone key A - HUT Telephony 0xb9 */
#define KEY_NUMERIC_B		0x20d
#define KEY_NUMERIC_C		0x20e
#define KEY_NUMERIC_D		0x20f

#define KEY_CAMERA_FOCUS	0x210
#define KEY_WPS_BUTTON		0x211	/* WiFi Protected Setup key */

#define KEY_TOUCHPAD_TOGGLE	0x212	/* Request switch touchpad on or off */
#define KEY_TOUCHPAD_ON		0x213
#define KEY_TOUCHPAD_OFF	0x214

#define KEY_CAMERA_ZOOMIN	0x215
#define KEY_CAMERA_ZOOMOUT	0x216
#define KEY_CAMERA_UP		0x217
#define KEY_CAMERA_DOWN		0x218
#define KEY_CAMERA_LEFT		0x219
#define KEY_CAMERA_RIGHT	0x21a

#define KEY_ATTENDANT_ON	0x21b
#define KEY_ATTENDANT_OFF	0x21c
#define KEY_ATTENDANT_TOGGLE	0x21d	/* Attendant call on or off */
#define KEY_LIGHTS_TOGGLE	0x21e	/* Reading light on or off */

#define BTN_DPAD_UP		0x220
#define BTN_DPAD_DOWN		0x221
#define BTN_DPAD_LEFT		0x222
#define BTN_DPAD_RIGHT		0x223

#define KEY_ALS_TOGGLE		0x230	/* Ambient light sensor */

#define KEY_BUTTONCONFIG		0x240	/* AL Button Configuration */
#define KEY_TASKMANAGER		0x241	/* AL Task/Project Manager */
#define KEY_JOURNAL		0x242	/* AL Log/Journal/Timecard */
#define KEY_CONTROLPANEL		0x243	/* AL Control Panel */
#define KEY_APPSELECT		0x244	/* AL Select Task/Application */
#define KEY_SCREENSAVER		0x245	/* AL Screen Saver */
#define KEY_VOICECOMMAND		0x246	/* Listening Voice Command */

#define KEY_BRIGHTNESS_MIN		0x250	/* Set Brightness to Minimum */
#define KEY_BRIGHTNESS_MAX		0x251	/* Set Brightness to Maximum */

#define KEY_KBDINPUTASSIST_PREV		0x260
#define KEY_KBDINPUTASSIST_NEXT		0x261
#define KEY_KBDINPUTASSIST_PREVGROUP		0x262
#define KEY_KBDINPUTASSIST_NEXTGROUP		0x263
#define KEY_KBDINPUTASSIST_ACCEPT		0x264
#define KEY_KBDINPUTASSIST_CANCEL		0x265

/* Diagonal movement keys */
#define KEY_RIGHT_UP			0x266
#define KEY_RIGHT_DOWN			0x267
#define KEY_LEFT_UP			0x268
#define KEY_LEFT_DOWN			0x269

#define KEY_ROOT_MENU			0x26a /* Show Device's Root Menu */
/* Show Top Menu of the Media (e.g. DVD) */
#define KEY_MEDIA_TOP_MENU		0x26b
#define KEY_NUMERIC_11			0x26c
#define KEY_NUMERIC_12			0x26d
/*
 * Toggle Audio Description: refers to an audio service that helps blind and
 * visually impaired consumers understand the action in a program. Note: in
 * some countries this is referred to as "Video Description".
 */
#define KEY_AUDIO_DESC			0x26e
#define KEY_3D_MODE			0x26f
#define KEY_NEXT_FAVORITE		0x270
#define KEY_STOP_RECORD			0x271
#define KEY_PAUSE_RECORD		0x272
#define KEY_VOD				0x273 /* Video on Demand */
#define KEY_UNMUTE			0x274
#define KEY_FASTREVERSE			0x275
#define KEY_SLOWREVERSE			0x276
/*
 * Control a data application associated with the currently viewed channel,
 * e.g. teletext or data broadcast application (MHEG, MHP, HbbTV, etc.)
 */
#define KEY_DATA			0x275

#define BTN_TRIGGER_HAPPY		0x2c0
#define BTN_TRIGGER_HAPPY1		0x2c0
#define BTN_TRIGGER_HAPPY2		0x2c1
#define BTN_TRIGGER_HAPPY3		0x2c2
#define BTN_TRIGGER_HAPPY4		0x2c3
#define BTN_TRIGGER_HAPPY5		0x2c4
#define BTN_TRIGGER_HAPPY6		0x2c5
#define BTN_TRIGGER_HAPPY7		0x2c6
#define BTN_TRIGGER_HAPPY8		0x2c7
#define BTN_TRIGGER_HAPPY9		0x2c8
#define BTN_TRIGGER_HAPPY10		0x2c9
#define BTN_TRIGGER_HAPPY11		0x2ca
#define BTN_TRIGGER_HAPPY12		0x2cb
#define BTN_TRIGGER_HAPPY13		0x2cc
#define BTN_TRIGGER_HAPPY14		0x2cd
#define BTN_TRIGGER_HAPPY15		0x2ce
#define BTN_TRIGGER_HAPPY16		0x2cf
#define BTN_TRIGGER_HAPPY17		0x2d0
#define BTN_TRIGGER_HAPPY18		0x2d1
#define BTN_TRIGGER_HAPPY19		0x2d2
#define BTN_TRIGGER_HAPPY20		0x2d3
#define BTN_TRIGGER_HAPPY21		0x2d4
#define BTN_TRIGGER_HAPPY22		0x2d5
#define BTN_TRIGGER_HAPPY23		0x2d6
#define BTN_TRIGGER_HAPPY24		0x2d7
#define BTN_TRIGGER_HAPPY25		0x2d8
#define BTN_TRIGGER_HAPPY26		0x2d9
#define BTN_TRIGGER_HAPPY27		0x2da
#define BTN_TRIGGER_HAPPY28		0x2db
#define BTN_TRIGGER_HAPPY29		0x2dc
#define BTN_TRIGGER_HAPPY30		0x2dd
#define BTN_TRIGGER_HAPPY31		0x2de
#define BTN_TRIGGER_HAPPY32		0x2df
#define BTN_TRIGGER_HAPPY33		0x2e0
#define BTN_TRIGGER_HAPPY34		0x2e1
#define BTN_TRIGGER_HAPPY35		0x2e2
#define BTN_TRIGGER_HAPPY36		0x2e3
#define BTN_TRIGGER_HAPPY37		0x2e4
#define BTN_TRIGGER_HAPPY38		0x2e5
#define BTN_TRIGGER_HAPPY39		0x2e6
#define BTN_TRIGGER_HAPPY40		0x2e7
-}
