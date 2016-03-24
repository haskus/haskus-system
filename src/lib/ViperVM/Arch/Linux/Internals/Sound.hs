{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

module ViperVM.Arch.Linux.Internals.Sound
   ( Cea861AudioInfoFrame (..)
   , AesIec958 (..)
   -- * Hardware dependent: /dev/snd/hw*
   , hwVersion
   , HwInterface (..)
   , HwInfo (..)
   , HwDspStatus (..)
   , HwDspImage (..)
   , ioctlHwVersion
   , ioctlHwInfo
   , ioctlHwDspStatus
   , ioctlHwDspLoad
   -- * PCM: /dev/snd/pcm*
   , pcmVersion
   , PcmClass (..)
   , PcmFormat (..)
   , PcmInfoFlag (..)
   , PcmInfoFlags
   , PcmState (..)
   , PcmInfo (..)
   , PcmHwParam (..)
   , PcmHwParamsFlag (..)
   , PcmHwParamsFlags
   , Mask (..)
   , Interval (..)
   , IntervalOption (..)
   , IntervalOptions
   , PcmHwParams (..)
   , PcmTimeStampMode (..)
   , PcmSwParams (..)
   , PcmChannelInfo (..)
   , PcmAudioTimeStamp (..)
   , PcmStatus (..)
   , PcmMmapStatus (..)
   , PcmMmapControl (..)
   , PcmSyncFlag (..)
   , PcmSyncFlags
   , PcmSyncPtr (..)
   , XferI (..)
   , XferN (..)
   , PcmTimeStampType (..)
   , ChannelPosition (..)
   , ChannelOption (..)
   , ioctlPcmVersion
   , ioctlPcmInfo
   , ioctlPcmTimeStamp
   , ioctlPcmTTimeStamp
   , ioctlPcmHwRefine
   , ioctlPcmHwParams
   , ioctlPcmHwFree
   , ioctlPcmSwParams
   , ioctlPcmStatus
   , ioctlPcmDelay
   , ioctlPcmHwSync
   , ioctlPcmSyncPtr
   , ioctlPcmStatusExt
   , ioctlPcmChannelInfo
   , ioctlPcmPrepare
   , ioctlPcmReset
   , ioctlPcmStart
   , ioctlPcmDrop
   , ioctlPcmDrain
   , ioctlPcmPause
   , ioctlPcmRewind
   , ioctlPcmResume
   , ioctlPcmXRun
   , ioctlPcmForward
   , ioctlPcmWriteIFrames
   , ioctlPcmReadIFrames
   , ioctlPcmWriteNFrames
   , ioctlPcmReadNFrames
   , ioctlPcmLink
   , ioctlPcmUnlink
   -- * MIDI: /dev/snd/midi*
   , midiVersion
   , MidiStream (..)
   , MidiFlag (..)
   , MidiFlags
   , MidiInfo (..)
   , MidiParams (..)
   , MidiStatus (..)
   , ioctlMidiVersion
   , ioctlMidiInfo
   , ioctlMidiParams
   , ioctlMidiStatus
   , ioctlMidiDrop
   , ioctlMidiDrain
   -- * Timer: /dev/snd/timer
   , timerVersion
   , TimerClass (..)
   , TimerSlaveClass (..)
   , TimerGlobal (..)
   , TimerFlag (..)
   , TimerFlags
   , TimerId (..)
   , TimerGInfo (..)
   , TimerGParams (..)
   , TimerGStatus (..)
   , TimerSelect (..)
   , TimerInfo (..)
   , TimerParamsFlag (..)
   , TimerParamsFlags
   , TimerParams (..)
   , TimerStatus (..)
   , ioctlTimerVersion
   , ioctlTimerNextDevice
   , ioctlTimerTRead
   , ioctlTimerGInfo
   , ioctlTimerGParams
   , ioctlTimerGStatus
   , ioctlTimerSelect
   , ioctlTimerInfo
   , ioctlTimerParams
   , ioctlTimerStatus
   , ioctlTimerStart
   , ioctlTimerStop
   , ioctlTimerContinue
   , ioctlTimerPause
   , TimerRead (..)
   , TimerEvent (..)
   , TimerTRead (..)
   -- * Control: /dev/snd/control
   , controlVersion
   , ControlCardInfo (..)
   , ControlElementType (..)
   , ControlElementInterface (..)
   , ControlElementAccess (..)
   , ControlElementAccesses
   , ControlPower (..)
   , ControlElementId (..)
   , ControlElementList (..)
   , IntegerValue (..)
   , Integer64Value (..)
   , EnumeratedValue (..)
   , ControlElementInfo (..)
   , ControlElementValue (..)
   , ControlTLV (..)
   , ioctlControlVersion
   , ioctlControlCardInfo
   , ioctlControlElemList
   , ioctlControlElemInfo
   , ioctlControlElemRead
   , ioctlControlElemWrite
   , ioctlControlElemLock
   , ioctlControlElemUnlock
   , ioctlControlSubscribeEvents
   , ioctlControlElemAdd
   , ioctlControlElemReplace
   , ioctlControlElemRemove
   , ioctlControlTLVRead
   , ioctlControlTLVWrite
   , ioctlControlTLVCommand
   , ioctlControlHwDepNextDevice
   , ioctlControlHwInfo
   , ioctlControlPcmNextDevice
   , ioctlControlPcmInfo
   , ioctlControlPcmPreferSubdevice
   , ioctlControlMidiNextDevice
   , ioctlControlMidiInfo
   , ioctlControlMidiPreferSubdevice
   , ioctlControlPower
   , ioctlControlPowerState
   -- * Read interface
   , ControlEventType (..)
   , ControlEventMask (..)
   , ControlEvent (..)
   )
where

import Data.Word
import Data.Bits
import Data.Int
import Foreign.Ptr
import Foreign.CStorable
import Foreign.Storable
import Foreign.C.Types (CChar, CSize)
import GHC.Generics (Generic)

import ViperVM.Format.Binary.Vector (Vector)
import ViperVM.Format.Binary.Union
import ViperVM.Format.Binary.BitSet
import ViperVM.Format.Binary.Enum
import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Time
import ViperVM.Arch.Linux.Process (ProcessID)

-- From linux/include/uapi/sound/asound.h

-----------------------------------------------------------------------------
-- Digit audio interface
-----------------------------------------------------------------------------

data AesIec958 = AesIec958
   { aesStatus      :: Vector 24 Word8  -- ^ AES/IEC958 channel status bits
   , aesSubcode     :: Vector 147 Word8 -- ^ AES/IEC958 subcode bits
   , aesPadding     :: CChar            -- ^ nothing
   , aesDigSubFrame :: Vector 4 Word8   -- ^ AES/IEC958 subframe bits
   } deriving (Generic, Show, CStorable)

instance Storable AesIec958 where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf


-----------------------------------------------------------------------------
-- Digit audio interface
-----------------------------------------------------------------------------

data Cea861AudioInfoFrame = Cea861AudioInfoFrame
   { ceaCodingTypeChannelCount :: Word8 -- ^ coding type and channel count
   , ceaSampleFrequencySize    :: Word8 -- ^ sample frequency and size
   , ceaUnused                 :: Word8 -- ^ not used, all zeros
   , ceaChannelAllocationCode  :: Word8 -- ^ channel allocation code
   , ceaDownmixLevelShift      :: Word8 -- ^ downmix inhibit & level-shit values
   } deriving (Generic, Show, CStorable)

instance Storable Cea861AudioInfoFrame where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

-----------------------------------------------------------------------------
-- Section for driver hardware dependent interface - /dev/snd/hw?
-----------------------------------------------------------------------------

hwVersion :: Word32
hwVersion = 0x00010001


data HwInterface
   = HwInterfaceOPL2
   | HwInterfaceOPL3
   | HwInterfaceOPL4
   | HwInterfaceSB16CSP        -- ^ Creative Signal Processor
   | HwInterfaceEMU10K1        -- ^ FX8010 processor in EMU10K1 chip
   | HwInterfaceYSS225         -- ^ Yamaha FX processor
   | HwInterfaceICS2115        -- ^ Wavetable synth
   | HwInterfaceSSCAPE         -- ^ Ensoniq SoundScape ISA card (MC68EC000)
   | HwInterfaceVX             -- ^ Digigram VX cards
   | HwInterfaceMIXART         -- ^ Digigram miXart cards
   | HwInterfaceUSX2Y          -- ^ Tascam US122, US224 & US428 usb
   | HwInterfaceEMUX_WAVETABLE -- ^ EmuX wavetable
   | HwInterfaceBLUETOOTH      -- ^ Bluetooth audio
   | HwInterfaceUSX2Y_PCM      -- ^ Tascam US122, US224 & US428 rawusb pcm
   | HwInterfacePCXHR          -- ^ Digigram PCXHR
   | HwInterfaceSB_RC          -- ^ SB Extigy/Audigy2NX remote control
   | HwInterfaceHDA            -- ^ HD-audio
   | HwInterfaceUSB_STREAM     -- ^ direct access to usb stream
   | HwInterfaceFW_DICE        -- ^ TC DICE FireWire device
   | HwInterfaceFW_FIREWORKS   -- ^ Echo Audio Fireworks based device
   | HwInterfaceFW_BEBOB       -- ^ BridgeCo BeBoB based device
   | HwInterfaceFW_OXFW        -- ^ Oxford OXFW970/971 based device
   | HwInterfaceFW_DIGI00X     -- ^ Digidesign Digi 002/003 family
   | HwInterfaceFW_TASCAM      -- ^ TASCAM FireWire series
   deriving (Show,Eq)

data HwInfo = HwInfo
   { hwInfoDevice    :: Int             -- ^ WR: device number
   , hwInfoCard      :: Int             -- ^ R: card number
   , hwInfoId        :: Vector 64 CChar -- ^ ID (user selectable)
   , hwInfoName      :: Vector 80 CChar -- ^ hwdep name
   , hwInfoInterface :: Int             -- ^ hwdep interface
   , hwInfoReserved  :: Vector 64 Word8 -- ^ reserved for future
   } deriving (Generic, Show, CStorable)

instance Storable HwInfo where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

-- | Generic DSP loader
data HwDspStatus = HwDspStatus
   { hwDspVersion    :: Word            -- ^ R: driver-specific version
   , hwDspId         :: Vector 32 CChar -- ^ R: driver-specific ID string
   , hwDspNumDsps    :: Word            -- ^ R: number of DSP images to transfer
   , hwDspLoadedDsps :: Word            -- ^ R: bit flags indicating the loaded DSPs
   , hwDspChipReady  :: Word            -- ^ R: 1 = initialization finished
   , hwDspReserved   :: Vector 16 Word8 -- ^ reserved for future use
   } deriving (Generic, Show, CStorable)

instance Storable HwDspStatus where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data HwDspImage = HwDspImage
   { hwDspImageIndex      :: Word            -- ^ W: DSP index
   , hwDspImageName       :: Vector 64 CChar -- ^ W: ID (e.g. file name)
   , hwDspImageBin        :: Ptr ()          -- ^ W: binary image
   , hwDspImageLength     :: CSize           -- ^ W: size of image in bytes
   , hwDspImageDriverData :: Word64          -- ^ W: driver-specific data
   } deriving (Generic, Show, CStorable)

instance Storable HwDspImage where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

hwIoctlW :: Storable a => Word8 -> a -> FileDescriptor -> SysRet ()
hwIoctlW = ioctlWrite 0x48

hwIoctlR :: Storable a => Word8 -> FileDescriptor -> SysRet a
hwIoctlR = ioctlRead 0x48

ioctlHwVersion :: FileDescriptor -> SysRet Int
ioctlHwVersion = hwIoctlR 0x00

ioctlHwInfo :: FileDescriptor -> SysRet HwInfo
ioctlHwInfo = hwIoctlR 0x01

ioctlHwDspStatus :: FileDescriptor -> SysRet HwDspStatus
ioctlHwDspStatus = hwIoctlR 0x02

ioctlHwDspLoad :: HwDspImage -> FileDescriptor -> SysRet ()
ioctlHwDspLoad = hwIoctlW 0x03

-----------------------------------------------------------------------------
-- Digital Audio (PCM) interface - /dev/snd/pcm??
-----------------------------------------------------------------------------

pcmVersion :: Word32
pcmVersion = 0x0002000d

data PcmClass
   = PcmClassGeneric  -- ^ standard mono or stereo device
   | PcmClassMulti    -- ^ multichannel device
   | PcmClassModem    -- ^ software modem class
   | PcmClassDigitize -- ^ digitizer class
   deriving (Show,Eq,Enum)


data PcmSubClass
   = PcmSubClassGenericMix -- ^ mono or stereo subdevices are mixed together 
   | PcmSubClassMultiMix   -- ^ multichannel subdevices are mixed together 
   deriving (Show,Eq,Enum)

data PcmStream
   = PcmStreamPlayback
   | PcmStreamCapture
   deriving (Show,Eq,Enum)

data PcmAccess
   = PcmAccessMmapInterleaved     -- ^ interleaved mmap
   | PcmAccessMmapNontInterleaved -- ^ noninterleaved m
   | PcmAccessMmapComplex         -- ^ complex mmap
   | PcmAccessRwInterleaved       -- ^ readi/writei
   | PcmAccessRwNonInterleaved    -- ^ readn/writen
   deriving (Show,Eq,Enum)

data PcmFormat
   = PcmFormatS8
   | PcmFormatU8
   | PcmFormatS16_LE
   | PcmFormatS16_BE
   | PcmFormatU16_LE
   | PcmFormatU16_BE
   | PcmFormatS24_LE             -- ^ low three bytes
   | PcmFormatS24_BE             -- ^ low three bytes
   | PcmFormatU24_LE             -- ^ low three bytes
   | PcmFormatU24_BE             -- ^ low three bytes
   | PcmFormatS32_LE
   | PcmFormatS32_BE
   | PcmFormatU32_LE
   | PcmFormatU32_BE
   | PcmFormatFLOAT_LE           -- ^ 4-byte float, IEEE-754 32-bit, range -1.0 to 1.0
   | PcmFormatFLOAT_BE           -- ^ 4-byte float, IEEE-754 32-bit, range -1.0 to 1.0
   | PcmFormatFLOAT64_LE         -- ^ 8-byte float, IEEE-754 64-bit, range -1.0 to 1.0
   | PcmFormatFLOAT64_BE         -- ^ 8-byte float, IEEE-754 64-bit, range -1.0 to 1.0
   | PcmFormatIEC958_SUBFRAME_LE -- ^ IEC-958 subframe, Little Endian
   | PcmFormatIEC958_SUBFRAME_BE -- ^ IEC-958 subframe, Big Endian
   | PcmFormatMU_LAW
   | PcmFormatA_LAW
   | PcmFormatIMA_ADPCM
   | PcmFormatMPEG
   | PcmFormatGSM
   | PcmFormatSPECIAL
   | PcmFormatS24_3LE            -- ^ in three bytes
   | PcmFormatS24_3BE            -- ^ in three bytes
   | PcmFormatU24_3LE            -- ^ in three bytes
   | PcmFormatU24_3BE            -- ^ in three bytes
   | PcmFormatS20_3LE            -- ^ in three bytes
   | PcmFormatS20_3BE            -- ^ in three bytes
   | PcmFormatU20_3LE            -- ^ in three bytes
   | PcmFormatU20_3BE            -- ^ in three bytes
   | PcmFormatS18_3LE            -- ^ in three bytes
   | PcmFormatS18_3BE            -- ^ in three bytes
   | PcmFormatU18_3LE            -- ^ in three bytes
   | PcmFormatU18_3BE            -- ^ in three bytes
   | PcmFormatG723_24            -- ^ 8 samples in 3 bytes
   | PcmFormatG723_24_1B         -- ^ 1 sample in 1 byte
   | PcmFormatG723_40            -- ^ 8 Samples in 5 bytes
   | PcmFormatG723_40_1B         -- ^ 1 sample in 1 byte
   | PcmFormatDSD_U8             -- ^ DSD, 1-byte samples DSD (x8)
   | PcmFormatDSD_U16_LE         -- ^ DSD, 2-byte samples DSD (x16), little endian
   | PcmFormatDSD_U32_LE         -- ^ DSD, 4-byte samples DSD (x32), little endian
   | PcmFormatDSD_U16_BE         -- ^ DSD, 2-byte samples DSD (x16), big endian
   | PcmFormatDSD_U32_BE         -- ^ DSD, 4-byte samples DSD (x32), big endian
   deriving (Show,Eq)

instance Enum PcmFormat where
   fromEnum x = case x of
      PcmFormatS8                 -> 0
      PcmFormatU8                 -> 1
      PcmFormatS16_LE             -> 2
      PcmFormatS16_BE             -> 3
      PcmFormatU16_LE             -> 4
      PcmFormatU16_BE             -> 5
      PcmFormatS24_LE             -> 6
      PcmFormatS24_BE             -> 7
      PcmFormatU24_LE             -> 8
      PcmFormatU24_BE             -> 9
      PcmFormatS32_LE             -> 10
      PcmFormatS32_BE             -> 11
      PcmFormatU32_LE             -> 12
      PcmFormatU32_BE             -> 13
      PcmFormatFLOAT_LE           -> 14
      PcmFormatFLOAT_BE           -> 15
      PcmFormatFLOAT64_LE         -> 16
      PcmFormatFLOAT64_BE         -> 17
      PcmFormatIEC958_SUBFRAME_LE -> 18
      PcmFormatIEC958_SUBFRAME_BE -> 19
      PcmFormatMU_LAW             -> 20
      PcmFormatA_LAW              -> 21
      PcmFormatIMA_ADPCM          -> 22
      PcmFormatMPEG               -> 23
      PcmFormatGSM                -> 24
      PcmFormatSPECIAL            -> 31
      PcmFormatS24_3LE            -> 32
      PcmFormatS24_3BE            -> 33
      PcmFormatU24_3LE            -> 34
      PcmFormatU24_3BE            -> 35
      PcmFormatS20_3LE            -> 36
      PcmFormatS20_3BE            -> 37
      PcmFormatU20_3LE            -> 38
      PcmFormatU20_3BE            -> 39
      PcmFormatS18_3LE            -> 40
      PcmFormatS18_3BE            -> 41
      PcmFormatU18_3LE            -> 42
      PcmFormatU18_3BE            -> 43
      PcmFormatG723_24            -> 44
      PcmFormatG723_24_1B         -> 45
      PcmFormatG723_40            -> 46
      PcmFormatG723_40_1B         -> 47
      PcmFormatDSD_U8             -> 48
      PcmFormatDSD_U16_LE         -> 49
      PcmFormatDSD_U32_LE         -> 50
      PcmFormatDSD_U16_BE         -> 51
      PcmFormatDSD_U32_BE         -> 52

   toEnum x = case x of
    0  -> PcmFormatS8
    1  -> PcmFormatU8
    2  -> PcmFormatS16_LE
    3  -> PcmFormatS16_BE
    4  -> PcmFormatU16_LE
    5  -> PcmFormatU16_BE
    6  -> PcmFormatS24_LE
    7  -> PcmFormatS24_BE
    8  -> PcmFormatU24_LE
    9  -> PcmFormatU24_BE
    10 -> PcmFormatS32_LE
    11 -> PcmFormatS32_BE
    12 -> PcmFormatU32_LE
    13 -> PcmFormatU32_BE
    14 -> PcmFormatFLOAT_LE
    15 -> PcmFormatFLOAT_BE
    16 -> PcmFormatFLOAT64_LE
    17 -> PcmFormatFLOAT64_BE
    18 -> PcmFormatIEC958_SUBFRAME_LE
    19 -> PcmFormatIEC958_SUBFRAME_BE
    20 -> PcmFormatMU_LAW
    21 -> PcmFormatA_LAW
    22 -> PcmFormatIMA_ADPCM
    23 -> PcmFormatMPEG
    24 -> PcmFormatGSM
    31 -> PcmFormatSPECIAL
    32 -> PcmFormatS24_3LE
    33 -> PcmFormatS24_3BE
    34 -> PcmFormatU24_3LE
    35 -> PcmFormatU24_3BE
    36 -> PcmFormatS20_3LE
    37 -> PcmFormatS20_3BE
    38 -> PcmFormatU20_3LE
    39 -> PcmFormatU20_3BE
    40 -> PcmFormatS18_3LE
    41 -> PcmFormatS18_3BE
    42 -> PcmFormatU18_3LE
    43 -> PcmFormatU18_3BE
    44 -> PcmFormatG723_24
    45 -> PcmFormatG723_24_1B
    46 -> PcmFormatG723_40
    47 -> PcmFormatG723_40_1B
    48 -> PcmFormatDSD_U8
    49 -> PcmFormatDSD_U16_LE
    50 -> PcmFormatDSD_U32_LE
    51 -> PcmFormatDSD_U16_BE
    52 -> PcmFormatDSD_U32_BE
    _  -> error "Unknown PCM format"

data PcmSubFormat
   = PcmSubFormatStd
   deriving (Show,Eq,Enum)

data PcmInfoFlag
   = PcmInfoMmap                     -- ^ hardware supports mmap
   | PcmInfoMmapValid                -- ^ period data are valid during transfer
   | PcmInfoDouble                   -- ^ Double buffering needed for PCM start/stop
   | PcmInfoBatch                    -- ^ double buffering
   | PcmInfoInterleaved              -- ^ channels are interleaved
   | PcmInfoNonInterleaved           -- ^ channels are not interleaved
   | PcmInfoComplex                  -- ^ complex frame organization (mmap only)
   | PcmInfoBLockTransfer            -- ^ hardware transfer block of samples
   | PcmInfoOverrange                -- ^ hardware supports ADC (capture) overrange detection
   | PcmInfoResume                   -- ^ hardware supports stream resume after suspend
   | PcmInfoPause                    -- ^ pause ioctl is supported
   | PcmInfoHalfDuplex               -- ^ only half duplex
   | PcmInfoJOintDuplex              -- ^ playback and capture stream are somewhat correlated
   | PcmInfoSyncStart                -- ^ pcm support some kind of sync go
   | PcmInfoNoPeriodWakeUp           -- ^ period wakeup can be disabled
   | PcmInfoHasLinkAtime             -- ^ report hardware link audio time, reset on startup
   | PcmInfoHaskLinkAbsoluteAtime    -- ^ report absolute hardware link audio time, not reset on startup
   | PcmInfoHasLinkEstimatedAtime    -- ^ report estimated link audio time
   | PcmInfoHasLinkSynchronizedAtime -- ^ report synchronized audio/system time
   | PcmInfoDrainTrigger             -- ^ internal kernel flag - trigger in drain
   | PcmInfoFifoInFrames             -- ^ internal kernel flag - FIFO size is in frames
   deriving (Show,Eq,Enum)

instance CBitSet PcmInfoFlag where
   toBitOffset x = case x of
      PcmInfoMmap                     -> 0
      PcmInfoMmapValid                -> 1
      PcmInfoDouble                   -> 2
      PcmInfoBatch                    -> 4
      PcmInfoInterleaved              -> 8
      PcmInfoNonInterleaved           -> 9
      PcmInfoComplex                  -> 10
      PcmInfoBLockTransfer            -> 16
      PcmInfoOverrange                -> 17
      PcmInfoResume                   -> 18
      PcmInfoPause                    -> 19
      PcmInfoHalfDuplex               -> 20
      PcmInfoJOintDuplex              -> 21
      PcmInfoSyncStart                -> 22
      PcmInfoNoPeriodWakeUp           -> 23
      PcmInfoHasLinkAtime             -> 24
      PcmInfoHaskLinkAbsoluteAtime    -> 25
      PcmInfoHasLinkEstimatedAtime    -> 26
      PcmInfoHasLinkSynchronizedAtime -> 27
      PcmInfoDrainTrigger             -> 30
      PcmInfoFifoInFrames             -> 31
   fromBitOffset x = case x of
      0  -> PcmInfoMmap
      1  -> PcmInfoMmapValid
      2  -> PcmInfoDouble
      4  -> PcmInfoBatch
      8  -> PcmInfoInterleaved
      9  -> PcmInfoNonInterleaved
      10 -> PcmInfoComplex
      16 -> PcmInfoBLockTransfer
      17 -> PcmInfoOverrange
      18 -> PcmInfoResume
      19 -> PcmInfoPause
      20 -> PcmInfoHalfDuplex
      21 -> PcmInfoJOintDuplex
      22 -> PcmInfoSyncStart
      23 -> PcmInfoNoPeriodWakeUp
      24 -> PcmInfoHasLinkAtime
      25 -> PcmInfoHaskLinkAbsoluteAtime
      26 -> PcmInfoHasLinkEstimatedAtime
      27 -> PcmInfoHasLinkSynchronizedAtime
      30 -> PcmInfoDrainTrigger
      31 -> PcmInfoFifoInFrames
      _  -> error "Unknown PCM info flag"

type PcmInfoFlags = BitSet Word32 PcmInfoFlag


data PcmState
   = PcmStateOpen         -- ^ stream is open
   | PcmStateSetup        -- ^ stream has a setup
   | PcmStatePrepared     -- ^ stream is ready to start
   | PcmStateRunning      -- ^ stream is running
   | PcmStateXRun         -- ^ stream reached an xrun
   | PcmStateDraining     -- ^ stream is draining
   | PcmStatePaused       -- ^ stream is paused
   | PcmStateSuspended    -- ^ hardware is suspended
   | PcmStateDisconnected -- ^ hardware is disconnected
   deriving (Show,Eq,Enum,CEnum)

type PcmStateField = EnumField Int PcmState

data PcmMmapOffset
   = PcmMmapOffsetData
   | PcmMmapOffsetStatus
   | PcmMmapOffsetControl
   deriving (Show,Eq,Enum)

instance CEnum PcmMmapOffset where
   fromCEnum x = case x of
      PcmMmapOffsetData    -> 0x00000000
      PcmMmapOffsetStatus  -> 0x80000000
      PcmMmapOffsetControl -> 0x81000000
   toCEnum x = case x of
      0x00000000 -> PcmMmapOffsetData
      0x80000000 -> PcmMmapOffsetStatus
      0x81000000 -> PcmMmapOffsetControl
      _          -> error "Unknown PCM map offset"

data PcmInfo = PcmInfo
   { pcmInfoDevice               :: Word            -- ^ RO/WR (control): device number
   , pcmInfoSubDevice            :: Word            -- ^ RO/WR (control): subdevice number
   , pcmInfoStream               :: Int             -- ^ RO/WR (control): stream direction
   , pcmInfoCard                 :: Int             -- ^ R: card number
   , pcmInfoID                   :: Vector 64 CChar -- ^ ID (user selectable)
   , pcmInfoName                 :: Vector 80 CChar -- ^ name of this device
   , pcmInfoSubName              :: Vector 32 CChar -- ^ subdevice name
   , pcmInfoDevClass             :: Int             -- ^ SNDRV_PCM_CLASS_*
   , pcmInfoDevSubClass          :: Int             -- ^ SNDRV_PCM_SUBCLASS_*
   , pcmInfoSubDevicesCount      :: Word
   , pcmInfoSubDevicesAvailabled :: Word
   , pcmInfoSync                 :: Vector 16 Word8 -- ^ hardware synchronization ID
   , pcmInfoReserved             :: Vector 64 Word8 -- ^ reserved for future...
   } deriving (Generic, Show, CStorable)

instance Storable PcmInfo where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data PcmHwParam
   = PcmHwParamAccess      -- ^ Access type
   | PcmHwParamFormat      -- ^ Format
   | PcmHwParamSubFormat   -- ^ Subformat
   | PcmHwParamSampleBits  -- ^ Bits per sample 
   | PcmHwParamFrameBits   -- ^ Bits per frame 
   | PcmHwParamChannels    -- ^ Channels 
   | PcmHwParamRate        -- ^ Approx rate 
   | PcmHwParamPeriodTime  -- ^ Approx distance between interrupts in us 
   | PcmHwParamPeriodSize  -- ^ Approx frames between interrupts 
   | PcmHwParamPeriodBytes -- ^ Approx bytes between interrupts 
   | PcmHwParamPeriods     -- ^ Approx interrupts per buffer 
   | PcmHwParamBufferTime  -- ^ Approx duration of buffer in us 
   | PcmHwParamBufferSize  -- ^ Size of buffer in frames 
   | PcmHwParamBufferBytes -- ^ Size of buffer in bytes 
   | PcmHwParamTickTime    -- ^ Approx tick duration in us 
   deriving (Show,Eq)

instance Enum PcmHwParam where
   fromEnum x = case x of
      PcmHwParamAccess      -> 0
      PcmHwParamFormat      -> 1
      PcmHwParamSubFormat   -> 2
      PcmHwParamSampleBits  -> 8
      PcmHwParamFrameBits   -> 9
      PcmHwParamChannels    -> 10
      PcmHwParamRate        -> 11
      PcmHwParamPeriodTime  -> 12
      PcmHwParamPeriodSize  -> 13
      PcmHwParamPeriodBytes -> 14
      PcmHwParamPeriods     -> 15
      PcmHwParamBufferTime  -> 16
      PcmHwParamBufferSize  -> 17
      PcmHwParamBufferBytes -> 18
      PcmHwParamTickTime    -> 19
   toEnum x = case x of
      0  -> PcmHwParamAccess
      1  -> PcmHwParamFormat
      2  -> PcmHwParamSubFormat
      8  -> PcmHwParamSampleBits
      9  -> PcmHwParamFrameBits
      10 -> PcmHwParamChannels
      11 -> PcmHwParamRate
      12 -> PcmHwParamPeriodTime
      13 -> PcmHwParamPeriodSize
      14 -> PcmHwParamPeriodBytes
      15 -> PcmHwParamPeriods
      16 -> PcmHwParamBufferTime
      17 -> PcmHwParamBufferSize
      18 -> PcmHwParamBufferBytes
      19 -> PcmHwParamTickTime
      _  -> error "Unknown PCM HW Param"

data Interval = Interval
   { intervalMin :: Word
   , intervalMax :: Word
   , intervalOptions :: IntervalOptions
   } deriving (Show,Eq,Generic,CStorable)

instance Storable Interval where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data IntervalOption
   = IntervalOpenMin
   | IntervalOpenMax
   | IntervalInteger
   | IntervalEmpty
   deriving (Show,Eq,Enum,CBitSet)

type IntervalOptions = BitSet Word IntervalOption

data PcmHwParamsFlag
   = PcmHwParamsNoResample     -- ^ avoid rate resampling
   | PcmHwParamsExportBuffer   -- ^ export buffer
   | PcmHwParamsNoPeriodWakeUp -- ^ disable period wakeups
   deriving (Show,Eq,Enum,CBitSet)

type PcmHwParamsFlags = BitSet Word PcmHwParamsFlag

data Mask = Mask
   { maskBits :: Vector 8 Word32
   } deriving (Generic,CStorable,Show)

instance Storable Mask where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data PcmHwParams = PcmHwParams
   { pcmHwParamsFlags               :: PcmHwParamsFlags
   , pcmHwParamsMasks               :: Vector 8 Mask
   , pcmHwParamsIntervals           :: Vector 21 Interval
   , pcmHwParamsRequestedMasks      :: Word               -- ^ W: requested masks
   , pcmHwParamsChangedMasks        :: Word               -- ^ R: changed masks
   , pcmHwParamsInfo                :: Word               -- ^ R: Info flags for returned setup
   , pcmHwParamsMostSignificantBits :: Word               -- ^ R: used most significant bits
   , pcmHwParamsRateNumerator       :: Word               -- ^ R: rate numerator
   , pcmHwParamsRateDenominator     :: Word               -- ^ R: rate denominator
   , pcmHwParamsFifoSize            :: Word64             -- ^ R: chip FIFO size in frames
   , pcmHwParamsReserved            :: Vector 64 Word8    -- ^ reserved for future
   } deriving (Generic, CStorable, Show)

instance Storable PcmHwParams where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf


data PcmTimeStampMode
   = PcmTimeStampNone
   | PcmTimeStampEnabled
   deriving (Show,Eq,Enum)

data PcmSwParams = PcmSwParams
   { pcmSwParamsTimeStamp        :: Int             -- ^ timestamp mode
   , pcmSwParamsPeriodStep       :: Word
   , pcmSwParamsSleepMin         :: Word            -- ^ min ticks to sleep
   , pcmSwParamsAvailMin         :: Word64          -- ^ min avail frames for wakeup
   , pcmSwParamsXFerAlign        :: Word64          -- ^ obsolete: xfer size need to be a multiple
   , pcmSwParamsStartThreshold   :: Word64          -- ^ min hw_avail frames for automatic start
   , pcmSwParamsStopThreshold    :: Word64          -- ^ min avail frames for automatic stop
   , pcmSwParamsSilenceThreshold :: Word64          -- ^ min distance from noise for silence filling
   , pcmSwParamsSilenceSize      :: Word64          -- ^ silence block size
   , pcmSwParamsBoundary         :: Word64          -- ^ pointers wrap point
   , pcmSwParamsProtoVersion     :: Word            -- ^ protocol version
   , pcmSwParamsTimeStampType    :: Word            -- ^ timestamp type (req. proto >= 2.0.12)
   , pcmSwParamsReserved         :: Vector 56 Word8 -- ^ reserved for future
   } deriving (Generic, CStorable, Show)

instance Storable PcmSwParams where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf


data PcmChannelInfo = PcmChannelInfo
   { pcmChannelInfoChannel :: Word
   , pcmChannelInfoOffset  :: Int64 -- ^ mmap offset
   , pcmChannelInfoFirst   :: Word  -- ^ offset to first sample in bits
   , pcmChannelInfoStep    :: Word  -- ^ samples distance in bits
   } deriving (Show,Generic,CStorable)

instance Storable PcmChannelInfo where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data PcmAudioTimeStamp
   = PcmAudioTimeStampCompat           -- ^ For backwards compatibility only, maps to wallclock/link time for HDAudio playback and DEFAULT/DMA time for everything else
   | PcmAudioTimeStampDefault          -- ^ DMA time, reported as per hw_ptr
   | PcmAudioTimeStampLink             -- ^ link time reported by sample or wallclock counter, reset on startup
   | PcmAudioTimeStampLinkAbsolute     -- ^ link time reported by sample or wallclock counter, not reset on startup
   | PcmAudioTimeStampLinkEstimated    -- ^ link time estimated indirectly
   | PcmAudioTimeStampLinkSynchronized -- ^ link time synchronized with system time
   deriving (Show,Eq,Enum)

data PcmStatus = PcmStatus
   { pcmStatusState              :: PcmStateField   -- ^ stream state
   , pcmStatusTriggerTimeStamp   :: TimeSpec        -- ^ time when stream was started/stopped/paused
   , pcmStatusTimeStamp          :: TimeSpec        -- ^ reference timestamp
   , pcmStatusApplPtr            :: Word64          -- ^ appl ptr
   , pcmStatusHwPtr              :: Word64          -- ^ hw ptr
   , pcmStatusDelay              :: Word64          -- ^ current delay in frames
   , pcmStatusAvail              :: Word64          -- ^ number of frames available
   , pcmStatusAvailMax           :: Word64          -- ^ max frames available on hw since last status
   , pcmStatusOverRange          :: Word64          -- ^ count of ADC (capture) overrange detections from last status
   , pcmStatusSyspendedState     :: PcmStateField   -- ^ suspended stream state
   , pcmStatusAudioTimeStampData :: Word32          -- ^ needed for 64-bit alignment, used for configs/report to/from userspace
   , pcmStatusAudioTimeStamp     :: TimeSpec        -- ^ sample counter, wall clock, PHC or on-demand sync'ed
   , pcmStatusDriverTimeStamp    :: TimeSpec        -- ^ useful in case reference system tstamp is reported with delay
   , pcmStatusTimeStampAccuracy  :: Word32          -- ^ in ns units, only valid if indicated in audio_tstamp_data
   , pcmStatusReserved           :: Vector 20 Word8 -- ^ must be filled with zero
   } deriving (Show,Generic,CStorable)

instance Storable PcmStatus where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf


data PcmMmapStatus = PcmMmapStatus
   { pcmMmapStatusState          :: PcmStateField -- ^ RO: state - SNDRV_PCM_STATE_XXXX
   , pcmMmapStatusPadding        :: Int      -- ^ Needed for 64 bit alignment
   , pcmMmapStatusHwPtr          :: Word64   -- ^ RO: hw ptr (0...boundary-1)
   , pcmMmapStatusTimeStamp      :: TimeSpec -- ^ Timestamp
   , pcmMmapStatusSuspendedState :: PcmStateField -- ^ RO: suspended stream state
   , pcmMmapStatusAudioTimeStamp :: TimeSpec -- ^ from sample counter or wall clock
   } deriving (Show,Generic,CStorable)

instance Storable PcmMmapStatus where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data PcmMmapControl = PcmMmapControl
   { pcmMmapControlApplPtr  :: Word64  -- ^ RW: appl ptr (0...boundary-1)
   , pcmMmapControlAvailMin :: Word64  -- ^ RW: min available frames for wakeup
   } deriving (Show,Generic,CStorable)

instance Storable PcmMmapControl where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data PcmSyncFlag
   = PcmSyncFlagHwSync        -- ^ execute hwsync 
   | PcmSyncFlagPtrAppl       -- ^ get appl_ptr from driver (r/w op) 
   | PcmSyncFlagPtrAvailMin   -- ^ get avail_min from driver 
   deriving (Show,Eq,Enum,CBitSet)

type PcmSyncFlags = BitSet Word PcmSyncFlag

data PcmSyncPtr = PcmSyncPtr
   { pcmSyncPtrFlags :: PcmSyncFlags
   , pcmSyncPtrStatus :: PcmMmapStatus
   , pcmSyncPtrControl   :: PcmMmapControl
   , pcmSyncPtrPadding :: Vector 48 Word8
   } deriving (Show, Generic, CStorable)

instance Storable PcmSyncPtr where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf


data XferI = XferI
   { xferiResult :: Int64
   , xferiBuffer :: Ptr ()
   , xferiFrames :: Word64
   } deriving (Show, Generic, CStorable)

instance Storable XferI where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data XferN = XferN
   { xfernResult  :: Int64
   , xfernBuffers :: Ptr (Ptr ())
   , xfernFrames  :: Word64
   } deriving (Show, Generic, CStorable)

instance Storable XferN where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data PcmTimeStampType
   = PcmTimeStampGetTimeOfDay  -- ^ gettimeofday equivalent 
   | PcmTimeStampMonotonic     -- ^ posix_clock_monotonic equivalent 
   | PcmTimeStampMonotonicRaw  -- ^ monotonic_raw (no NTP) 
   deriving (Show,Eq,Enum)


-- | Channel positions
data ChannelPosition
   = ChannelPosUnknown             -- ^ Unknown
   | ChannelPosNA                  -- ^ N/A, silent
   | ChannelPosMono                -- ^ mono stream
   | ChannelPosFrontLeft           -- ^ front left
   | ChannelPosFrontRight          -- ^ front right
   | ChannelPosRearLeft            -- ^ rear left
   | ChannelPosRearRight           -- ^ rear right
   | ChannelPosFrontCenter         -- ^ front center
   | ChannelPosLFE                 -- ^ LFE
   | ChannelPosSideLeft            -- ^ side left
   | ChannelPosSideRight           -- ^ side right
   | ChannelPosRearCenter          -- ^ rear center
   | ChannelPosFrontLeftCenter     -- ^ front left center
   | ChannelPosFrontRightCenter    -- ^ front right center
   | ChannelPosRearLeftCenter      -- ^ rear left center
   | ChannelPosRearRightCenter     -- ^ rear right center
   | ChannelPosFrontLeftWide       -- ^ front left wide
   | ChannelPosFrontRightWide      -- ^ front right wide
   | ChannelPosFrontLeftHigh       -- ^ front left high
   | ChannelPosFrontCenterHigh     -- ^ front center high
   | ChannelPosFrontRightHigh      -- ^ front right high
   | ChannelPosTopCenter           -- ^ top center
   | ChannelPosTopFrontLeft        -- ^ top front left
   | ChannelPosTopFrontRight       -- ^ top front right
   | ChannelPosTopFrontCenter      -- ^ top front center
   | ChannelPosTopRearLeft         -- ^ top rear left
   | ChannelPosTopRearRight        -- ^ top rear right
   | ChannelPosTopRearCenter       -- ^ top rear center
   | ChannelPosTopFrontLeftCenter  -- ^ top front left center
   | ChannelPosTopFrontRightCenter -- ^ top front right center
   | ChannelPosTopSideLeft         -- ^ top side left
   | ChannelPosTopSideRight        -- ^ top side right
   | ChannelPosLeftLFE             -- ^ left LFE
   | ChannelPosRightLFE            -- ^ right LFE
   | ChannelPosBottomCenter        -- ^ bottom center
   | ChannelPosBottomLeftCenter    -- ^ bottom left center
   | ChannelPosBottomRightCenter   -- ^ bottom right center
   deriving (Show,Eq,Enum)

data ChannelOption
   = ChannelPhaseInverse
   | ChannelDriverSpec
   deriving (Show,Eq,CBitSet)

instance Enum ChannelOption where
   fromEnum x = case x of
      ChannelPhaseInverse -> 16
      ChannelDriverSpec   -> 17
   toEnum x = case x of
      16 -> ChannelPhaseInverse
      17 -> ChannelDriverSpec
      _  -> error "Unknown channel option"        

pcmIoctl :: Word8 -> FileDescriptor -> SysRet ()
pcmIoctl n = ioctlSignal 0x41 n (0 :: Int)

pcmIoctlWR :: Storable a => Word8 -> a -> FileDescriptor -> SysRet a
pcmIoctlWR = ioctlWriteRead 0x41

pcmIoctlW :: Storable a => Word8 -> a -> FileDescriptor -> SysRet ()
pcmIoctlW = ioctlWrite 0x41

pcmIoctlR :: Storable a => Word8 -> FileDescriptor -> SysRet a
pcmIoctlR = ioctlRead 0x41

ioctlPcmVersion :: FileDescriptor -> SysRet Int
ioctlPcmVersion = pcmIoctlR 0x00

ioctlPcmInfo :: FileDescriptor -> SysRet PcmInfo
ioctlPcmInfo = pcmIoctlR 0x01

ioctlPcmTimeStamp :: Int -> FileDescriptor -> SysRet ()
ioctlPcmTimeStamp = pcmIoctlW 0x02

ioctlPcmTTimeStamp :: Int -> FileDescriptor -> SysRet ()
ioctlPcmTTimeStamp = pcmIoctlW 0x03

ioctlPcmHwRefine :: PcmHwParams -> FileDescriptor -> SysRet PcmHwParams
ioctlPcmHwRefine = pcmIoctlWR 0x10

ioctlPcmHwParams :: PcmHwParams -> FileDescriptor -> SysRet PcmHwParams
ioctlPcmHwParams = pcmIoctlWR 0x11

ioctlPcmHwFree :: FileDescriptor -> SysRet ()
ioctlPcmHwFree = pcmIoctl 0x12

ioctlPcmSwParams :: PcmSwParams -> FileDescriptor -> SysRet PcmSwParams
ioctlPcmSwParams = pcmIoctlWR 0x13

ioctlPcmStatus :: FileDescriptor -> SysRet PcmStatus
ioctlPcmStatus = pcmIoctlR 0x20

ioctlPcmDelay :: FileDescriptor -> SysRet Int64
ioctlPcmDelay = pcmIoctlR 0x21

ioctlPcmHwSync :: FileDescriptor -> SysRet ()
ioctlPcmHwSync = pcmIoctl 0x22

ioctlPcmSyncPtr :: PcmSyncPtr -> FileDescriptor -> SysRet PcmSyncPtr
ioctlPcmSyncPtr = pcmIoctlWR 0x23

ioctlPcmStatusExt :: PcmStatus -> FileDescriptor -> SysRet PcmStatus
ioctlPcmStatusExt = pcmIoctlWR 0x24

ioctlPcmChannelInfo :: FileDescriptor -> SysRet PcmChannelInfo
ioctlPcmChannelInfo = pcmIoctlR 0x32

ioctlPcmPrepare :: FileDescriptor -> SysRet ()
ioctlPcmPrepare = pcmIoctl 0x40

ioctlPcmReset :: FileDescriptor -> SysRet ()
ioctlPcmReset = pcmIoctl 0x41

ioctlPcmStart :: FileDescriptor -> SysRet ()
ioctlPcmStart = pcmIoctl 0x42

ioctlPcmDrop :: FileDescriptor -> SysRet ()
ioctlPcmDrop = pcmIoctl 0x43

ioctlPcmDrain :: FileDescriptor -> SysRet ()
ioctlPcmDrain = pcmIoctl 0x44

ioctlPcmPause :: Int -> FileDescriptor -> SysRet ()
ioctlPcmPause = pcmIoctlW 0x45

ioctlPcmRewind :: Word64 -> FileDescriptor -> SysRet ()
ioctlPcmRewind = pcmIoctlW 0x46

ioctlPcmResume :: FileDescriptor -> SysRet ()
ioctlPcmResume = pcmIoctl 0x47

ioctlPcmXRun :: FileDescriptor -> SysRet ()
ioctlPcmXRun = pcmIoctl 0x48

ioctlPcmForward :: Word64 -> FileDescriptor -> SysRet ()
ioctlPcmForward = pcmIoctlW 0x49

ioctlPcmWriteIFrames :: XferI -> FileDescriptor -> SysRet ()
ioctlPcmWriteIFrames = pcmIoctlW 0x50

ioctlPcmReadIFrames :: FileDescriptor -> SysRet XferI
ioctlPcmReadIFrames = pcmIoctlR 0x51

ioctlPcmWriteNFrames :: XferN -> FileDescriptor -> SysRet ()
ioctlPcmWriteNFrames = pcmIoctlW 0x52

ioctlPcmReadNFrames :: FileDescriptor -> SysRet XferN
ioctlPcmReadNFrames = pcmIoctlR 0x53

ioctlPcmLink :: Int -> FileDescriptor -> SysRet ()
ioctlPcmLink = pcmIoctlW 0x60

ioctlPcmUnlink :: FileDescriptor -> SysRet ()
ioctlPcmUnlink = pcmIoctl 0x61


-----------------------------------------------------------------------------
-- Raw MIDI interface - /dev/snd/midi*
-----------------------------------------------------------------------------

midiVersion :: Word32
midiVersion = 0x00020000

data MidiStream
   = MidiStreamOutput
   | MidiStreamInput
   deriving (Show,Eq,Enum)


data MidiFlag
   = MidiFlagOutput
   | MidiFlagInput
   | MidiFlagDuplex
   deriving (Show,Eq,Enum,CBitSet)

type MidiFlags = BitSet Word MidiFlag

data MidiInfo = MidiInfo
   { midiInfoDevice         :: Word            -- ^ RO/WR (control): device number
   , midiInfoSubDevice      :: Word            -- ^ RO/WR (control): subdevice number
   , midiInfoStream         :: Int             -- ^ WR: stream
   , midiInfoCard           :: Int             -- ^ R: card number
   , midiInfoFlags          :: MidiFlags       -- ^ SNDRV_RAWMIDI_INFO_XXXX
   , midiInfoId             :: Vector 64 CChar -- ^ ID (user selectable)
   , midiInfoName           :: Vector 80 CChar -- ^ name of device
   , midiInfoSubName        :: Vector 32 CChar -- ^ name of active or selected subdevice
   , midiInfoSubDeviceCount :: Word
   , midiInfoSubDeviceAvail :: Word
   , midiInfoReserved       :: Vector 64 Word8 -- ^ reserved for future use
   } deriving (Show, Generic, CStorable)

instance Storable MidiInfo where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data MidiParams = MidiParams
   { midiParamsStream          :: Int
   , midiParamsBufferSize      :: CSize           -- ^ queue size in bytes
   , midiParamsAvailMin        :: CSize           -- ^ minimum avail bytes for wakeup
   , midiParamsNoActiveSensing :: Word            -- ^ do not send active sensing byte in close()
   , midiParamsReserved        :: Vector 16 Word8 -- ^ reserved for future use
   } deriving (Show, Generic, CStorable)

instance Storable MidiParams where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data MidiStatus = MidiStatus
   { midiStatusStream    :: Int
   , midiStatusTimeStamp :: TimeSpec        -- ^ Timestamp
   , midiStatusAvail     :: CSize           -- ^ available bytes
   , midiStatusXRuns     :: CSize           -- ^ count of overruns since last status (in bytes)
   , midiStatusReserved  :: Vector 16 Word8 -- ^ reserved for future use
   } deriving (Show, Generic, CStorable)

instance Storable MidiStatus where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

midiIoctlW :: Storable a => Word8 -> a -> FileDescriptor -> SysRet ()
midiIoctlW = ioctlWrite 0x57

midiIoctlR :: Storable a => Word8 -> FileDescriptor -> SysRet a
midiIoctlR = ioctlRead 0x57

midiIoctlWR :: Storable a => Word8 -> a -> FileDescriptor -> SysRet a
midiIoctlWR = ioctlWriteRead 0x57


ioctlMidiVersion :: FileDescriptor -> SysRet Int
ioctlMidiVersion = midiIoctlR 0x00

ioctlMidiInfo :: FileDescriptor -> SysRet MidiInfo
ioctlMidiInfo = midiIoctlR 0x01

ioctlMidiParams :: MidiParams -> FileDescriptor -> SysRet MidiParams
ioctlMidiParams = midiIoctlWR 0x10

ioctlMidiStatus :: MidiStatus -> FileDescriptor -> SysRet MidiStatus
ioctlMidiStatus = midiIoctlWR 0x20

ioctlMidiDrop :: Int -> FileDescriptor -> SysRet ()
ioctlMidiDrop = midiIoctlW 0x30

ioctlMidiDrain :: Int -> FileDescriptor -> SysRet ()
ioctlMidiDrain = midiIoctlW 0x31


-----------------------------------------------------------------------------
-- Timer section - /dev/snd/timer
-----------------------------------------------------------------------------

timerVersion :: Word32
timerVersion = 0x00020006

data TimerClass
   = TimerClassNone
   | TimerClassSlave
   | TimerClassGlobal
   | TimerClassCard
   | TimerClassPCM
   deriving (Show,Eq)

instance Enum TimerClass where
   fromEnum x = case x of
      TimerClassNone   -> -1
      TimerClassSlave  -> 0
      TimerClassGlobal -> 1
      TimerClassCard   -> 2
      TimerClassPCM    -> 3
   toEnum x = case x of
      -1 -> TimerClassNone
      0  -> TimerClassSlave
      1  -> TimerClassGlobal
      2  -> TimerClassCard
      3  -> TimerClassPCM
      _  -> error "Unknown timer class"


-- | Slave timer classes
data TimerSlaveClass
   = TimerSlaveClassNone
   | TimerSlaveClassApplication
   | TimerSlaveClassSequencer       -- ^ alias
   | TimerSlaveClassOssSequencer    -- ^ alias
   deriving (Show,Eq,Enum)

-- | Global timers (device member)
data TimerGlobal
   = TimerGlobalSystem
   | TimerGlobalRTC
   | TimerGlobalHPET
   | TimerGlobalHRTimer
   deriving (Show,Eq,Enum)

-- | Timer info flags
data TimerFlag
   = TimerFlagSlave  -- ^ Cannot be controlled
   deriving (Show,Eq,Enum,CBitSet)

type TimerFlags = BitSet Word TimerFlag


data TimerId = TimerId
   { timerIdDeviceClass    :: Int
   , timerIdDeviceSubClass :: Int
   , timerIdCard           :: Int
   , timerIdDevice         :: Int
   , timerIdSubDevice      :: Int
   } deriving (Show,Generic,CStorable)

instance Storable TimerId where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf


data TimerGInfo = TimerGInfo
   { timerGInfoTimerID       :: TimerId         -- ^ requested timer ID
   , timerGInfoFlags         :: TimerFlags      -- ^ timer flags
   , timerGInfoCard          :: Int             -- ^ card number
   , timerGInfoId            :: Vector 64 CChar -- ^ timer identification
   , timerGInfoName          :: Vector 80 CChar -- ^ timer name
   , timerGInfoReserved      :: Word64          -- ^ reserved for future use
   , timerGInfoResolution    :: Word64          -- ^ average period resolution in ns
   , timerGInfoResolutionMin :: Word64          -- ^ minimal period resolution in ns
   , timerGInfoResolutionMax :: Word64          -- ^ maximal period resolution in ns
   , timerGInfoClients       :: Word            -- ^ active timer clients
   , timerGInfoReserved2     :: Vector 32 Word8
   } deriving (Show,Generic,CStorable)

instance Storable TimerGInfo where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data TimerGParams = TimerGParams
   { timerGParamsTimerId          :: TimerId        -- ^ requested timer ID
   , timerGParamsPeriodNumerator  :: Word64         -- ^ requested precise period duration (in seconds) - numerator
   , timerGParamsPeriodDenomintor :: Word64         -- ^ requested precise period duration (in seconds) - denominator
   , timerGParamsReserved         :: Vector 32 Word8
   } deriving (Show,Generic,CStorable)

instance Storable TimerGParams where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data TimerGStatus = TimerGStatus
   { timerGStatusTimerId               :: TimerId         -- ^ requested timer ID
   , timerGStatusResolution            :: Word64          -- ^ current period resolution in ns
   , timerGStatusResolutionNumerator   :: Word64          -- ^ precise current period resolution (in seconds) - numerator
   , timerGStatusResolutionDenominator :: Word64          -- ^ precise current period resolution (in seconds) - denominator
   , timerGStatusReserved              :: Vector 32 Word8
   } deriving (Show,Generic,CStorable)

instance Storable TimerGStatus where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf


data TimerSelect = TimerSelect
   { timerSelectTimerId  :: TimerId         -- ^ bind to timer ID
   , timerSelectReserved :: Vector 32 Word8 -- ^ reserved
   } deriving (Show,Generic,CStorable)

instance Storable TimerSelect where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf


data TimerInfo = TimerInfo
   { timerInfoFlags         :: TimerFlags      -- ^ timer flags
   , timerInfoCard          :: Int             -- ^ card number
   , timerInfoId            :: Vector 64 CChar -- ^ timer identification
   , timerInfoName          :: Vector 80 CChar -- ^ timer name
   , timerInfoReserved      :: Word64          -- ^ reserved for future use
   , timerInfoResolution    :: Word64          -- ^ average period resolution in ns
   , timerInfoReserved2     :: Vector 64 Word8
   } deriving (Show,Generic,CStorable)

instance Storable TimerInfo where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf


data TimerParamsFlag
   = TimerParamFlagAuto       -- ^ auto start, otherwise one-shot 
   | TimerParamFlagExclusive  -- ^ exclusive use, precise start/stop/pause/continue 
   | TimerParamFlagEarlyEvent -- ^ write early event to the poll queue 
   deriving (Show,Eq,Enum,CBitSet)

type TimerParamsFlags = BitSet Word TimerParamsFlag

data TimerParams = TimerParams
   { timerParamsFlags     :: TimerParamsFlags -- ^ flags - SNDRV_MIXER_PSFLG_*
   , timerParamsTicks     :: Word             -- ^ requested resolution in ticks
   , timerParamsQueueSize :: Word             -- ^ total size of queue (32-1024)
   , timerParamsReserved  :: Word             -- ^ reserved, was: failure locations
   , timerParamsFilter    :: Word             -- ^ event filter (bitmask of SNDRV_TIMER_EVENT_*)
   , timerParamsReserved2 :: Vector 60 Word8  -- ^ reserved
   } deriving (Show,Generic,CStorable)

instance Storable TimerParams where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data TimerStatus = TimerStatus
   { timerStatusTimeStamp  :: TimeSpec        -- ^ Timestamp - last update
   , timerStatusResolution :: Word            -- ^ current period resolution in ns
   , timerStatusLost       :: Word            -- ^ counter of master tick lost
   , timerStatusOverrun    :: Word            -- ^ count of read queue overruns
   , timerStatusQueue      :: Word            -- ^ used queue size
   , timerStatusReserved   :: Vector 64 Word8 -- ^ reserved
   } deriving (Show,Generic,CStorable)

instance Storable TimerStatus where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

timerIoctl :: Word8 -> FileDescriptor -> SysRet ()
timerIoctl n = ioctlSignal 0x54 n (0 :: Int)

timerIoctlW :: Storable a => Word8 -> a -> FileDescriptor -> SysRet ()
timerIoctlW = ioctlWrite 0x54

timerIoctlR :: Storable a => Word8 -> FileDescriptor -> SysRet a
timerIoctlR = ioctlRead 0x54

timerIoctlWR :: Storable a => Word8 -> a -> FileDescriptor -> SysRet a
timerIoctlWR = ioctlWriteRead 0x54

ioctlTimerVersion :: FileDescriptor -> SysRet Int
ioctlTimerVersion = timerIoctlR 0x00

ioctlTimerNextDevice :: TimerId -> FileDescriptor -> SysRet TimerId
ioctlTimerNextDevice = timerIoctlWR 0x01

ioctlTimerTRead :: Int -> FileDescriptor -> SysRet ()
ioctlTimerTRead = timerIoctlW 0x02

ioctlTimerGInfo :: TimerGInfo -> FileDescriptor -> SysRet TimerGInfo
ioctlTimerGInfo = timerIoctlWR 0x03

ioctlTimerGParams :: TimerGParams -> FileDescriptor -> SysRet ()
ioctlTimerGParams = timerIoctlW 0x04

ioctlTimerGStatus :: TimerGStatus -> FileDescriptor -> SysRet TimerGStatus
ioctlTimerGStatus = timerIoctlWR 0x05

ioctlTimerSelect :: TimerSelect -> FileDescriptor -> SysRet ()
ioctlTimerSelect = timerIoctlW 0x10

ioctlTimerInfo :: FileDescriptor -> SysRet TimerInfo
ioctlTimerInfo = timerIoctlR 0x11

ioctlTimerParams :: TimerParams -> FileDescriptor -> SysRet ()
ioctlTimerParams = timerIoctlW 0x12

ioctlTimerStatus :: FileDescriptor -> SysRet TimerStatus
ioctlTimerStatus = timerIoctlR 0x14

ioctlTimerStart :: FileDescriptor -> SysRet ()
ioctlTimerStart = timerIoctl 0xa0

ioctlTimerStop :: FileDescriptor -> SysRet ()
ioctlTimerStop = timerIoctl 0xa1

ioctlTimerContinue :: FileDescriptor -> SysRet ()
ioctlTimerContinue = timerIoctl 0xa2

ioctlTimerPause :: FileDescriptor -> SysRet ()
ioctlTimerPause = timerIoctl 0xa3

data TimerRead = TimerRead
   { timerReadResolution :: Word
   , timerReadTicks      :: Word
   } deriving (Show,Generic,CStorable)

instance Storable TimerRead where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data TimerEvent
   = TimerEventResolution        -- ^ val = resolution in ns 
   | TimerEventTick              -- ^ val = ticks 
   | TimerEventStart             -- ^ val = resolution in ns 
   | TimerEventStop              -- ^ val = 0 
   | TimerEventContinue          -- ^ val = resolution in ns 
   | TimerEventPause             -- ^ val = 0 
   | TimerEventEarly             -- ^ val = 0, early event 
   | TimerEventSuspend           -- ^ val = 0 
   | TimerEventResume            -- ^ val = resolution in ns 
   -- master timer events for slave timer instances
   | TimerEventMasterStart
   | TimerEventMasterStop
   | TimerEventMasterContinue
   | TimerEventMasterPause
   | TimerEventMasterSuspend
   | TimerEventMasterResume
   deriving (Show,Eq)

instance Enum TimerEvent where
   fromEnum x = case x of
      TimerEventResolution       -> 0
      TimerEventTick             -> 1
      TimerEventStart            -> 2
      TimerEventStop             -> 3
      TimerEventContinue         -> 4
      TimerEventPause            -> 5
      TimerEventEarly            -> 6
      TimerEventSuspend          -> 7
      TimerEventResume           -> 8
      TimerEventMasterStart      -> 12
      TimerEventMasterStop       -> 13
      TimerEventMasterContinue   -> 14
      TimerEventMasterPause      -> 15
      TimerEventMasterSuspend    -> 17
      TimerEventMasterResume     -> 18
   toEnum x = case x of
      0  -> TimerEventResolution
      1  -> TimerEventTick
      2  -> TimerEventStart
      3  -> TimerEventStop
      4  -> TimerEventContinue
      5  -> TimerEventPause
      6  -> TimerEventEarly
      7  -> TimerEventSuspend
      8  -> TimerEventResume
      12 -> TimerEventMasterStart
      13 -> TimerEventMasterStop
      14 -> TimerEventMasterContinue
      15 -> TimerEventMasterPause
      17 -> TimerEventMasterSuspend
      18 -> TimerEventMasterResume
      _  -> error "Unknown timer event"

data TimerTRead = TimerTRead
   { timerTReadEvent     :: Int
   , timerTReadTimeStamp :: TimeSpec
   , timerTReadValue     :: Word
   } deriving (Show,Generic,CStorable)

instance Storable TimerTRead where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

-----------------------------------------------------------------------------
-- Driver control interface - /dev/snd/control*
-----------------------------------------------------------------------------

controlVersion :: Word32
controlVersion = 0x00020007

data ControlCardInfo = ControlCardInfo
   { controlCardInfoCard       :: Int              -- ^ card number
   , controlCardInfoPad        :: Int              -- ^ reserved for future (was type)
   , controlCardInfoId         :: Vector 16 CChar  -- ^ ID of card (user selectable)
   , controlCardInfoDriver     :: Vector 16 CChar  -- ^ Driver name
   , controlCardInfoName       :: Vector 32 CChar  -- ^ Short name of soundcard
   , controlCardInfoLongName   :: Vector 80 CChar  -- ^ name + info text about soundcard
   , controlCardInfoReserved   :: Vector 16 Word8  -- ^ reserved for future (was ID of mixer)
   , controlCardInfoMixerName  :: Vector 80 CChar  -- ^ visual mixer identification
   , controlCardInfoComponents :: Vector 128 CChar -- ^ card components / fine identification, delimited with one space (AC97 etc..)
   } deriving (Show,Generic,CStorable)

instance Storable ControlCardInfo where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data ControlElementType
   = ControlElemNone
   | ControlElemBoolean
   | ControlElemInteger
   | ControlElemEnumerated
   | ControlElemBytes
   | ControlElemIec958
   | ControlElemInteger64
   deriving (Show,Eq, Enum)


data ControlElementInterface
   = ControlElemInterfaceCard
   | ControlElemInterfaceHwDep
   | ControlElemInterfaceMixer
   | ControlElemInterfacePCM
   | ControlElemInterfaceMidi
   | ControlElemInterfaceTimer
   | ControlElemInterfaceSequencer
   deriving (Show,Eq,Enum)

data ControlElementAccess
   = ControlElemAccessRead
   | ControlElemAccessWrite
   | ControlElemAccessVolatile    -- ^ control value may be changed without a notification
   | ControlElemAccessTimeStamp   -- ^ when was control changed
   | ControlElemAccessTlvRead     -- ^ TLV read is possible
   | ControlElemAccessTlvWrite    -- ^ TLV write is possible
   | ControlElemAccessTlvCommand  -- ^ TLV command is possible 
   | ControlElemAccessInactive    -- ^ control does actually nothing, but may be updated 
   | ControlElemAccessLock        -- ^ write lock 
   | ControlElemAccessOwner       -- ^ write lock owner 
   | ControlElemAccessTlvCallBack -- ^ kernel use a TLV callback 
   | ControlElemAccessUser        -- ^ user space element 
   deriving (Show,Eq,Enum)

instance CBitSet ControlElementAccess where
   toBitOffset x = case x of
      ControlElemAccessInactive     -> 8
      ControlElemAccessLock         -> 9
      ControlElemAccessOwner        -> 10
      ControlElemAccessTlvCallBack  -> 28
      ControlElemAccessUser         -> 29
      _                             -> fromEnum x
   fromBitOffset x = case x of
      8  -> ControlElemAccessInactive
      9  -> ControlElemAccessLock
      10 -> ControlElemAccessOwner
      28 -> ControlElemAccessTlvCallBack
      29 -> ControlElemAccessUser
      _  -> toEnum x

type ControlElementAccesses = BitSet Word32 ControlElementAccess

-- for further details see the ACPI and PCI power management specification

data ControlPower
   = ControlPowerD0     -- ^ full On
   | ControlPowerD1     -- ^ partial On
   | ControlPowerD2     -- ^ partial On
   | ControlPowerD3hot  -- ^ Off, with power
   | ControlPowerD3cold -- ^ Off, without power
   deriving (Show,Eq)

instance Enum ControlPower where
   fromEnum x = case x of
      ControlPowerD0     -> 0x0000
      ControlPowerD1     -> 0x0100
      ControlPowerD2     -> 0x0200
      ControlPowerD3hot  -> 0x0300
      ControlPowerD3cold -> 0x0301
   toEnum x = case x of
      0x0000 -> ControlPowerD0
      0x0100 -> ControlPowerD1
      0x0200 -> ControlPowerD2
      0x0300 -> ControlPowerD3hot
      0x0301 -> ControlPowerD3cold
      _      -> error "Unknown control power"

data ControlElementId = ControlElementId
   { controlElemIdNumId     :: Int             -- ^ numeric identifier, zero = invalid
   , controlElemIdInterface :: Int             -- ^ interface identifier
   , controlElemIdDevice    :: Word            -- ^ device/client number
   , controlElemIdSubDevice :: Word            -- ^ subdevice (substream) number
   , controlElemIdName      :: Vector 44 CChar -- ^ ASCII name of item
   , controlElemIdIndex     :: Word            -- ^ index of item
   } deriving (Show,Generic,CStorable)

instance Storable ControlElementId where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf


data ControlElementList = ControlElementList
   { controlElemListOffset   :: Word            -- ^ W: first element ID to get
   , controlElemListSpace    :: Word            -- ^ W: count of element IDs to get
   , controlElemListUsed     :: Word            -- ^ R: count of element IDs set
   , controlElemListCount    :: Word            -- ^ R: count of all elements
   , controlElemListPids     :: Ptr ()          -- ^ R: IDs
   , controlElemListReserved :: Vector 50 Word8
   } deriving (Show,Generic,CStorable)

instance Storable ControlElementList where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf


data IntegerValue = IntegerValue
   { integerValueMin  :: Word32 -- ^ R: minimum value
   , integerValueMax  :: Word32 -- ^ R: maximum value
   , integerValueStep :: Word32 -- ^ R: step (0 variable)
   } deriving (Show,Generic,CStorable)

instance Storable IntegerValue where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data Integer64Value = Integer64Value
   { integer64ValueMin  :: Word64 -- ^ R: minimum value
   , integer64ValueMax  :: Word64 -- ^ R: maximum value
   , integer64ValueStep :: Word64 -- ^ R: step (0 variable)
   } deriving (Show,Generic,CStorable)

instance Storable Integer64Value where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data EnumeratedValue = EnumeratedValue
   { enumeratedCount       :: Word            -- ^ R: number of items
   , enumeratedItem        :: Word            -- ^ W: item number
   , enumeratedName        :: Vector 64 CChar -- ^ R: value name
   , enumeratedNamesPtr    :: Word64          -- ^ W: names list (ELEM_ADD only)
   , enumeratedNamesLength :: Word
   } deriving (Show,Generic,CStorable)

instance Storable EnumeratedValue where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data ControlElementInfo = ControlElementInfo
   { controlElemInfoId :: ControlElementId
   , controlElemInfoType :: Int
   , controlElemInfoAccess :: Word
   , controlElemInfoCount :: Word
   , controlElemInfoOwner :: ProcessID
   , controlElemInfoValue :: Union '[IntegerValue,Integer64Value,EnumeratedValue,Vector 128 Word8]
   , controlElemInfoDimensions :: Vector 4 Word16
   } deriving (Show,Generic,CStorable)


instance Storable ControlElementInfo where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

data ControlElementValue = ControlElementValue
   { controlElemValueId        :: ControlElementId                    -- ^ W: element ID
   , controlElemValueIndirect  :: Word                                -- ^ W: indirect access - obsoleted
   , controlElemValueValue     :: Union '[Vector 512 Word8,AesIec958]
   , controlElemValueTimeStamp :: TimeSpec
   , controlElemValueReserved  :: Vector 112 Word8
   } deriving (Show,Generic,CStorable)

instance Storable ControlElementValue where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf


data ControlTLV = ControlTLV
   { controlTlvNumId  :: Word          -- ^ control element numeric identification
   , controlTlvLength :: Word          -- ^ in bytes aligned to 4
   , controlTlvTlv    :: Vector 0 Word -- ^ first TLV
   -- FIXME: the array is allocated "after" the struct...
   } deriving (Show,Generic,CStorable)

instance Storable ControlTLV where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

controlIoctlW :: Storable a => Word8 -> a -> FileDescriptor -> SysRet ()
controlIoctlW = ioctlWrite 0x55

controlIoctlR :: Storable a => Word8 -> FileDescriptor -> SysRet a
controlIoctlR = ioctlRead 0x55

controlIoctlWR :: Storable a => Word8 -> a -> FileDescriptor -> SysRet a
controlIoctlWR = ioctlWriteRead 0x55

ioctlControlVersion :: FileDescriptor -> SysRet Int
ioctlControlVersion = controlIoctlR 0x00

ioctlControlCardInfo :: FileDescriptor -> SysRet ControlCardInfo
ioctlControlCardInfo = controlIoctlR 0x01

ioctlControlElemList :: ControlElementList -> FileDescriptor -> SysRet ControlElementList
ioctlControlElemList = controlIoctlWR 0x10

ioctlControlElemInfo :: ControlElementInfo -> FileDescriptor -> SysRet ControlElementInfo
ioctlControlElemInfo = controlIoctlWR 0x11

ioctlControlElemRead :: ControlElementValue -> FileDescriptor -> SysRet ControlElementValue
ioctlControlElemRead = controlIoctlWR 0x12

ioctlControlElemWrite :: ControlElementValue -> FileDescriptor -> SysRet ControlElementValue
ioctlControlElemWrite = controlIoctlWR 0x13

ioctlControlElemLock :: ControlElementId -> FileDescriptor -> SysRet ()
ioctlControlElemLock = controlIoctlW 0x14

ioctlControlElemUnlock :: ControlElementId -> FileDescriptor -> SysRet ()
ioctlControlElemUnlock = controlIoctlW 0x15

ioctlControlSubscribeEvents :: Int -> FileDescriptor -> SysRet Int
ioctlControlSubscribeEvents = controlIoctlWR 0x16

ioctlControlElemAdd :: ControlElementInfo -> FileDescriptor -> SysRet ControlElementInfo
ioctlControlElemAdd = controlIoctlWR 0x17

ioctlControlElemReplace :: ControlElementInfo -> FileDescriptor -> SysRet ControlElementInfo
ioctlControlElemReplace = controlIoctlWR 0x18

ioctlControlElemRemove :: ControlElementInfo -> FileDescriptor -> SysRet ControlElementInfo
ioctlControlElemRemove = controlIoctlWR 0x19

ioctlControlTLVRead :: ControlTLV -> FileDescriptor -> SysRet ControlTLV
ioctlControlTLVRead = controlIoctlWR 0x1a

ioctlControlTLVWrite :: ControlTLV -> FileDescriptor -> SysRet ControlTLV
ioctlControlTLVWrite = controlIoctlWR 0x1b

ioctlControlTLVCommand :: ControlTLV -> FileDescriptor -> SysRet ControlTLV
ioctlControlTLVCommand = controlIoctlWR 0x1c

ioctlControlHwDepNextDevice :: Int -> FileDescriptor -> SysRet Int
ioctlControlHwDepNextDevice = controlIoctlWR 0x20

ioctlControlHwInfo :: FileDescriptor -> SysRet HwInfo
ioctlControlHwInfo = controlIoctlR 0x21

ioctlControlPcmNextDevice :: FileDescriptor -> SysRet Int
ioctlControlPcmNextDevice = controlIoctlR 0x30

ioctlControlPcmInfo :: PcmInfo -> FileDescriptor -> SysRet PcmInfo
ioctlControlPcmInfo = controlIoctlWR 0x31

ioctlControlPcmPreferSubdevice :: Int -> FileDescriptor -> SysRet ()
ioctlControlPcmPreferSubdevice = controlIoctlW 0x32

ioctlControlMidiNextDevice :: Int -> FileDescriptor -> SysRet Int
ioctlControlMidiNextDevice = controlIoctlWR 0x40

ioctlControlMidiInfo :: MidiInfo -> FileDescriptor -> SysRet MidiInfo
ioctlControlMidiInfo = controlIoctlWR 0x41

ioctlControlMidiPreferSubdevice :: Int -> FileDescriptor -> SysRet ()
ioctlControlMidiPreferSubdevice = controlIoctlW 0x42

ioctlControlPower :: Int -> FileDescriptor -> SysRet Int
ioctlControlPower = controlIoctlWR 0xd0

ioctlControlPowerState :: FileDescriptor -> SysRet Int
ioctlControlPowerState = controlIoctlR 0xd1


-----------------------------------------------------------------------------
-- Read interface
-----------------------------------------------------------------------------

data ControlEventType
   = ControlEventTypeElem
   deriving (Show,Eq,Enum)

data ControlEventMask
   = ControlEventMaskValue    -- ^ element value was changed
   | ControlEventMaskInfo     -- ^ element info was changed 
   | ControlEventMaskAdd      -- ^ element was added 
   | ControlEventMaskTLV      -- ^ element TLV tree was changed 
   | ControlEventMaskRemove   -- ^ element was removed 
   deriving (Show,Eq)

instance Enum ControlEventMask where
   fromEnum x = case x of
      ControlEventMaskValue   -> 1
      ControlEventMaskInfo    -> 2
      ControlEventMaskAdd     -> 4
      ControlEventMaskTLV     -> 8
      ControlEventMaskRemove  -> complement 0
   toEnum x = case x of
      1                     -> ControlEventMaskValue
      2                     -> ControlEventMaskInfo
      4                     -> ControlEventMaskAdd
      8                     -> ControlEventMaskTLV
      v | v == complement 0 -> ControlEventMaskRemove
      _                     -> error "Unknown event mask"

data ControlEvent = ControlEvent
   { controlEventType   :: Int
   , controlEventMask   :: Word
   , controlEventElemId :: ControlElementId
   } deriving (Show,Generic,CStorable)

instance Storable ControlEvent where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf
