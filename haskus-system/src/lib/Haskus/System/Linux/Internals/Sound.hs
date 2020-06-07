{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskus.System.Linux.Internals.Sound
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
   , PcmSubClass (..)
   , PcmStream (..)
   , PcmAccess (..)
   , PcmFormat (..)
   , PcmSubFormat (..)
   , PcmInfoFlag (..)
   , PcmInfoFlags
   , PcmState (..)
   , PcmInfo (..)
   , PcmHwParam (..)
   , PcmHwParamMask (..)
   , PcmHwParamInterval (..)
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

import Haskus.Utils.Types.Generics (Generic)
import Haskus.Utils.Types hiding (Union)
import Haskus.Utils.Flow
import Haskus.Binary.Vector (Vector, vectorReverse)
import Haskus.Binary.Union
import Haskus.Number.Word
import Haskus.Number.Int
import Haskus.Binary.CTypes
import Foreign.Ptr
import Haskus.Binary.BitSet
import Haskus.Binary.Enum
import Haskus.Binary.Bits
import Haskus.Binary.Storable
import Haskus.Format.String
import Haskus.System.Linux.Internals.Error
import Haskus.System.Linux.Ioctl
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Time
import Haskus.System.Linux.Process (ProcessID)

-- =============================================================
--    From linux/include/uapi/sound/asound.h
-- =============================================================

-----------------------------------------------------------------------------
-- Digital audio interface
-----------------------------------------------------------------------------

data AesIec958 = AesIec958
   { aesStatus      :: Vector 24 Word8  -- ^ AES/IEC958 channel status bits
   , aesSubcode     :: Vector 147 Word8 -- ^ AES/IEC958 subcode bits
   , aesPadding     :: CChar            -- ^ nothing
   , aesDigSubFrame :: Vector 4 Word8   -- ^ AES/IEC958 subframe bits
   } deriving (Generic, Show, Storable)

data Cea861AudioInfoFrame = Cea861AudioInfoFrame
   { ceaCodingTypeChannelCount :: Word8 -- ^ coding type and channel count
   , ceaSampleFrequencySize    :: Word8 -- ^ sample frequency and size
   , ceaUnused                 :: Word8 -- ^ not used, all zeros
   , ceaChannelAllocationCode  :: Word8 -- ^ channel allocation code
   , ceaDownmixLevelShift      :: Word8 -- ^ downmix inhibit & level-shift values
   } deriving (Generic, Show, Storable)

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
   { hwInfoDevice    :: Int32            -- ^ WR: device number
   , hwInfoCard      :: Int32            -- ^ R: card number
   , hwInfoId        :: CStringBuffer 64 -- ^ ID (user selectable)
   , hwInfoName      :: CStringBuffer 80 -- ^ hwdep name
   , hwInfoInterface :: Int32            -- ^ hwdep interface
   , hwInfoReserved  :: Vector 64 Word8  -- ^ reserved for future
   } deriving (Generic, Show, Storable)

-- | Generic DSP loader
data HwDspStatus = HwDspStatus
   { hwDspVersion    :: Word32           -- ^ R: driver-specific version
   , hwDspId         :: CStringBuffer 32 -- ^ R: driver-specific ID string
   , hwDspNumDsps    :: Word32           -- ^ R: number of DSP images to transfer
   , hwDspLoadedDsps :: Word32           -- ^ R: bit flags indicating the loaded DSPs
   , hwDspChipReady  :: Word32           -- ^ R: 1 = initialization finished
   , hwDspReserved   :: Vector 16 Word8  -- ^ reserved for future use
   } deriving (Generic, Show, Storable)

data HwDspImage = HwDspImage
   { hwDspImageIndex      :: Word32           -- ^ W: DSP index
   , hwDspImageName       :: CStringBuffer 64 -- ^ W: ID (e.g. file name)
   , hwDspImageBin        :: Ptr ()           -- ^ W: binary image
   , hwDspImageLength     :: CSize            -- ^ W: size of image in bytes
   , hwDspImageDriverData :: Word64           -- ^ W: driver-specific data
   } deriving (Generic, Show, Storable)

hwIoctlW :: (MonadInIO m, Storable a) => Word8 -> a -> Handle -> Excepts '[ErrorCode] m ()
hwIoctlW = ioctlWrite 0x48

hwIoctlR :: (MonadInIO m, Storable a) => Word8 -> Handle -> Excepts '[ErrorCode] m a
hwIoctlR = ioctlRead 0x48

ioctlHwVersion :: MonadInIO m => Handle -> Excepts '[ErrorCode] m Int32
ioctlHwVersion = hwIoctlR 0x00

ioctlHwInfo :: MonadInIO m => Handle -> Excepts '[ErrorCode] m HwInfo
ioctlHwInfo = hwIoctlR 0x01

ioctlHwDspStatus :: MonadInIO m => Handle -> Excepts '[ErrorCode] m HwDspStatus
ioctlHwDspStatus = hwIoctlR 0x02

ioctlHwDspLoad :: MonadInIO m => HwDspImage -> Handle -> Excepts '[ErrorCode] m ()
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
   deriving (Show,Eq,Enum,Bounded)


data PcmSubClass
   = PcmSubClassGenericMix -- ^ mono or stereo subdevices are mixed together 
   | PcmSubClassMultiMix   -- ^ multichannel subdevices are mixed together 
   deriving (Show,Eq,Enum,Bounded)

data PcmStream
   = PcmStreamPlayback
   | PcmStreamCapture
   deriving (Show,Eq,Enum,Bounded)

data PcmAccess
   = PcmAccessMmapInterleaved     -- ^ interleaved mmap
   | PcmAccessMmapNontInterleaved -- ^ noninterleaved m
   | PcmAccessMmapComplex         -- ^ complex mmap
   | PcmAccessRwInterleaved       -- ^ readi/writei
   | PcmAccessRwNonInterleaved    -- ^ readn/writen
   deriving (Show,Eq,Enum,Ord,BitOffset,Bounded)

-- | PCM sample format
--
-- (_snd_pcm_format in alsa/pcm.h)
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
   | PcmFormatS20_LE             -- ^ Signed 20bit Little Endian in 4bytes format, LSB justified
   | PcmFormatS20_BE             -- ^ Signed 20bit Big Endian in 4bytes format, LSB justified
   | PcmFormatU20_LE             -- ^ Unsigned 20bit Little Endian in 4bytes format, LSB justified
   | PcmFormatU20_BE             -- ^ Unsigned 20bit Big Endian in 4bytes format, LSB justified
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
   deriving (Show,Eq,Ord,Enum,Bounded)

instance BitOffset PcmFormat where
   toBitOffset x = case x of
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
      PcmFormatS20_LE             -> 25
      PcmFormatS20_BE             -> 26
      PcmFormatU20_LE             -> 27
      PcmFormatU20_BE             -> 28
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

   fromBitOffset x = case x of
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
    25 -> PcmFormatS20_LE
    26 -> PcmFormatS20_BE
    27 -> PcmFormatU20_LE
    28 -> PcmFormatU20_BE
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
    f  -> error ("Unknown PCM format: " ++ show f)

data PcmSubFormat
   = PcmSubFormatStd
   deriving (Show,Eq,Enum,Ord,BitOffset,Bounded)

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
   | PcmInfoJointDuplex              -- ^ playback and capture stream are somewhat correlated
   | PcmInfoSyncStart                -- ^ pcm support some kind of sync go
   | PcmInfoNoPeriodWakeUp           -- ^ period wakeup can be disabled
   | PcmInfoHasLinkAtime             -- ^ report hardware link audio time, reset on startup
   | PcmInfoHaskLinkAbsoluteAtime    -- ^ report absolute hardware link audio time, not reset on startup
   | PcmInfoHasLinkEstimatedAtime    -- ^ report estimated link audio time
   | PcmInfoHasLinkSynchronizedAtime -- ^ report synchronized audio/system time
   | PcmInfoDrainTrigger             -- ^ internal kernel flag - trigger in drain
   | PcmInfoFifoInFrames             -- ^ internal kernel flag - FIFO size is in frames
   deriving (Show,Eq,Enum,Ord,Bounded)

instance BitOffset PcmInfoFlag where
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
      PcmInfoJointDuplex              -> 21
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
      21 -> PcmInfoJointDuplex
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
   deriving (Show,Eq,Enum,CEnum,Bounded)

type PcmStateField = EnumField Int32 PcmState

data PcmMmapOffset
   = PcmMmapOffsetData
   | PcmMmapOffsetStatus
   | PcmMmapOffsetControl
   deriving (Show,Eq,Enum,Bounded)

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
   { pcmInfoDevice               :: Word32           -- ^ RO/WR (control): device number
   , pcmInfoSubDevice            :: Word32           -- ^ RO/WR (control): subdevice number
   , pcmInfoStream               :: Int32            -- ^ RO/WR (control): stream direction
   , pcmInfoCard                 :: Int32            -- ^ R: card number
   , pcmInfoID                   :: CStringBuffer 64 -- ^ ID (user selectable)
   , pcmInfoName                 :: CStringBuffer 80 -- ^ name of this device
   , pcmInfoSubName              :: CStringBuffer 32 -- ^ subdevice name
   , pcmInfoDevClass             :: Int32            -- ^ SNDRV_PCM_CLASS_*
   , pcmInfoDevSubClass          :: Int32            -- ^ SNDRV_PCM_SUBCLASS_*
   , pcmInfoSubDevicesCount      :: Word32
   , pcmInfoSubDevicesAvailabled :: Word32
   , pcmInfoSync                 :: Vector 16 Word8  -- ^ hardware synchronization ID
   , pcmInfoReserved             :: Vector 64 Word8  -- ^ reserved for future...
   } deriving (Generic, Show, Storable)

-- | Alsa manipulates two kinds of parameters: masks and intervals.
-- Most alsa-lib API handle both, but internally we need to know which one it is
-- to set field accordingly (e.g., "intervals" or "masks" fields in HwParams).
data PcmHwParam
   = PcmHwParamMask     PcmHwParamMask       -- ^ Mask-like parameter
   | PcmHwParamInterval PcmHwParamInterval   -- ^ Interval-like parameter
   deriving (Show,Eq)

-- | Mask-like parameter
data PcmHwParamMask
   = PcmHwParamAccess      -- ^ Access type
   | PcmHwParamFormat      -- ^ Format
   | PcmHwParamSubFormat   -- ^ Subformat
   deriving (Show,Eq,Enum)

-- | Interval-like parameter
data PcmHwParamInterval
   = PcmHwParamSampleBits  -- ^ Bits per sample 
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
   deriving (Show,Eq,Enum,Bounded)


instance Enum PcmHwParam where
   fromEnum x = case x of
      PcmHwParamMask     PcmHwParamAccess      -> 0
      PcmHwParamMask     PcmHwParamFormat      -> 1
      PcmHwParamMask     PcmHwParamSubFormat   -> 2
      PcmHwParamInterval PcmHwParamSampleBits  -> 8
      PcmHwParamInterval PcmHwParamFrameBits   -> 9
      PcmHwParamInterval PcmHwParamChannels    -> 10
      PcmHwParamInterval PcmHwParamRate        -> 11
      PcmHwParamInterval PcmHwParamPeriodTime  -> 12
      PcmHwParamInterval PcmHwParamPeriodSize  -> 13
      PcmHwParamInterval PcmHwParamPeriodBytes -> 14
      PcmHwParamInterval PcmHwParamPeriods     -> 15
      PcmHwParamInterval PcmHwParamBufferTime  -> 16
      PcmHwParamInterval PcmHwParamBufferSize  -> 17
      PcmHwParamInterval PcmHwParamBufferBytes -> 18
      PcmHwParamInterval PcmHwParamTickTime    -> 19
   toEnum x = case x of
      0  -> PcmHwParamMask     PcmHwParamAccess
      1  -> PcmHwParamMask     PcmHwParamFormat
      2  -> PcmHwParamMask     PcmHwParamSubFormat
      8  -> PcmHwParamInterval PcmHwParamSampleBits
      9  -> PcmHwParamInterval PcmHwParamFrameBits
      10 -> PcmHwParamInterval PcmHwParamChannels
      11 -> PcmHwParamInterval PcmHwParamRate
      12 -> PcmHwParamInterval PcmHwParamPeriodTime
      13 -> PcmHwParamInterval PcmHwParamPeriodSize
      14 -> PcmHwParamInterval PcmHwParamPeriodBytes
      15 -> PcmHwParamInterval PcmHwParamPeriods
      16 -> PcmHwParamInterval PcmHwParamBufferTime
      17 -> PcmHwParamInterval PcmHwParamBufferSize
      18 -> PcmHwParamInterval PcmHwParamBufferBytes
      19 -> PcmHwParamInterval PcmHwParamTickTime
      _  -> error "Unknown PCM HW Param"

-- | A parameter interval
data Interval = Interval
   { intervalMin     :: Word32
   , intervalMax     :: Word32
   , intervalOptions :: IntervalOptions
   } deriving (Show,Eq,Generic,Storable)

data IntervalOption
   = IntervalOpenMin -- ^ Is interval lower bound excluded?
   | IntervalOpenMax -- ^ Is interval upper bound excluded?
   | IntervalInteger -- ^ Does the interval contain only integer values?
   | IntervalEmpty   -- ^ Is the interval empty?
   deriving (Show,Eq,Enum,BitOffset)

type IntervalOptions = BitSet Word32 IntervalOption

data PcmHwParamsFlag
   = PcmHwParamsNoResample     -- ^ avoid rate resampling
   | PcmHwParamsExportBuffer   -- ^ export buffer
   | PcmHwParamsNoPeriodWakeUp -- ^ disable period wakeups
   deriving (Show,Eq,Enum,BitOffset)

type PcmHwParamsFlags = BitSet Word32 PcmHwParamsFlag

-- | A parameter set (or mask)
--
-- Word-order: left-to-right
-- Byte-order: native
-- Bit-order: right-to-left
newtype Mask
   = Mask (Vector 8 Word32)
   deriving stock (Show,Eq)
   deriving newtype (Bitwise)

instance StaticStorable Mask where
   type SizeOf    Mask       = SizeOf    (Vector 8 Word32)
   type Alignment Mask       = Alignment (Vector 8 Word32)
   staticPeekIO ptr          = Mask . vectorReverse <$> staticPeekIO (castPtr ptr)
   staticPokeIO ptr (Mask v) = staticPokeIO (castPtr ptr) (vectorReverse v)

instance Storable Mask where
   sizeOf _    = natValue @(SizeOf Mask)
   alignment _ = natValue @(Alignment Mask)
   peekIO      = staticPeekIO
   pokeIO      = staticPokeIO

-- | PCM hw parameters
data PcmHwParams = PcmHwParams
   { pcmHwParamsFlags               :: PcmHwParamsFlags
   , pcmHwParamsMasks               :: Vector 8 Mask
   , pcmHwParamsIntervals           :: Vector 21 Interval
   , pcmHwParamsRequestedMasks      :: Word32             -- ^ W: requested masks
   , pcmHwParamsChangedMasks        :: Word32             -- ^ R: changed masks
   , pcmHwParamsInfo                :: Word32             -- ^ R: Info flags for returned setup
   , pcmHwParamsMostSignificantBits :: Word32             -- ^ R: used most significant bits
   , pcmHwParamsRateNumerator       :: Word32             -- ^ R: rate numerator
   , pcmHwParamsRateDenominator     :: Word32             -- ^ R: rate denominator
   , pcmHwParamsFifoSize            :: Word64             -- ^ R: chip FIFO size in frames
   , pcmHwParamsReserved            :: Vector 64 Word8    -- ^ reserved for future
   } deriving (Generic, Storable, Show)

data PcmTimeStampMode
   = PcmTimeStampNone
   | PcmTimeStampEnabled
   deriving (Show,Eq,Enum)

data PcmSwParams = PcmSwParams
   { pcmSwParamsTimeStamp        :: Int32           -- ^ timestamp mode
   , pcmSwParamsPeriodStep       :: Word32
   , pcmSwParamsSleepMin         :: Word32          -- ^ min ticks to sleep
   , pcmSwParamsAvailMin         :: Word64          -- ^ min avail frames for wakeup
   , pcmSwParamsXFerAlign        :: Word64          -- ^ obsolete: xfer size need to be a multiple
   , pcmSwParamsStartThreshold   :: Word64          -- ^ min hw_avail frames for automatic start
   , pcmSwParamsStopThreshold    :: Word64          -- ^ min avail frames for automatic stop
   , pcmSwParamsSilenceThreshold :: Word64          -- ^ min distance from noise for silence filling
   , pcmSwParamsSilenceSize      :: Word64          -- ^ silence block size
   , pcmSwParamsBoundary         :: Word64          -- ^ pointers wrap point
   , pcmSwParamsProtoVersion     :: Word32          -- ^ protocol version
   , pcmSwParamsTimeStampType    :: Word32          -- ^ timestamp type (req. proto >= 2.0.12)
   , pcmSwParamsReserved         :: Vector 56 Word8 -- ^ reserved for future
   } deriving (Generic, Storable, Show)

data PcmChannelInfo = PcmChannelInfo
   { pcmChannelInfoChannel :: Word32
   , pcmChannelInfoOffset  :: Int64  -- ^ mmap offset
   , pcmChannelInfoFirst   :: Word32 -- ^ offset to first sample in bits
   , pcmChannelInfoStep    :: Word32 -- ^ samples distance in bits
   } deriving (Show,Generic,Storable)

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
   } deriving (Show,Generic,Storable)

data PcmMmapStatus = PcmMmapStatus
   { pcmMmapStatusState          :: PcmStateField -- ^ RO: state - SNDRV_PCM_STATE_XXXX
   , pcmMmapStatusPadding        :: Int32         -- ^ Needed for 64 bit alignment
   , pcmMmapStatusHwPtr          :: Word64        -- ^ RO: hw ptr (0...boundary-1)
   , pcmMmapStatusTimeStamp      :: TimeSpec      -- ^ Timestamp
   , pcmMmapStatusSuspendedState :: PcmStateField -- ^ RO: suspended stream state
   , pcmMmapStatusAudioTimeStamp :: TimeSpec      -- ^ from sample counter or wall clock
   } deriving (Show,Generic,Storable)

data PcmMmapControl = PcmMmapControl
   { pcmMmapControlApplPtr  :: Word64  -- ^ RW: appl ptr (0...boundary-1)
   , pcmMmapControlAvailMin :: Word64  -- ^ RW: min available frames for wakeup
   } deriving (Show,Generic,Storable)

data PcmSyncFlag
   = PcmSyncFlagHwSync        -- ^ execute hwsync 
   | PcmSyncFlagPtrAppl       -- ^ get appl_ptr from driver (r/w op) 
   | PcmSyncFlagPtrAvailMin   -- ^ get avail_min from driver 
   deriving (Show,Eq,Enum,BitOffset)

type PcmSyncFlags = BitSet Word32 PcmSyncFlag

data PcmSyncPtr = PcmSyncPtr
   { pcmSyncPtrFlags   :: PcmSyncFlags
   , pcmSyncPtrStatus  :: PcmMmapStatus
   , pcmSyncPtrControl :: PcmMmapControl
   , pcmSyncPtrPadding :: Vector 48 Word8
   } deriving (Show, Generic, Storable)

data XferI = XferI
   { xferiResult :: Int64
   , xferiBuffer :: Ptr ()
   , xferiFrames :: Word64
   } deriving (Show, Generic, Storable)

data XferN = XferN
   { xfernResult  :: Int64
   , xfernBuffers :: Ptr (Ptr ())
   , xfernFrames  :: Word64
   } deriving (Show, Generic, Storable)

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
   deriving (Show,Eq,Enum,Bounded)

data ChannelOption
   = ChannelPhaseInverse
   | ChannelDriverSpec
   deriving (Show,Eq,BitOffset)

instance Enum ChannelOption where
   fromEnum x = case x of
      ChannelPhaseInverse -> 16
      ChannelDriverSpec   -> 17
   toEnum x = case x of
      16 -> ChannelPhaseInverse
      17 -> ChannelDriverSpec
      _  -> error "Unknown channel option"        

pcmIoctl :: MonadInIO m => Word8 -> Handle -> Excepts '[ErrorCode] m ()
pcmIoctl n = ioctlSignal 0x41 n (0 :: Int)

pcmIoctlWR :: (MonadInIO m, Storable a) => Word8 -> a -> Handle -> Excepts '[ErrorCode] m a
pcmIoctlWR = ioctlWriteRead 0x41

pcmIoctlW :: (MonadInIO m, Storable a) => Word8 -> a -> Handle -> Excepts '[ErrorCode] m ()
pcmIoctlW = ioctlWrite 0x41

pcmIoctlR :: (MonadInIO m, Storable a) => Word8 -> Handle -> Excepts '[ErrorCode] m a
pcmIoctlR = ioctlRead 0x41

ioctlPcmVersion :: MonadInIO m => Handle -> Excepts '[ErrorCode] m Int32
ioctlPcmVersion = pcmIoctlR 0x00

ioctlPcmInfo :: MonadInIO m => Handle -> Excepts '[ErrorCode] m PcmInfo
ioctlPcmInfo = pcmIoctlR 0x01

ioctlPcmTimeStamp :: MonadInIO m => Int32 -> Handle -> Excepts '[ErrorCode] m ()
ioctlPcmTimeStamp = pcmIoctlW 0x02

ioctlPcmTTimeStamp :: MonadInIO m => Int32 -> Handle -> Excepts '[ErrorCode] m ()
ioctlPcmTTimeStamp = pcmIoctlW 0x03

ioctlPcmHwRefine :: MonadInIO m => PcmHwParams -> Handle -> Excepts '[ErrorCode] m PcmHwParams
ioctlPcmHwRefine = pcmIoctlWR 0x10

ioctlPcmHwParams :: MonadInIO m => PcmHwParams -> Handle -> Excepts '[ErrorCode] m PcmHwParams
ioctlPcmHwParams = pcmIoctlWR 0x11

ioctlPcmHwFree :: MonadInIO m => Handle -> Excepts '[ErrorCode] m ()
ioctlPcmHwFree = pcmIoctl 0x12

ioctlPcmSwParams :: MonadInIO m => PcmSwParams -> Handle -> Excepts '[ErrorCode] m PcmSwParams
ioctlPcmSwParams = pcmIoctlWR 0x13

ioctlPcmStatus :: MonadInIO m => Handle -> Excepts '[ErrorCode] m PcmStatus
ioctlPcmStatus = pcmIoctlR 0x20

ioctlPcmDelay :: MonadInIO m => Handle -> Excepts '[ErrorCode] m Int64
ioctlPcmDelay = pcmIoctlR 0x21

ioctlPcmHwSync :: MonadInIO m => Handle -> Excepts '[ErrorCode] m ()
ioctlPcmHwSync = pcmIoctl 0x22

ioctlPcmSyncPtr :: MonadInIO m => PcmSyncPtr -> Handle -> Excepts '[ErrorCode] m PcmSyncPtr
ioctlPcmSyncPtr = pcmIoctlWR 0x23

ioctlPcmStatusExt :: MonadInIO m => PcmStatus -> Handle -> Excepts '[ErrorCode] m PcmStatus
ioctlPcmStatusExt = pcmIoctlWR 0x24

ioctlPcmChannelInfo :: MonadInIO m => Handle -> Excepts '[ErrorCode] m PcmChannelInfo
ioctlPcmChannelInfo = pcmIoctlR 0x32

ioctlPcmPrepare :: MonadInIO m => Handle -> Excepts '[ErrorCode] m ()
ioctlPcmPrepare = pcmIoctl 0x40

ioctlPcmReset :: MonadInIO m => Handle -> Excepts '[ErrorCode] m ()
ioctlPcmReset = pcmIoctl 0x41

ioctlPcmStart :: MonadInIO m => Handle -> Excepts '[ErrorCode] m ()
ioctlPcmStart = pcmIoctl 0x42

ioctlPcmDrop :: MonadInIO m => Handle -> Excepts '[ErrorCode] m ()
ioctlPcmDrop = pcmIoctl 0x43

ioctlPcmDrain :: MonadInIO m => Handle -> Excepts '[ErrorCode] m ()
ioctlPcmDrain = pcmIoctl 0x44

ioctlPcmPause :: MonadInIO m => Int32 -> Handle -> Excepts '[ErrorCode] m ()
ioctlPcmPause = pcmIoctlW 0x45

ioctlPcmRewind :: MonadInIO m => Word64 -> Handle -> Excepts '[ErrorCode] m ()
ioctlPcmRewind = pcmIoctlW 0x46

ioctlPcmResume :: MonadInIO m => Handle -> Excepts '[ErrorCode] m ()
ioctlPcmResume = pcmIoctl 0x47

ioctlPcmXRun :: MonadInIO m => Handle -> Excepts '[ErrorCode] m ()
ioctlPcmXRun = pcmIoctl 0x48

ioctlPcmForward :: MonadInIO m => Word64 -> Handle -> Excepts '[ErrorCode] m ()
ioctlPcmForward = pcmIoctlW 0x49

ioctlPcmWriteIFrames :: MonadInIO m => XferI -> Handle -> Excepts '[ErrorCode] m ()
ioctlPcmWriteIFrames = pcmIoctlW 0x50

ioctlPcmReadIFrames :: MonadInIO m => Handle -> Excepts '[ErrorCode] m XferI
ioctlPcmReadIFrames = pcmIoctlR 0x51

ioctlPcmWriteNFrames :: MonadInIO m => XferN -> Handle -> Excepts '[ErrorCode] m ()
ioctlPcmWriteNFrames = pcmIoctlW 0x52

ioctlPcmReadNFrames :: MonadInIO m => Handle -> Excepts '[ErrorCode] m XferN
ioctlPcmReadNFrames = pcmIoctlR 0x53

ioctlPcmLink :: MonadInIO m => Int32 -> Handle -> Excepts '[ErrorCode] m ()
ioctlPcmLink = pcmIoctlW 0x60

ioctlPcmUnlink :: MonadInIO m => Handle -> Excepts '[ErrorCode] m ()
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
   deriving (Show,Eq,Enum,BitOffset)

type MidiFlags = BitSet Word32 MidiFlag

data MidiInfo = MidiInfo
   { midiInfoDevice         :: Word32           -- ^ RO/WR (control): device number
   , midiInfoSubDevice      :: Word32           -- ^ RO/WR (control): subdevice number
   , midiInfoStream         :: Int32            -- ^ WR: stream
   , midiInfoCard           :: Int32            -- ^ R: card number
   , midiInfoFlags          :: MidiFlags        -- ^ SNDRV_RAWMIDI_INFO_XXXX
   , midiInfoId             :: CStringBuffer 64 -- ^ ID (user selectable)
   , midiInfoName           :: CStringBuffer 80 -- ^ name of device
   , midiInfoSubName        :: CStringBuffer 32 -- ^ name of active or selected subdevice
   , midiInfoSubDeviceCount :: Word32
   , midiInfoSubDeviceAvail :: Word32
   , midiInfoReserved       :: Vector 64 Word8  -- ^ reserved for future use
   } deriving (Show, Generic, Storable)

data MidiParams = MidiParams
   { midiParamsStream          :: Int32
   , midiParamsBufferSize      :: CSize           -- ^ queue size in bytes
   , midiParamsAvailMin        :: CSize           -- ^ minimum avail bytes for wakeup
   , midiParamsNoActiveSensing :: Word32          -- ^ do not send active sensing byte in close()
   , midiParamsReserved        :: Vector 16 Word8 -- ^ reserved for future use
   } deriving (Show, Generic, Storable)

data MidiStatus = MidiStatus
   { midiStatusStream    :: Int32
   , midiStatusTimeStamp :: TimeSpec        -- ^ Timestamp
   , midiStatusAvail     :: CSize           -- ^ available bytes
   , midiStatusXRuns     :: CSize           -- ^ count of overruns since last status (in bytes)
   , midiStatusReserved  :: Vector 16 Word8 -- ^ reserved for future use
   } deriving (Show, Generic, Storable)

midiIoctlW :: (MonadInIO m, Storable a) => Word8 -> a -> Handle -> Excepts '[ErrorCode] m ()
midiIoctlW = ioctlWrite 0x57

midiIoctlR :: (MonadInIO m, Storable a) => Word8 -> Handle -> Excepts '[ErrorCode] m a
midiIoctlR = ioctlRead 0x57

midiIoctlWR :: (MonadInIO m, Storable a) => Word8 -> a -> Handle -> Excepts '[ErrorCode] m a
midiIoctlWR = ioctlWriteRead 0x57


ioctlMidiVersion :: MonadInIO m => Handle -> Excepts '[ErrorCode] m Int32
ioctlMidiVersion = midiIoctlR 0x00

ioctlMidiInfo :: MonadInIO m => Handle -> Excepts '[ErrorCode] m MidiInfo
ioctlMidiInfo = midiIoctlR 0x01

ioctlMidiParams :: MonadInIO m => MidiParams -> Handle -> Excepts '[ErrorCode] m MidiParams
ioctlMidiParams = midiIoctlWR 0x10

ioctlMidiStatus :: MonadInIO m => MidiStatus -> Handle -> Excepts '[ErrorCode] m MidiStatus
ioctlMidiStatus = midiIoctlWR 0x20

ioctlMidiDrop :: MonadInIO m => Int32 -> Handle -> Excepts '[ErrorCode] m ()
ioctlMidiDrop = midiIoctlW 0x30

ioctlMidiDrain :: MonadInIO m => Int32 -> Handle -> Excepts '[ErrorCode] m ()
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
   deriving (Show,Eq,Enum,BitOffset)

type TimerFlags = BitSet Word32 TimerFlag


data TimerId = TimerId
   { timerIdDeviceClass    :: Int32
   , timerIdDeviceSubClass :: Int32
   , timerIdCard           :: Int32
   , timerIdDevice         :: Int32
   , timerIdSubDevice      :: Int32
   } deriving (Show,Generic,Storable)

data TimerGInfo = TimerGInfo
   { timerGInfoTimerID       :: TimerId          -- ^ requested timer ID
   , timerGInfoFlags         :: TimerFlags       -- ^ timer flags
   , timerGInfoCard          :: Int32            -- ^ card number
   , timerGInfoId            :: CStringBuffer 64 -- ^ timer identification
   , timerGInfoName          :: CStringBuffer 80 -- ^ timer name
   , timerGInfoReserved      :: Word64           -- ^ reserved for future use
   , timerGInfoResolution    :: Word64           -- ^ average period resolution in ns
   , timerGInfoResolutionMin :: Word64           -- ^ minimal period resolution in ns
   , timerGInfoResolutionMax :: Word64           -- ^ maximal period resolution in ns
   , timerGInfoClients       :: Word32           -- ^ active timer clients
   , timerGInfoReserved2     :: Vector 32 Word8
   } deriving (Show,Generic,Storable)

data TimerGParams = TimerGParams
   { timerGParamsTimerId          :: TimerId        -- ^ requested timer ID
   , timerGParamsPeriodNumerator  :: Word64         -- ^ requested precise period duration (in seconds) - numerator
   , timerGParamsPeriodDenomintor :: Word64         -- ^ requested precise period duration (in seconds) - denominator
   , timerGParamsReserved         :: Vector 32 Word8
   } deriving (Show,Generic,Storable)

data TimerGStatus = TimerGStatus
   { timerGStatusTimerId               :: TimerId         -- ^ requested timer ID
   , timerGStatusResolution            :: Word64          -- ^ current period resolution in ns
   , timerGStatusResolutionNumerator   :: Word64          -- ^ precise current period resolution (in seconds) - numerator
   , timerGStatusResolutionDenominator :: Word64          -- ^ precise current period resolution (in seconds) - denominator
   , timerGStatusReserved              :: Vector 32 Word8
   } deriving (Show,Generic,Storable)

data TimerSelect = TimerSelect
   { timerSelectTimerId  :: TimerId         -- ^ bind to timer ID
   , timerSelectReserved :: Vector 32 Word8 -- ^ reserved
   } deriving (Show,Generic,Storable)

data TimerInfo = TimerInfo
   { timerInfoFlags      :: TimerFlags       -- ^ timer flags
   , timerInfoCard       :: Int32            -- ^ card number
   , timerInfoId         :: CStringBuffer 64 -- ^ timer identification
   , timerInfoName       :: CStringBuffer 80 -- ^ timer name
   , timerInfoReserved   :: Word64           -- ^ reserved for future use
   , timerInfoResolution :: Word64           -- ^ average period resolution in ns
   , timerInfoReserved2  :: Vector 64 Word8
   } deriving (Show,Generic,Storable)

data TimerParamsFlag
   = TimerParamFlagAuto       -- ^ auto start, otherwise one-shot 
   | TimerParamFlagExclusive  -- ^ exclusive use, precise start/stop/pause/continue 
   | TimerParamFlagEarlyEvent -- ^ write early event to the poll queue 
   deriving (Show,Eq,Enum,BitOffset)

type TimerParamsFlags = BitSet Word32 TimerParamsFlag

data TimerParams = TimerParams
   { timerParamsFlags     :: TimerParamsFlags -- ^ flags - SNDRV_MIXER_PSFLG_*
   , timerParamsTicks     :: Word32           -- ^ requested resolution in ticks
   , timerParamsQueueSize :: Word32           -- ^ total size of queue (32-1024)
   , timerParamsReserved  :: Word32           -- ^ reserved, was: failure locations
   , timerParamsFilter    :: Word32           -- ^ event filter (bitmask of SNDRV_TIMER_EVENT_*)
   , timerParamsReserved2 :: Vector 60 Word8  -- ^ reserved
   } deriving (Show,Generic,Storable)

data TimerStatus = TimerStatus
   { timerStatusTimeStamp  :: TimeSpec        -- ^ Timestamp - last update
   , timerStatusResolution :: Word32          -- ^ current period resolution in ns
   , timerStatusLost       :: Word32          -- ^ counter of master tick lost
   , timerStatusOverrun    :: Word32          -- ^ count of read queue overruns
   , timerStatusQueue      :: Word32          -- ^ used queue size
   , timerStatusReserved   :: Vector 64 Word8 -- ^ reserved
   } deriving (Show,Generic,Storable)

timerIoctl :: MonadInIO m => Word8 -> Handle -> Excepts '[ErrorCode] m ()
timerIoctl n = ioctlSignal 0x54 n (0 :: Int)

timerIoctlW :: (MonadInIO m, Storable a) => Word8 -> a -> Handle -> Excepts '[ErrorCode] m ()
timerIoctlW = ioctlWrite 0x54

timerIoctlR :: (MonadInIO m, Storable a) => Word8 -> Handle -> Excepts '[ErrorCode] m a
timerIoctlR = ioctlRead 0x54

timerIoctlWR :: (MonadInIO m, Storable a) => Word8 -> a -> Handle -> Excepts '[ErrorCode] m a
timerIoctlWR = ioctlWriteRead 0x54

ioctlTimerVersion :: MonadInIO m => Handle -> Excepts '[ErrorCode] m Int32
ioctlTimerVersion = timerIoctlR 0x00

ioctlTimerNextDevice :: MonadInIO m => TimerId -> Handle -> Excepts '[ErrorCode] m TimerId
ioctlTimerNextDevice = timerIoctlWR 0x01

ioctlTimerTRead :: MonadInIO m => Int32 -> Handle -> Excepts '[ErrorCode] m ()
ioctlTimerTRead = timerIoctlW 0x02

ioctlTimerGInfo :: MonadInIO m => TimerGInfo -> Handle -> Excepts '[ErrorCode] m TimerGInfo
ioctlTimerGInfo = timerIoctlWR 0x03

ioctlTimerGParams :: MonadInIO m => TimerGParams -> Handle -> Excepts '[ErrorCode] m ()
ioctlTimerGParams = timerIoctlW 0x04

ioctlTimerGStatus :: MonadInIO m => TimerGStatus -> Handle -> Excepts '[ErrorCode] m TimerGStatus
ioctlTimerGStatus = timerIoctlWR 0x05

ioctlTimerSelect :: MonadInIO m => TimerSelect -> Handle -> Excepts '[ErrorCode] m ()
ioctlTimerSelect = timerIoctlW 0x10

ioctlTimerInfo :: MonadInIO m => Handle -> Excepts '[ErrorCode] m TimerInfo
ioctlTimerInfo = timerIoctlR 0x11

ioctlTimerParams :: MonadInIO m => TimerParams -> Handle -> Excepts '[ErrorCode] m ()
ioctlTimerParams = timerIoctlW 0x12

ioctlTimerStatus :: MonadInIO m => Handle -> Excepts '[ErrorCode] m TimerStatus
ioctlTimerStatus = timerIoctlR 0x14

ioctlTimerStart :: MonadInIO m => Handle -> Excepts '[ErrorCode] m ()
ioctlTimerStart = timerIoctl 0xa0

ioctlTimerStop :: MonadInIO m => Handle -> Excepts '[ErrorCode] m ()
ioctlTimerStop = timerIoctl 0xa1

ioctlTimerContinue :: MonadInIO m => Handle -> Excepts '[ErrorCode] m ()
ioctlTimerContinue = timerIoctl 0xa2

ioctlTimerPause :: MonadInIO m => Handle -> Excepts '[ErrorCode] m ()
ioctlTimerPause = timerIoctl 0xa3

data TimerRead = TimerRead
   { timerReadResolution :: Word32
   , timerReadTicks      :: Word32
   } deriving (Show,Generic,Storable)

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
   { timerTReadEvent     :: Int32
   , timerTReadTimeStamp :: TimeSpec
   , timerTReadValue     :: Word32
   } deriving (Show,Generic,Storable)

-----------------------------------------------------------------------------
-- Driver control interface - /dev/snd/control*
-----------------------------------------------------------------------------

controlVersion :: Word32
controlVersion = 0x00020007

data ControlCardInfo = ControlCardInfo
   { controlCardInfoCard       :: Int32             -- ^ card number
   , controlCardInfoPad        :: Int32             -- ^ reserved for future (was type)
   , controlCardInfoId         :: CStringBuffer 16  -- ^ ID of card (user selectable)
   , controlCardInfoDriver     :: CStringBuffer 16  -- ^ Driver name
   , controlCardInfoName       :: CStringBuffer 32  -- ^ Short name of soundcard
   , controlCardInfoLongName   :: CStringBuffer 80  -- ^ name + info text about soundcard
   , controlCardInfoReserved   :: Vector 16 Word8   -- ^ reserved for future (was ID of mixer)
   , controlCardInfoMixerName  :: CStringBuffer 80  -- ^ visual mixer identification
   , controlCardInfoComponents :: CStringBuffer 128 -- ^ card components / fine identification, delimited with one space (AC97 etc..)
   } deriving (Show,Generic,Storable)

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

instance BitOffset ControlElementAccess where
   toBitOffset x = case x of
      ControlElemAccessInactive     -> 8
      ControlElemAccessLock         -> 9
      ControlElemAccessOwner        -> 10
      ControlElemAccessTlvCallBack  -> 28
      ControlElemAccessUser         -> 29
      _                             -> fromIntegral (fromEnum x)
   fromBitOffset x = case x of
      8  -> ControlElemAccessInactive
      9  -> ControlElemAccessLock
      10 -> ControlElemAccessOwner
      28 -> ControlElemAccessTlvCallBack
      29 -> ControlElemAccessUser
      _  -> toEnum (fromIntegral x)

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
   { controlElemIdNumId     :: Int32            -- ^ numeric identifier, zero = invalid
   , controlElemIdInterface :: Int32            -- ^ interface identifier
   , controlElemIdDevice    :: Word32           -- ^ device/client number
   , controlElemIdSubDevice :: Word32           -- ^ subdevice (substream) number
   , controlElemIdName      :: CStringBuffer 44 -- ^ ASCII name of item
   , controlElemIdIndex     :: Word32           -- ^ index of item
   } deriving (Show,Generic,Storable)

data ControlElementList = ControlElementList
   { controlElemListOffset   :: Word32          -- ^ W: first element ID to get
   , controlElemListSpace    :: Word32          -- ^ W: count of element IDs to get
   , controlElemListUsed     :: Word32          -- ^ R: count of element IDs set
   , controlElemListCount    :: Word32          -- ^ R: count of all elements
   , controlElemListPids     :: Ptr ()          -- ^ R: IDs
   , controlElemListReserved :: Vector 50 Word8
   } deriving (Show,Generic,Storable)

data IntegerValue = IntegerValue
   { integerValueMin  :: Word32 -- ^ R: minimum value
   , integerValueMax  :: Word32 -- ^ R: maximum value
   , integerValueStep :: Word32 -- ^ R: step (0 variable)
   } deriving (Show,Generic,Storable)

data Integer64Value = Integer64Value
   { integer64ValueMin  :: Word64 -- ^ R: minimum value
   , integer64ValueMax  :: Word64 -- ^ R: maximum value
   , integer64ValueStep :: Word64 -- ^ R: step (0 variable)
   } deriving (Show,Generic,Storable)

data EnumeratedValue = EnumeratedValue
   { enumeratedCount       :: Word32           -- ^ R: number of items
   , enumeratedItem        :: Word32           -- ^ W: item number
   , enumeratedName        :: CStringBuffer 64 -- ^ R: value name
   , enumeratedNamesPtr    :: Word64           -- ^ W: names list (ELEM_ADD only)
   , enumeratedNamesLength :: Word32
   } deriving (Show,Generic,Storable)

data ControlElementInfo = ControlElementInfo
   { controlElemInfoId         :: ControlElementId
   , controlElemInfoType       :: Int32
   , controlElemInfoAccess     :: Word32
   , controlElemInfoCount      :: Word32
   , controlElemInfoOwner      :: ProcessID
   , controlElemInfoValue      :: Union '[IntegerValue,Integer64Value,EnumeratedValue,Vector 128 Word8]
   , controlElemInfoDimensions :: Vector 4 Word16
   } deriving (Show,Generic,Storable)

data ControlElementValue = ControlElementValue
   { controlElemValueId        :: ControlElementId                    -- ^ W: element ID
   , controlElemValueIndirect  :: Word32                              -- ^ W: indirect access - obsoleted
   , controlElemValueValue     :: Union '[Vector 512 Word8,AesIec958]
   , controlElemValueTimeStamp :: TimeSpec
   , controlElemValueReserved  :: Vector 112 Word8
   } deriving (Show,Generic,Storable)

data ControlTLV = ControlTLV
   { controlTlvNumId  :: Word32          -- ^ control element numeric identification
   , controlTlvLength :: Word32          -- ^ in bytes aligned to 4
   , controlTlvTlv    :: Vector 0 Word32 -- ^ first TLV
   -- FIXME: the array is allocated "after" the struct...
   } deriving (Show,Generic,Storable)

controlIoctlW :: (MonadInIO m, Storable a) => Word8 -> a -> Handle -> Excepts '[ErrorCode] m ()
controlIoctlW = ioctlWrite 0x55

controlIoctlR :: (MonadInIO m, Storable a) => Word8 -> Handle -> Excepts '[ErrorCode] m a
controlIoctlR = ioctlRead 0x55

controlIoctlWR :: (MonadInIO m, Storable a) => Word8 -> a -> Handle -> Excepts '[ErrorCode] m a
controlIoctlWR = ioctlWriteRead 0x55

ioctlControlVersion :: MonadInIO m => Handle -> Excepts '[ErrorCode] m Int32
ioctlControlVersion = controlIoctlR 0x00

ioctlControlCardInfo :: MonadInIO m => Handle -> Excepts '[ErrorCode] m ControlCardInfo
ioctlControlCardInfo = controlIoctlR 0x01

ioctlControlElemList :: MonadInIO m => ControlElementList -> Handle -> Excepts '[ErrorCode] m ControlElementList
ioctlControlElemList = controlIoctlWR 0x10

ioctlControlElemInfo :: MonadInIO m => ControlElementInfo -> Handle -> Excepts '[ErrorCode] m ControlElementInfo
ioctlControlElemInfo = controlIoctlWR 0x11

ioctlControlElemRead :: MonadInIO m => ControlElementValue -> Handle -> Excepts '[ErrorCode] m ControlElementValue
ioctlControlElemRead = controlIoctlWR 0x12

ioctlControlElemWrite :: MonadInIO m => ControlElementValue -> Handle -> Excepts '[ErrorCode] m ControlElementValue
ioctlControlElemWrite = controlIoctlWR 0x13

ioctlControlElemLock :: MonadInIO m => ControlElementId -> Handle -> Excepts '[ErrorCode] m ()
ioctlControlElemLock = controlIoctlW 0x14

ioctlControlElemUnlock :: MonadInIO m => ControlElementId -> Handle -> Excepts '[ErrorCode] m ()
ioctlControlElemUnlock = controlIoctlW 0x15

ioctlControlSubscribeEvents :: MonadInIO m => Int32 -> Handle -> Excepts '[ErrorCode] m Int32
ioctlControlSubscribeEvents = controlIoctlWR 0x16

ioctlControlElemAdd :: MonadInIO m => ControlElementInfo -> Handle -> Excepts '[ErrorCode] m ControlElementInfo
ioctlControlElemAdd = controlIoctlWR 0x17

ioctlControlElemReplace :: MonadInIO m => ControlElementInfo -> Handle -> Excepts '[ErrorCode] m ControlElementInfo
ioctlControlElemReplace = controlIoctlWR 0x18

ioctlControlElemRemove :: MonadInIO m => ControlElementInfo -> Handle -> Excepts '[ErrorCode] m ControlElementInfo
ioctlControlElemRemove = controlIoctlWR 0x19

ioctlControlTLVRead :: MonadInIO m => ControlTLV -> Handle -> Excepts '[ErrorCode] m ControlTLV
ioctlControlTLVRead = controlIoctlWR 0x1a

ioctlControlTLVWrite :: MonadInIO m => ControlTLV -> Handle -> Excepts '[ErrorCode] m ControlTLV
ioctlControlTLVWrite = controlIoctlWR 0x1b

ioctlControlTLVCommand :: MonadInIO m => ControlTLV -> Handle -> Excepts '[ErrorCode] m ControlTLV
ioctlControlTLVCommand = controlIoctlWR 0x1c

ioctlControlHwDepNextDevice :: MonadInIO m => Int32 -> Handle -> Excepts '[ErrorCode] m Int32
ioctlControlHwDepNextDevice = controlIoctlWR 0x20

ioctlControlHwInfo :: MonadInIO m => Handle -> Excepts '[ErrorCode] m HwInfo
ioctlControlHwInfo = controlIoctlR 0x21

ioctlControlPcmNextDevice :: MonadInIO m => Handle -> Excepts '[ErrorCode] m Int32
ioctlControlPcmNextDevice = controlIoctlR 0x30

ioctlControlPcmInfo :: MonadInIO m => PcmInfo -> Handle -> Excepts '[ErrorCode] m PcmInfo
ioctlControlPcmInfo = controlIoctlWR 0x31

ioctlControlPcmPreferSubdevice :: MonadInIO m => Int32 -> Handle -> Excepts '[ErrorCode] m ()
ioctlControlPcmPreferSubdevice = controlIoctlW 0x32

ioctlControlMidiNextDevice :: MonadInIO m => Int32 -> Handle -> Excepts '[ErrorCode] m Int32
ioctlControlMidiNextDevice = controlIoctlWR 0x40

ioctlControlMidiInfo :: MonadInIO m => MidiInfo -> Handle -> Excepts '[ErrorCode] m MidiInfo
ioctlControlMidiInfo = controlIoctlWR 0x41

ioctlControlMidiPreferSubdevice :: MonadInIO m => Int32 -> Handle -> Excepts '[ErrorCode] m ()
ioctlControlMidiPreferSubdevice = controlIoctlW 0x42

ioctlControlPower :: MonadInIO m => Int32 -> Handle -> Excepts '[ErrorCode] m Int32
ioctlControlPower = controlIoctlWR 0xd0

ioctlControlPowerState :: MonadInIO m => Handle -> Excepts '[ErrorCode] m Int32
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
   { controlEventType   :: Int32
   , controlEventMask   :: Word32
   , controlEventElemId :: ControlElementId
   } deriving (Show,Generic,Storable)
