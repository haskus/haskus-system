module ViperVM.Arch.Linux.Input.ForceFeedback
   ( ForceFeedbackDirection(..)
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
   , removeForceFeedback
   , supportedSimultaneousEffects
   )
where

import Data.Word
import Data.Int
import Foreign.Ptr (Ptr)

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Ioctl

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
-- @right_coeff: controls how fast the force grows when the joystick moves to the right
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

-- | Erase a force effect
--
-- EVIOCRMFF
removeForceFeedback :: IOCTL -> FileDescriptor -> Int -> SysRet ()
removeForceFeedback ioctl =  ioctlWrite ioctl 0x45 0x81 defaultCheckRet 

-- | Report the number of effects playable at the same time
--
-- EVIOCGEFFECTS
supportedSimultaneousEffects :: IOCTL -> FileDescriptor -> SysRet Int
supportedSimultaneousEffects ioctl = ioctlRead ioctl 0x45 0x84 defaultCheck

-- TODO
-- #define EVIOCSFF		_IOC(_IOC_WRITE, 'E', 0x80, sizeof(struct ff_effect))	/* send a force effect to a force feedback device */

