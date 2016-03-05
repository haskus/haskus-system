{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ViperVM.Arch.Linux.Input.ForceFeedback
   ( ForceFeedbackEffect(..)
   , ForceFeedbackEffectHeader(..)
   , ForceFeedbackDirection(..)
   , ForceFeedbackType(..)
   , ForceFeedbackPeriodicEffectType(..)
   , ForceFeedbackDeviceProperties(..)
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
   , sendForceFeedback
   )
where

import Data.Word
import Data.Int
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import GHC.Generics (Generic)
import Foreign.Storable
import Foreign.CStorable

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
   deriving (Show,Eq)

instance Storable ForceFeedbackDirection where
   sizeOf _    = 2
   alignment _ = 1
   peek ptr    = do
      v <- peek (castPtr ptr :: Ptr Word16)
      return $ case v of
         0x0000 -> FFDown
         0x4000 -> FFLeft
         0x8000 -> FFUp
         0xC000 -> FFRight
         _      -> error "Invalid force feedback direction"
   poke ptr d  = poke (castPtr ptr :: Ptr Word16) $ case d of
         FFDown   -> 0x0000
         FFLeft   -> 0x4000
         FFUp     -> 0x8000
         FFRight  -> 0xC000
      
instance CStorable ForceFeedbackDirection where
   cSizeOf    = sizeOf
   cAlignment = alignment
   cPeek      = peek
   cPoke      = poke

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

data ForceFeedbackPeriodicEffectType
   = Square
   | Triangle
   | Sine
   | SawUp
   | SawDown
   | Custom
   deriving (Show,Eq)

-- | ForceFeedbackPeriodicEffectType type is represented as a Word16 in ForceFeedbackPeriodicEffect
instance Storable ForceFeedbackPeriodicEffectType where
   alignment _ = 2
   sizeOf    _ = 2
   peek ptr    = toEnum . fromIntegral <$> peek (castPtr ptr :: Ptr Word16)
   poke ptr v  = poke (castPtr ptr :: Ptr Word16) (fromIntegral (fromEnum v))

instance CStorable ForceFeedbackPeriodicEffectType where
   cAlignment = alignment
   cSizeOf    = sizeOf
   cPeek      = peek
   cPoke      = poke

instance Enum ForceFeedbackPeriodicEffectType where
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

-- | Defines scheduling of the force-feedback effect
-- @length: duration of the effect
-- @delay: delay before effect should start playing
data ForceFeedbackReplay = ForceFeedbackReplay
   { ffReplayLength :: Word16
   , ffReplayDelay  :: Word16
   } deriving (Show,Eq,Generic,CStorable)

instance Storable ForceFeedbackReplay where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke

-- | Defines what triggers the force-feedback effect
-- @button: number of the button triggering the effect
-- @interval: controls how soon the effect can be re-triggered
data ForceFeedbackTrigger = ForceFeedbackTrigger
   { ffTriggerButton   :: Word16
   , ffTriggerInterval :: Word16
   } deriving (Show,Eq,Generic,CStorable)

instance Storable ForceFeedbackTrigger where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke


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
   } deriving (Eq,Show,Generic,CStorable)

instance Storable  ForceFeedbackEnvelope where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke


-- | Defines parameters of a constant force-feedback effect
-- @level: strength of the effect; may be negative
-- @envelope: envelope data
data ForceFeedbackConstantEffect = ForceFeedbackConstantEffect
   { ffConstantEffectLevel    :: Int16
   , ffConstantEffectEnvelope :: ForceFeedbackEnvelope
   } deriving (Eq,Show,Generic,CStorable)

instance Storable  ForceFeedbackConstantEffect where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke


-- | Defines parameters of a ramp force-feedback effect
-- @start_level: beginning strength of the effect; may be negative
-- @end_level: final strength of the effect; may be negative
-- @envelope: envelope data
data ForceFeedbackRampEffect = ForceFeedbackRampEffect
   { ffRampEffectStartLevel :: Int16
   , ffRampEffectEndLevel   :: Int16
   , ffRampEffectEnvelope   :: ForceFeedbackEnvelope
   } deriving (Eq,Show,Generic,CStorable)

instance Storable  ForceFeedbackRampEffect where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke


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
   } deriving (Eq,Show,Generic,CStorable)

instance Storable  ForceFeedbackConditionEffect where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke

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
   { ffPeriodicEffectWaveform   :: ForceFeedbackPeriodicEffectType
   , ffPeriodicEffectPeriod     :: Word16
   , ffPeriodicEffectMagnitude  :: Int16
   , ffPeriodicEffectOffset     :: Int16
   , ffPeriodicEffectPhase      :: Word16
   , ffPeriodicEffectEnvelope   :: ForceFeedbackEnvelope
   , ffPeriodicEffectCustomLen  :: Word32
   , ffPeriodicEffectCustomData :: Ptr Int16
   } deriving (Eq,Show,Generic,CStorable)

instance Storable  ForceFeedbackPeriodicEffect where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke


-- | Defines parameters of a periodic force-feedback effect
-- @strong_magnitude: magnitude of the heavy motor
-- @weak_magnitude: magnitude of the light one
--
-- Some rumble pads have two motors of different weight. Strong_magnitude
-- represents the magnitude of the vibration generated by the heavy one.
data ForceFeedbackRumbleEffect = ForceFeedbackRumbleEffect
   { ffRumbleEffectStrongMagnitude :: Word16
   , ffRumbleEffectWeakMagnitude   :: Word16
   } deriving (Eq,Show,Generic,CStorable)

instance Storable  ForceFeedbackRumbleEffect where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke

-- | Erase a force effect
--
-- EVIOCRMFF
removeForceFeedback :: IOCTL -> FileDescriptor -> Int -> SysRet ()
removeForceFeedback ioctl = ioctlWrite ioctl 0x45 0x81 defaultCheck

-- | Report the number of effects playable at the same time
--
-- EVIOCGEFFECTS
supportedSimultaneousEffects :: IOCTL -> FileDescriptor -> SysRet Int
supportedSimultaneousEffects ioctl = ioctlRead ioctl 0x45 0x84 defaultCheck

data ForceFeedbackEffectHeader = ForceFeedbackEffectHeader
   { ffHeaderType       :: Word16
   , ffHeaderID         :: Int16
   , ffHeaderDirection  :: ForceFeedbackDirection
   , ffHeaderTrigger    :: ForceFeedbackTrigger
   , ffHeaderReplay     :: ForceFeedbackReplay
   } deriving (Eq,Show,Generic,CStorable)

instance Storable ForceFeedbackEffectHeader where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke

data ForceFeedbackEffect
   = ForceFeedbackEffectConstant
      { ffHeader           :: ForceFeedbackEffectHeader
      , ffConstantParams   :: ForceFeedbackConstantEffect
      }
   | ForceFeedbackEffectRamp
      { ffHeader           :: ForceFeedbackEffectHeader
      , ffRampParams       :: ForceFeedbackRampEffect
      }
   | ForceFeedbackEffectPeriodic
      { ffHeader           :: ForceFeedbackEffectHeader
      , ffPeriodicParams   :: ForceFeedbackPeriodicEffect
      }
   | ForceFeedbackEffectRumble
      { ffHeader           :: ForceFeedbackEffectHeader
      , ffRumbleParams     :: ForceFeedbackRumbleEffect
      }
   | ForceFeedbackEffectSpring
      { ffHeader           :: ForceFeedbackEffectHeader
      , ffConditionParam1  :: ForceFeedbackConditionEffect -- one for each axis
      , ffConditionParam2  :: ForceFeedbackConditionEffect
      }
   | ForceFeedbackEffectFriction
      { ffHeader           :: ForceFeedbackEffectHeader
      , ffConditionParam1  :: ForceFeedbackConditionEffect -- one for each axis
      , ffConditionParam2  :: ForceFeedbackConditionEffect
      }
   | ForceFeedbackEffectDamper
      { ffHeader           :: ForceFeedbackEffectHeader
      }
   | ForceFeedbackEffectInertia
      { ffHeader           :: ForceFeedbackEffectHeader
      }
      


instance Storable ForceFeedbackEffect where
   alignment _ = 8

   sizeOf _    = sizeOf (undefined :: ForceFeedbackEffectHeader) + maximum
      [ sizeOf (undefined :: ForceFeedbackConstantEffect)
      , sizeOf (undefined :: ForceFeedbackRampEffect)
      , sizeOf (undefined :: ForceFeedbackPeriodicEffect)
      , 2 * sizeOf (undefined :: ForceFeedbackConditionEffect) -- one for each axis
      , sizeOf (undefined :: ForceFeedbackRumbleEffect)
      ]

   peek ptr = do
      header <- peek (castPtr ptr :: Ptr ForceFeedbackEffectHeader)
      let 
         p1 = ptr `plusPtr` sizeOf (undefined :: ForceFeedbackEffectHeader)
         p2 = p1  `plusPtr` sizeOf (undefined :: ForceFeedbackConditionEffect)

      case toEnum (fromIntegral (ffHeaderType header)) of
         Ramp     -> ForceFeedbackEffectRamp      header <$> peek (castPtr p1)
         Rumble   -> ForceFeedbackEffectRumble    header <$> peek (castPtr p1)
         Periodic -> ForceFeedbackEffectPeriodic  header <$> peek (castPtr p1)
         Constant -> ForceFeedbackEffectConstant  header <$> peek (castPtr p1) 
         Spring   -> ForceFeedbackEffectSpring    header <$> peek (castPtr p1) <*> peek (castPtr p2)
         Friction -> ForceFeedbackEffectFriction  header <$> peek (castPtr p1) <*> peek (castPtr p2)
         Damper   -> return (ForceFeedbackEffectDamper   header)
         Inertia  -> return (ForceFeedbackEffectInertia  header)

   poke ptr x = do
      poke (castPtr ptr) (ffHeader x)
      let 
         p1 = ptr `plusPtr` sizeOf (undefined :: ForceFeedbackEffectHeader)
         p2 = p1  `plusPtr` sizeOf (undefined :: ForceFeedbackConditionEffect)

      case x of
         ForceFeedbackEffectRamp     _ v1    -> poke (castPtr p1) v1
         ForceFeedbackEffectRumble   _ v1    -> poke (castPtr p1) v1
         ForceFeedbackEffectPeriodic _ v1    -> poke (castPtr p1) v1
         ForceFeedbackEffectConstant _ v1    -> poke (castPtr p1) v1
         ForceFeedbackEffectSpring   _ v1 v2 -> poke (castPtr p1) v1 >> poke (castPtr p2) v2
         ForceFeedbackEffectFriction _ v1 v2 -> poke (castPtr p1) v1 >> poke (castPtr p2) v2
         ForceFeedbackEffectDamper   _ -> return ()
         ForceFeedbackEffectInertia  _ -> return ()

         
-- | Send a force effect to a force feedback device
--
-- TODO: we should return the effect ID
--
-- EVIOCSFF
sendForceFeedback :: IOCTL -> FileDescriptor -> ForceFeedbackEffect -> SysRet ()
sendForceFeedback ioctl = ioctlWrite ioctl 0x45 0x80 defaultCheck
