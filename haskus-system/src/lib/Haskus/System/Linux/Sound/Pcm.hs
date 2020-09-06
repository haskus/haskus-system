{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

-- | PCM devices
module Haskus.System.Linux.Sound.Pcm
   ( anyInterval
   , anyMask
   , anyHwParams
   , defaultSwParams
   , PcmConfig (..)
   , toConfig
   )
where

import Haskus.System.Linux.Internals.Sound
import qualified Haskus.Binary.BitSet as BitSet
import Haskus.Binary.BitSet (BitOffset)
import qualified Haskus.Binary.Vector as Vector
import Haskus.Binary.Bits (complement,zeroBits)
import Haskus.Number.Word

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio


-- Note [PCM params]
-- ~~~~~~~~~~~~~~~~~
--
-- PCM devices can be used to play sounds by supplying them samples. But first
-- we need to configure them by setting: buffer size, sample rate, pcm data
-- format, etc.
--
-- There are two kinds of parameters: sets (or masks) and intervals.
--    - Set params allow us to choose amongst a predefined set of values.
--    - Interval params allow us to choose any value in a given interval.
--
-- The idea to set parameters is to start with the largest sets and intervals
-- and to refine them until they contain a single value. At each refinement step
-- we can check if the device supports the given configuration.

-- | Any interval
anyInterval :: Interval
anyInterval = Interval
   { intervalMin     = 0
   , intervalMax     = maxBound
   , intervalOptions = BitSet.empty
   }

-- | Any mask
anyMask :: Mask
anyMask = Mask (complement zeroBits)

-- | Any hw parameters
anyHwParams :: PcmHwParams
anyHwParams = PcmHwParams
   { pcmHwParamsFlags               = BitSet.empty
   , pcmHwParamsMasks               = Vector.replicate anyMask
   , pcmHwParamsIntervals           = Vector.replicate anyInterval
   , pcmHwParamsRequestedMasks      = maxBound -- retrieve all masks/intervals
   , pcmHwParamsChangedMasks        = 0        -- nothing has been changed
   , pcmHwParamsInfo                = maxBound -- return all info flags
   , pcmHwParamsMostSignificantBits = 0
   , pcmHwParamsRateNumerator       = 0
   , pcmHwParamsRateDenominator     = 0
   , pcmHwParamsFifoSize            = 0
   , pcmHwParamsReserved            = Vector.replicate 0
   }

-- | Default sw parameters
--
-- snd_pcm_sw_params_default
defaultSwParams :: PcmSwParams
defaultSwParams = PcmSwParams
   { pcmSwParamsTimeStamp        = 0 -- PcmTimeStampNone
   , pcmSwParamsTimeStampType    = 0
   , pcmSwParamsPeriodStep       = 1
   , pcmSwParamsSleepMin         = 0
   , pcmSwParamsAvailMin         = period_size
   , pcmSwParamsXFerAlign        = 1
   , pcmSwParamsStartThreshold   = 1
   , pcmSwParamsStopThreshold    = buffer_size
   , pcmSwParamsSilenceThreshold = 0
   , pcmSwParamsSilenceSize      = 0
   , pcmSwParamsBoundary         = boundary
   , pcmSwParamsProtoVersion     = pcmVersion
   , pcmSwParamsReserved         = Vector.replicate 0
   }
   where
      buffer_size = 1024 -- FIXME: take as parameter from hw params
      period_size = 32   -- FIXME: ditto

      -- TODO: understand and document this
      boundary = go buffer_size
         where
            max_boundary = maxBound - buffer_size
            go x = case x*2 of
               x' | x' <= buffer_size  -> x -- handle overflow
                  | x' <= max_boundary -> go x'
                  | otherwise          -> x


-- | PCM configuration
data PcmConfig = PcmConfig
   { pcmConfigAccess              :: Set PcmAccess
   , pcmConfigFormat              :: Set PcmFormat
   , pcmConfigSubFormat           :: Set PcmSubFormat
   , pcmConfigRate                :: Ratio Word32
   , pcmConfigFifoSize            :: !Word64
   , pcmConfigMostSignificantBits :: !Word32
   , pcmConfigIntervals           :: Map PcmHwParamInterval Interval
   }
   deriving (Show)

-- | Convert raw PCM hw params into PcmConfig
toConfig :: PcmHwParams -> PcmConfig
toConfig params = PcmConfig
   { pcmConfigAccess              = fromMask m1
   , pcmConfigFormat              = fromMask m2
   , pcmConfigSubFormat           = fromMask m3
   , pcmConfigRate                = pcmHwParamsRateNumerator params % pcmHwParamsRateDenominator params
   , pcmConfigFifoSize            = pcmHwParamsFifoSize params
   , pcmConfigMostSignificantBits = pcmHwParamsMostSignificantBits params
   , pcmConfigIntervals           = fromIntervals
   }
   where
      fromMask :: forall a. (Ord a, Bounded a, Enum a, BitOffset a) => Mask -> Set a
      fromMask (Mask v) = Set.fromList (BitSet.enumerateSetBits v)

      fromIntervals = Map.fromList $ zip
                        [PcmHwParamSampleBits ..]
                        (Vector.toList (pcmHwParamsIntervals params))

      m1:m2:m3:_ = Vector.toList (pcmHwParamsMasks params)
