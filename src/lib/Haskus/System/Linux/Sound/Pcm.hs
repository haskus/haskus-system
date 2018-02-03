-- | PCM devices
module Haskus.System.Linux.Sound.Pcm
   ( anyInterval
   , anyMask
   , anyParams
   )
where

import Haskus.System.Linux.Internals.Sound
import qualified Haskus.Format.Binary.BitSet as BitSet
import qualified Haskus.Format.Binary.Vector as Vector


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
anyMask = Mask
   { maskBits = Vector.replicate 0xffffffff
   }

-- | Any parameter set
anyParams :: PcmHwParams
anyParams = PcmHwParams
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
