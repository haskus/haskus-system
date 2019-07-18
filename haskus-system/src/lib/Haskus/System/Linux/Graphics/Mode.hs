{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Display mode (resolution, refresh rate, etc.)
module Haskus.System.Linux.Graphics.Mode
   ( Mode(..)
   , ModeType(..)
   , ModeTypes
   , ModeFlag(..)
   , ModeFlags
   , showMode
   -- * Low level
   , fromStructMode
   , toStructMode
   )
where

import Haskus.Format.Binary.BitField
import qualified Haskus.Format.Binary.BitSet as BitSet
import Haskus.Format.Binary.Enum
import Haskus.Format.Binary.Word
import Foreign.Ptr (castPtr)
import Haskus.Format.Binary.Storable
import Haskus.Format.String
import Haskus.System.Linux.Graphics.KIO
import qualified Haskus.Utils.List as List

-- | Display mode
data Mode = Mode
   { modeClock               :: !Word32

   , modeHorizontalDisplay   :: !Word16
   , modeHorizontalSyncStart :: !Word16
   , modeHorizontalSyncEnd   :: !Word16
   , modeHorizontalTotal     :: !Word16
   , modeHorizontalSkew      :: !Word16

   , modeVerticalDisplay     :: !Word16
   , modeVerticalSyncStart   :: !Word16
   , modeVerticalSyncEnd     :: !Word16
   , modeVerticalTotal       :: !Word16
   , modeVerticalScan        :: !Word16

   , modeVerticalRefresh     :: !Word32
   , modeFlags               :: !BasicModeFlags
   , modeStereo3D            :: !Stereo3D
   , modeRatio               :: !AspectRatio
   , modeType                :: !ModeTypes
   , modeName                :: !String
   } deriving (Show)

-- | Show ModeFlag
showModeFlag :: ModeFlag -> String
showModeFlag = \case
   ModeFlagPHSync      -> "+HSync"
   ModeFlagNHSync      -> "-HSync"
   ModeFlagPVSync      -> "+VSync"
   ModeFlagNVSync      -> "-VSync"
   ModeFlagInterlace   -> "Interlace"
   ModeFlagDoubleScan  -> "Double-scan"
   ModeFlagCSync       -> "CSync"
   ModeFlagPCSync      -> "+CSync"
   ModeFlagNCSync      -> "-CSync"
   ModeFlagHSkew       -> "HSkew"
   ModeFlagBroadCast   -> "Broadcast"
   ModeFlagPixMux      -> "PixMux"
   ModeFlagDoubleClock -> "Clock*2"
   ModeFlagClockDiv2   -> "Clock/2"

-- | Show mode
showMode :: Mode -> String
showMode Mode{..} = mconcat
   [ modeName
   , " "
   , show modeVerticalRefresh
   , "MHz "
   , mconcat (List.intersperse " " (fmap showModeFlag (BitSet.elems modeFlags)))
   , "\n\th: width ", show4 modeHorizontalDisplay
   , " start ", show4 modeHorizontalSyncStart
   , " end ", show4 modeHorizontalSyncEnd
   , " total ", show4 modeHorizontalTotal
   , " skew ", show4 modeHorizontalSkew
   , "\n\tv: width ", show4 modeVerticalDisplay
   , " start ", show4 modeVerticalSyncStart
   , " end ", show4 modeVerticalSyncEnd
   , " total ", show4 modeVerticalTotal
   , " scan ", show4 modeVerticalScan
   ]
   where
      show4 :: Show a => a -> String
      show4 x = go (show x)
      go x
         | length x < 4 = go (' ':x)
         | otherwise    = x

instance Storable Mode where
   sizeOf _    = sizeOfT     @StructMode
   alignment _ = alignmentT  @StructMode
   peekIO v    = fromStructMode <$> peekIO (castPtr v)
   pokeIO p v  = pokeIO (castPtr p) (toStructMode v)


fromStructMode :: StructMode -> Mode
fromStructMode StructMode {..} =
   let
      flgs  = extractField @"flags" miFlags
      flg3d = fromEnumField $ extractField @"stereo3d" miFlags
      ratio = fromEnumField $ extractField @"aspect_ratio" miFlags
   in Mode
      { modeClock               = miClock
      , modeHorizontalDisplay   = miHDisplay
      , modeHorizontalSyncStart = miHSyncStart
      , modeHorizontalSyncEnd   = miHSyncEnd
      , modeHorizontalTotal     = miHTotal
      , modeHorizontalSkew      = miHSkew
      , modeVerticalDisplay     = miVDisplay
      , modeVerticalSyncStart   = miVSyncStart
      , modeVerticalSyncEnd     = miVSyncEnd
      , modeVerticalTotal       = miVTotal
      , modeVerticalScan        = miVScan
      , modeVerticalRefresh     = miVRefresh
      , modeFlags               = flgs
      , modeStereo3D            = flg3d
      , modeRatio               = ratio
      , modeType                = miType
      , modeName                = fromCStringBuffer miName
      }

toStructMode :: Mode -> StructMode
toStructMode Mode {..} =
   let
      flgs = updateField @"flags" modeFlags
           $ updateField @"stereo3d" (toEnumField modeStereo3D)
           $ BitFields 0

   in StructMode
      { miClock      = modeClock
      , miHDisplay   = modeHorizontalDisplay
      , miHSyncStart = modeHorizontalSyncStart
      , miHSyncEnd   = modeHorizontalSyncEnd
      , miHTotal     = modeHorizontalTotal
      , miHSkew      = modeHorizontalSkew
      , miVDisplay   = modeVerticalDisplay
      , miVSyncStart = modeVerticalSyncStart
      , miVSyncEnd   = modeVerticalSyncEnd
      , miVTotal     = modeVerticalTotal
      , miVScan      = modeVerticalScan
      , miVRefresh   = modeVerticalRefresh
      , miFlags      = flgs
      , miType       = modeType
      , miName       = toCStringBuffer modeName
      }
