{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Display mode (resolution, refresh rate, etc.)
module Haskus.System.Linux.Graphics.Mode
   ( Mode(..)
   , ModeType(..)
   , ModeTypes
   , ModeFlag(..)
   , ModeFlags
   -- * Low level
   , fromStructMode
   , toStructMode
   )
where

import Haskus.Format.Binary.BitField
import Haskus.Format.Binary.Enum
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Ptr (castPtr)
import Haskus.Format.Binary.Storable
import Haskus.Format.String
import Haskus.System.Linux.Internals.Graphics

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
   , modeFlags               :: !ModeFlags
   , modeStereo3D            :: !Stereo3D
   , modeType                :: !ModeTypes
   , modeName                :: !String
   } deriving (Show)

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
