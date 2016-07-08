{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

-- | Display mode (resolution, refresh rate, etc.)
module ViperVM.Arch.Linux.Graphics.Mode
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

import Data.Proxy
import Foreign.Storable
import Foreign.CStorable
import Foreign.Ptr (castPtr)
import ViperVM.Format.Binary.BitField
import ViperVM.Format.Binary.Enum
import ViperVM.Format.Binary.Word
import ViperVM.Format.String

import ViperVM.Arch.Linux.Internals.Graphics

-- | Display mode
data Mode = Mode
   { modeClock               :: Word32

   , modeHorizontalDisplay   :: Word16
   , modeHorizontalSyncStart :: Word16
   , modeHorizontalSyncEnd   :: Word16
   , modeHorizontalTotal     :: Word16
   , modeHorizontalSkew      :: Word16

   , modeVerticalDisplay     :: Word16
   , modeVerticalSyncStart   :: Word16
   , modeVerticalSyncEnd     :: Word16
   , modeVerticalTotal       :: Word16
   , modeVerticalScan        :: Word16

   , modeVerticalRefresh     :: Word32
   , modeFlags               :: ModeFlags
   , modeStereo3D            :: Stereo3D
   , modeType                :: ModeTypes
   , modeName                :: String
   } deriving (Show)

instance Storable Mode where
   sizeOf _    = cSizeOf (undefined :: StructMode)
   alignment _ = cAlignment (undefined :: StructMode)
   peek v      = fromStructMode <$> cPeek (castPtr v)
   poke p v    = cPoke (castPtr p) (toStructMode v)


fromStructMode :: StructMode -> Mode
fromStructMode StructMode {..} =
   let
      flgs  = extractField (Proxy :: Proxy "flags") miFlags
      flg3d = fromEnumField $ extractField (Proxy :: Proxy "stereo3d") miFlags
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
      flgs = updateField (Proxy :: Proxy "flags") modeFlags
           $ updateField (Proxy :: Proxy "stereo3d") (toEnumField modeStereo3D)
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
