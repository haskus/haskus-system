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

import Foreign.Storable
import Foreign.CStorable
import Data.Word
import Foreign.Ptr (castPtr)
import Foreign.C.String 
   ( castCCharToChar
   , castCharToCChar
   )
import qualified ViperVM.Format.Binary.Vector as Vec

import ViperVM.Format.Binary.BitSet
import ViperVM.Arch.Linux.Graphics.Internals

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
      extractName = takeWhile (/= '\0') . fmap castCCharToChar . Vec.toList
      (flgs,flg3d) = toModeFlag miFlags
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
      , modeType                = fromBits miType
      , modeName                = extractName miName
      }

toStructMode :: Mode -> StructMode
toStructMode Mode {..} =
   let
      modeName' = Vec.fromFilledListZ (castCharToCChar '\0')
                     (fmap castCharToCChar modeName)

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
      , miFlags      = fromModeFlag modeFlags modeStereo3D
      , miType       = toBits modeType
      , miName       = modeName'
      }
