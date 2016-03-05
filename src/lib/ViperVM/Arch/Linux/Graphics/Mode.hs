{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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
   , ModeStruct(..)
   , emptyModeStruct
   , fromModeStruct
   , toModeStruct
   , ModeCmdStruct(..)
   )
where

import Foreign.Storable
import Foreign.CStorable
import Data.Word
import GHC.Generics (Generic)
import Foreign.Ptr (castPtr)
import Foreign.C.String 
   ( castCCharToChar
   , castCharToCChar
   )
import Foreign.C.Types (CChar)
import ViperVM.Format.Binary.Vector (Vector)
import qualified ViperVM.Format.Binary.Vector as Vec

import ViperVM.Format.Binary.BitSet

type ModeName = Vector 32 CChar

data ModeType
   = ModeTypeBuiltin
   | ModeTypeClockC
   | ModeTypeControllerC
   | ModeTypePreferred
   | ModeTypeDefault
   | ModeTypeUserDef
   | ModeTypeDriver
   deriving (Show,Enum,EnumBitSet)

type ModeTypes = BitSet Word32 ModeType

data ModeFlag
   = ModeFlagPHSync
   | ModeFlagNHSync
   | ModeFlagPVSync
   | ModeFlagNVSync
   | ModeFlagInterlace
   | ModeFlagDoubleScan
   | ModeFlagCSync
   | ModeFlagPCSync
   | ModeFlagNCSync
   | ModeFlagHSkew
   | ModeFlagBroadCast
   | ModeFlagPixMux
   | ModeFlagDoubleClock
   | ModeFlagClockDiv2
   | ModeFlag3DFramePacking
   | ModeFlag3DFieldAlternative
   | ModeFlag3DLineAlternative
   | ModeFlag3DSideBySideFull
   | ModeFlag3DLDepth
   | ModeFlag3DLDepthGFXGFXDepth
   | ModeFlag3DTopAndBottom
   | ModeFlag3DSideBySideHalf
   deriving (Show,Enum,EnumBitSet)

type ModeFlags = BitSet Word32 ModeFlag


-- | Data matching the C structure drm_mode_modeinfo
data ModeStruct = ModeStruct
   { miClock         :: Word32
   , miHDisplay, miHSyncStart, miHSyncEnd, miHTotal, miHSkew :: Word16
   , miVDisplay, miVSyncStart, miVSyncEnd, miVTotal, miVScan :: Word16
   , miVRefresh      :: Word32
   , miFlags         :: Word32
   , miType          :: Word32
   , miName          :: ModeName
   } deriving (Generic,CStorable)

instance Storable ModeStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

emptyModeStruct :: ModeStruct
emptyModeStruct = ModeStruct 0 0 0 0 0 0 0 0 0 0 0 0 0 0 (Vec.replicate (castCharToCChar '\0'))

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
   , modeType                :: ModeTypes
   , modeName                :: String    -- length = DRM_DISPLAY_MODE_LEN = 32
   } deriving (Show)

instance Storable Mode where
   sizeOf _    = cSizeOf (undefined :: ModeStruct)
   alignment _ = cAlignment (undefined :: ModeStruct)
   peek v      = fromModeStruct <$> cPeek (castPtr v)
   poke p v    = cPoke (castPtr p) (toModeStruct v)


fromModeStruct :: ModeStruct -> Mode
fromModeStruct ModeStruct {..} =
   let
      extractName :: ModeName -> String
      extractName = takeWhile (/= '\0') . fmap castCCharToChar . Vec.toList
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
      , modeFlags               = fromBits miFlags
      , modeType                = fromBits miType
      , modeName                = extractName miName
      }

toModeStruct :: Mode -> ModeStruct
toModeStruct Mode {..} =
   let
      modeName' = Vec.fromFilledListZ (castCharToCChar '\0')
                     (fmap castCharToCChar modeName)

   in ModeStruct
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
      , miFlags      = toBits modeFlags
      , miType       = toBits modeType
      , miName       = modeName'
      }

-- | Data matching the C structure drm_mode_mode_cmd
data ModeCmdStruct = ModeCmdStruct
   { mcConnId     :: Word32
   , mcMode       :: ModeStruct
   } deriving Generic

instance CStorable ModeCmdStruct
instance Storable  ModeCmdStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke


data ModeFieldPresent
   = PresentTopField
   | PresentBottomField
   deriving (Show,Enum,EnumBitSet)
