{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}


-- We need this one to use type literal numbers (S (S .. Z)) of size 32
{-# OPTIONS -fcontext-stack=50 #-}

module ViperVM.Arch.Linux.Graphics.LowLevel.Mode
   ( Mode(..)
   , ModeType(..)
   , ModeFlag(..)
   , ModeStruct(..)
   , emptyModeStruct
   , fromModeStruct
   , toModeStruct
   )
where

import Foreign.Storable
import Foreign.CStorable
import Data.Word
import GHC.Generics (Generic)
import Data.Bits (shiftL,shiftR)
import Control.Applicative ((<$>))
import Foreign.Ptr (castPtr)
import qualified Data.Vector.Fixed as Vec
import Foreign.C.String 
   ( castCCharToChar
   , castCharToCChar
   )
import Foreign.C.Types (CChar)
import Data.Vector.Fixed.Cont (S,Z)
import Data.Vector.Fixed.Storable (Vec)

import ViperVM.Utils.EnumSet

type N32 = -- 32 
   S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (
   S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z
   )))))))))))))))))))))))))))))))


type PropertyNameLength  = N32

type DisplayModeLength   = N32

data ModeType
   = ModeTypeBuiltin
   | ModeTypeClockC
   | ModeTypeControllerC
   | ModeTypePreferred
   | ModeTypeDefault
   | ModeTypeUserDef
   | ModeTypeDriver
   deriving (Show,Enum)

instance EnumBitSet ModeType

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
   deriving (Show,Enum)

instance EnumBitSet ModeFlag


-- | Data matching the C structure drm_mode_modeinfo
data ModeStruct = ModeStruct
   { miClock         :: Word32
   , miHDisplay, miHSyncStart, miHSyncEnd, miHTotal, miHSkew :: Word16
   , miVDisplay, miVSyncStart, miVSyncEnd, miVTotal, miVScan :: Word16
   , miVRefresh      :: Word32
   , miFlags         :: Word32
   , miType          :: Word32
   , miName          :: StorableWrap (Vec DisplayModeLength CChar)
   } deriving Generic

instance CStorable ModeStruct
instance Storable ModeStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

emptyModeStruct :: ModeStruct
emptyModeStruct = ModeStruct 0 0 0 0 0 0 0 0 0 0 0 0 0 0 (Storable (Vec.fromList (replicate 32 (castCharToCChar '\0'))))

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
   , modeFlags               :: Word32
   , modeType                :: Word32
   , modeName                :: String    -- length = DRM_DISPLAY_MODE_LEN = 32
   } deriving (Show)

instance Storable Mode where
   sizeOf _    = cSizeOf (undefined :: ModeStruct)
   alignment _ = cAlignment (undefined :: ModeStruct)
   peek v      = fromModeStruct <$> (cPeek $ castPtr v)
   poke p v    = cPoke (castPtr p) (toModeStruct v)


fromModeStruct :: ModeStruct -> Mode
fromModeStruct (ModeStruct {..}) =
   let extractName (Storable x) = 
         takeWhile (/= '\0') (fmap castCCharToChar (Vec.toList x))
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
      , modeFlags               = miFlags
      , modeType                = miType
      , modeName                = extractName (miName)
      }

toModeStruct :: Mode -> ModeStruct
toModeStruct (Mode {..}) =
   let modeName' = Vec.fromList (fmap castCharToCChar modeName)

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
      , miFlags      = modeFlags
      , miType       = modeType
      , miName       = Storable modeName'
      }

-- | Type of the property
data PropertyType
   = PropTypePending
   | PropTypeRange
   | PropTypeImmutable
   | PropTypeEnum       -- ^ Enumerated type with text strings
   | PropTypeBlob
   | PropTypeBitmask    -- ^ Bitmask of enumerated types
   | PropTypeObject
   | PropTypeSignedRange
   deriving (Eq,Ord,Show)

toPropType :: Word32 -> PropertyType
toPropType typ =
   case typ of
      -- legacy types: 1 bit per type...
      1  -> PropTypePending
      2  -> PropTypeRange
      4  -> PropTypeImmutable
      8  -> PropTypeEnum
      16 -> PropTypeBlob
      32 -> PropTypeBitmask
      -- newer types, shifted int
      n -> case (n `shiftR` 6) of
         1 -> PropTypeObject
         2 -> PropTypeSignedRange
         _ -> error "Unknown type"

fromPropType :: PropertyType -> Word32
fromPropType typ =
   case typ of
      -- legacy types: 1 bit per type...
      PropTypePending      -> 1
      PropTypeRange        -> 2
      PropTypeImmutable    -> 4
      PropTypeEnum         -> 8
      PropTypeBlob         -> 16
      PropTypeBitmask      -> 32
      -- newer types, shifted int
      PropTypeObject       -> 1 `shiftL` 6
      PropTypeSignedRange  -> 2 `shiftL` 6

-- | Data matching the C structure drm_mode_property_enum
data PropEnumStruct = PropEnumStruct
   { peValue       :: Word64
   , peName        :: StorableWrap (Vec PropertyNameLength CChar)
   } deriving Generic

instance CStorable PropEnumStruct
instance Storable PropEnumStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- | Data matching the C structure drm_mode_get_property
data GetPropStruct = GetPropStruct
   { gpsValuesPtr    :: Word64
   , gpsEnumBlobPtr  :: Word64
   , gpsPropId       :: Word32
   , gpsFlags        :: Word32
   , gpsName         :: StorableWrap (Vec PropertyNameLength CChar)
   , gpsCountValues  :: Word32
   , gpsCountEnumBlobs :: Word32
   } deriving Generic

instance CStorable GetPropStruct
instance Storable GetPropStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- | Data matching the C structure drm_mode_set_property
data SetPropStruct = SetPropStruct
   { spsValue        :: Word64
   , spsPropId       :: Word32
   , spsConnId       :: Word32
   } deriving Generic

instance CStorable SetPropStruct
instance Storable SetPropStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke


-- | Data matching the C structure drm_mode_get_blob
data GetBlobStruct = GetBlobStruct
   { gbBlobId     :: Word32
   , gbLength     :: Word32
   , gbData       :: Word64
   } deriving Generic

instance CStorable GetBlobStruct
instance Storable GetBlobStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

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
   deriving (Show,Enum)

instance EnumBitSet ModeFieldPresent


