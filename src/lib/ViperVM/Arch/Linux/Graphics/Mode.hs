{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric   #-}

-- We need this one to use type literal numbers (S (S .. Z)) of size 32
{-# OPTIONS -fcontext-stack=50 #-}

-- | Display mode (resolution, refresh rate, etc.)
module ViperVM.Arch.Linux.Graphics.Mode
   ( Mode(..)
   , ModeStruct(..)
   , toMode
   , fromMode
   )
where

import Foreign.Storable
import Foreign.CStorable
import Data.Word
import Foreign.C.String 
   ( castCCharToChar
   , castCharToCChar
   )
import Control.Applicative ((<$>))
import Foreign.C.Types (CChar)
import Foreign.Ptr (castPtr)
import Data.Vector.Fixed.Cont (S,Z)
import Data.Vector.Fixed.Storable (Vec)
import Data.Vector.Fixed (toList, fromList)
import GHC.Generics (Generic)

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
   peek v      = toMode <$> (cPeek $ castPtr v)
   poke p v    = cPoke (castPtr p) (fromMode v)

-- | Constant DRM_DISPLAY_MODE_LEN = 32
type DisplayModeLength = -- 32 
   S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (
   S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z
   )))))))))))))))))))))))))))))))

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

toMode :: ModeStruct -> Mode
toMode (ModeStruct {..}) =
   let extractName (Storable x) = 
         takeWhile (/= '\0') (fmap castCCharToChar (toList x))
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

fromMode :: Mode -> ModeStruct
fromMode (Mode {..}) =
   let modeName' = fromList (fmap castCharToCChar modeName)

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
