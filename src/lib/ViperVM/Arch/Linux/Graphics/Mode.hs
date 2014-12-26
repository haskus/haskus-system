module ViperVM.Arch.Linux.Graphics.Mode
   ( Mode(..)
   )
where

import Foreign.Storable
import Data.Word
import Control.Applicative ((<$>), (<*>))
import Foreign.C.String (peekCString)
import Foreign.Ptr

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
   , modeVerticalSkew        :: Word16

   , modeVerticalRefresh     :: Word32
   , modeFlags               :: Word32
   , modeType                :: Word32
   , modeName                :: String    -- length = DRM_DISPLAY_MODE_LEN = 32
   } deriving (Show)

instance Storable Mode where
   sizeOf _    = 4 + 10 * 2 + 3*4 + 32
   alignment _ = 8
   peek ptr    = Mode
      <$> peekByteOff ptr 0

      <*> peekByteOff ptr 4
      <*> peekByteOff ptr 6
      <*> peekByteOff ptr 8
      <*> peekByteOff ptr 10
      <*> peekByteOff ptr 12

      <*> peekByteOff ptr 14
      <*> peekByteOff ptr 16
      <*> peekByteOff ptr 18
      <*> peekByteOff ptr 20
      <*> peekByteOff ptr 22

      <*> peekByteOff ptr 24
      <*> peekByteOff ptr 28
      <*> peekByteOff ptr 32
      <*> peekCString (castPtr $ ptr `plusPtr` 36)

   poke = undefined

