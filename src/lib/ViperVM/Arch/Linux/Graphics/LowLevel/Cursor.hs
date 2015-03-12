{-# LANGUAGE DeriveGeneric #-}

-- | Cursor
module ViperVM.Arch.Linux.Graphics.LowLevel.Cursor
   ( CursorMode(..)

   )
where

import Foreign.Storable
import Foreign.CStorable
import Data.Word
import Data.Int
import GHC.Generics (Generic)

import ViperVM.Utils.EnumSet

data CursorMode
   = CursorModeBO
   | CursorModeMove
   deriving (Eq,Enum,Show)

instance EnumBitSet CursorMode

-- | Data matching the C structure drm_mode_cursor
data CursorStruct = CursorStruct
   { curFlags     :: Word32
   , curCrtcId    :: Word32
   , curX         :: Int32
   , curY         :: Int32
   , curWidth     :: Word32
   , curHeight    :: Word32
   , curHandle    :: Word32   -- ^ if 0, turns the cursor off
   } deriving Generic

instance CStorable CursorStruct
instance Storable  CursorStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- | Data matching the C structure drm_mode_cursor2
data Cursor2Struct = Cursor2Struct
   { cur2Flags     :: Word32
   , cur2CrtcId    :: Word32
   , cur2X         :: Int32
   , cur2Y         :: Int32
   , cur2Width     :: Word32
   , cur2Height    :: Word32
   , cur2Handle    :: Word32   -- ^ if 0, turns the cursor off
   , cur2HotX      :: Int32
   , cur2HotY      :: Int32
   } deriving Generic

instance CStorable Cursor2Struct
instance Storable  Cursor2Struct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

