{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Plane
module ViperVM.Arch.Linux.Graphics.Plane
   ( SetPlaneStruct (..)
   , GetPlaneStruct (..)
   , GetPlaneResStruct (..)
   )
where

import Foreign.Storable
import Foreign.CStorable
import Data.Word
import Data.Int
import GHC.Generics (Generic)


-- | Data matching the C structure drm_mode_set_plane
data SetPlaneStruct = SetPlaneStruct
   { spPlaneId       :: Word32
   , spCrtcId        :: Word32
   , spFbId          :: Word32
   , spFlags         :: Word32
   , spCrtcX         :: Int32
   , spCrtcY         :: Int32
   , spCrtcW         :: Word32
   , spCrtcH         :: Word32
   , spSrcX          :: Word32
   , spSrcY          :: Word32
   , spSrcH          :: Word32
   , spSrcW          :: Word32
   } deriving (Generic,CStorable)

instance Storable SetPlaneStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

-- | Data matching the C structure drm_mode_get_plane
data GetPlaneStruct = GetPlaneStruct
   { gpPlaneId       :: Word32
   , gpCrtcId        :: Word32
   , gpFbId          :: Word32
   , gpPossibleCrtcs :: Word32
   , gpGammaSize     :: Word32
   , gpCountFmtTypes :: Word32
   , gpFormatTypePtr :: Word64
   } deriving (Generic,CStorable)

instance Storable GetPlaneStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

-- | Data matching the C structure drm_mode_get_plane_res
data GetPlaneResStruct = GetPlaneResStruct
   { gprsPlaneIdPtr  :: Word64
   , gprsCountPlanes :: Word32
   } deriving (Generic,CStorable)

instance Storable GetPlaneResStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek
