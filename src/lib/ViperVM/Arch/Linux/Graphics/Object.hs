{-# LANGUAGE DeriveGeneric #-}

-- | Object
module ViperVM.Arch.Linux.Graphics.Object
   ( ObjectType(..)
   )
where

import Foreign.Storable
import Foreign.CStorable
import Data.Word
import GHC.Generics (Generic)

data ObjectType
   = ObjectController
   | ObjectConnector
   | ObjectEncoder
   | ObjectMode
   | ObjectProperty
   | ObjectFrameBuffer
   | ObjectBlob
   | ObjectPlane
   deriving (Show,Eq)

toObjectType :: Word32 -> ObjectType
toObjectType x = case x of
   0xcccccccc -> ObjectController
   0xc0c0c0c0 -> ObjectConnector
   0xe0e0e0e0 -> ObjectEncoder
   0xdededede -> ObjectMode
   0xb0b0b0b0 -> ObjectProperty
   0xfbfbfbfb -> ObjectFrameBuffer
   0xbbbbbbbb -> ObjectBlob
   0xeeeeeeee -> ObjectPlane
   _          -> error "Invalid object type"

fromObjectType :: ObjectType -> Word32
fromObjectType x = case x of
   ObjectController   -> 0xcccccccc 
   ObjectConnector    -> 0xc0c0c0c0 
   ObjectEncoder      -> 0xe0e0e0e0 
   ObjectMode         -> 0xdededede 
   ObjectProperty     -> 0xb0b0b0b0 
   ObjectFrameBuffer  -> 0xfbfbfbfb 
   ObjectBlob         -> 0xbbbbbbbb 
   ObjectPlane        -> 0xeeeeeeee 

-- | Data matching the C structure drm_mode_obj_get_properties
data GetObjPropStruct = GetObjPropStruct
   { gopPropsPtr        :: Word64
   , gopValuesPtr       :: Word64
   , gopCountProps      :: Word32
   , gopObjId           :: Word32
   , gopObjType         :: Word32
   } deriving Generic

instance CStorable GetObjPropStruct
instance Storable GetObjPropStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- | Data matching the C structure drm_mode_obj_set_properties
data SetObjPropStruct = SetObjPropStruct
   { sopValue           :: Word64
   , sopPropId          :: Word32
   , sopObjId           :: Word32
   , sopObjType         :: Word32
   } deriving Generic

instance CStorable SetObjPropStruct
instance Storable SetObjPropStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

