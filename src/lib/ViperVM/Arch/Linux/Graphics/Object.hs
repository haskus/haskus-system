-- | Object
module ViperVM.Arch.Linux.Graphics.Object
   ( Object(..)
   , ObjectType(..)
   , toObjectType
   , fromObjectType
   )
where

import Data.Word

import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Graphics.Connector
import ViperVM.Arch.Linux.Graphics.Encoder
import ViperVM.Arch.Linux.Graphics.FrameBuffer
import ViperVM.Arch.Linux.Graphics.Mode

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


class Object a where
   getObjectType :: a -> ObjectType

instance Object Controller where
   getObjectType _ = ObjectController
instance Object Connector where
   getObjectType _ = ObjectConnector
instance Object Encoder where
   getObjectType _ = ObjectEncoder
instance Object Mode where
   getObjectType _ = ObjectMode
instance Object FrameBuffer where
   getObjectType _ = ObjectFrameBuffer
