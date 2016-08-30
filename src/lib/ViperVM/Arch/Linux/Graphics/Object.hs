{-# LANGUAGE DataKinds #-}

-- | Object
module ViperVM.Arch.Linux.Graphics.Object
   ( Object(..)
   , ObjectType(..)
   , getObjectPropertyCount
   )
where

import ViperVM.Format.Binary.Word
import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Graphics.Connector
import ViperVM.Arch.Linux.Graphics.Encoder
import ViperVM.Arch.Linux.Graphics.FrameBuffer
import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Internals.Graphics
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Handle

import ViperVM.Format.Binary.Enum
import ViperVM.Utils.Flow
import ViperVM.System.Sys

data ObjectType
   = ObjectController
   | ObjectConnector
   | ObjectEncoder
   | ObjectMode
   | ObjectProperty
   | ObjectFrameBuffer
   | ObjectBlob
   | ObjectPlane
   deriving (Show,Eq,Enum)

instance CEnum ObjectType where
   toCEnum x = case x of
      0xcccccccc -> ObjectController
      0xc0c0c0c0 -> ObjectConnector
      0xe0e0e0e0 -> ObjectEncoder
      0xdededede -> ObjectMode
      0xb0b0b0b0 -> ObjectProperty
      0xfbfbfbfb -> ObjectFrameBuffer
      0xbbbbbbbb -> ObjectBlob
      0xeeeeeeee -> ObjectPlane
      _          -> error "Invalid object type"

   fromCEnum x = case x of
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
   getObjectID   :: a -> Word32

instance Object Controller where
   getObjectType _ = ObjectController
   getObjectID x   = y
      where ControllerID y = controllerID x

instance Object Connector where
   getObjectType _ = ObjectConnector
   getObjectID x   = y
      where ConnectorID y = connectorID x

instance Object Encoder where
   getObjectType _ = ObjectEncoder
   getObjectID x   = y
      where EncoderID y = encoderID x

instance Object Mode where
   getObjectType _ = ObjectMode
   getObjectID _   = error "getObjectID unsupported for Mode objects"

instance Object FrameBuffer where
   getObjectType _ = ObjectFrameBuffer
   getObjectID x   = y
      where FrameBufferID y = fbID x


-- | Get object's number of properties
getObjectPropertyCount :: Object o => Handle -> o -> Flow Sys '[Word32, ErrorCode]
getObjectPropertyCount hdl o = do
      sysIO (ioctlGetObjectProperties s hdl)
         >.-.> gopCountProps
   where
      s = StructGetObjectProperties 0 0 0
            (getObjectID o)
            (fromCEnum (getObjectType o))
