{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- | Object
module Haskus.System.Linux.Graphics.Object
   ( Object(..)
   , ObjectType(..)
   , ObjectNotFound
   , getObjectPropertyCount
   , getObjectProperties
   , setObjectProperty
   , setObjectProperty'
   )
where

import Haskus.System.Linux.Graphics.FrameBuffer
import Haskus.System.Linux.Graphics.Mode
import Haskus.System.Linux.Graphics.State
import Haskus.System.Linux.Graphics.IDs
import Haskus.System.Linux.Graphics.Property
import Haskus.System.Linux.Internals.Graphics
import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Error
import Haskus.System.Linux.Handle
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Ptr
import Haskus.Format.Binary.Enum
import Haskus.Utils.Flow

data InvalidCount   = InvalidCount Int
data ObjectNotFound = ObjectNotFound deriving (Show,Eq)

data ObjectType
   = ObjectController
   | ObjectConnector
   | ObjectEncoder
   | ObjectMode
   | ObjectProperty
   | ObjectFrameBuffer
   | ObjectBlob
   | ObjectPlane
   deriving (Show,Eq,Ord,Enum)

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

instance Object Plane where
   getObjectType _ = ObjectPlane
   getObjectID x   = y
      where PlaneID y = planeID x


-- | Get object's number of properties
getObjectPropertyCount :: (MonadIO m, Object o) => Handle -> o -> Flow m '[Word32, ErrorCode]
getObjectPropertyCount hdl o = do
      liftIO (ioctlGetObjectProperties s hdl)
         >.-.> gopCountProps
   where
      s = StructGetObjectProperties 0 0 0
            (getObjectID o)
            (fromCEnum (getObjectType o))

-- | Return object properties
getObjectProperties :: forall m o. (MonadInIO m, Object o) => Handle -> o -> Flow m '[[RawProperty],ObjectNotFound,InvalidParam]
getObjectProperties hdl o =
       -- we assume 20 entries is usually enough and we adapt if it isn't. By
       -- using an initial value we avoid a syscall in most cases.
      fixCount go 20
   where
      fixCount f n = f n >%~^> \(InvalidCount n') -> fixCount f n'

      allocaArray' 0 f = f nullPtr
      allocaArray' n f = allocaArray (fromIntegral n) f

      go :: Int -> Flow m '[[RawProperty],InvalidCount,InvalidParam,ObjectNotFound]
      go n =
         allocaArray' n $ \(propsPtr :: Ptr Word32) ->
         allocaArray' n $ \(valsPtr :: Ptr Word64) -> do
            let
               s = StructGetObjectProperties 
                     (fromIntegral (ptrToWordPtr propsPtr))
                     (fromIntegral (ptrToWordPtr valsPtr))
                     (fromIntegral n)
                     (getObjectID o)
                     (fromCEnum (getObjectType o))
            getObjectProperties' s
               >.~|> checkCount n
               >.~.> extractProperties

      getObjectProperties' :: StructGetObjectProperties -> Flow m '[StructGetObjectProperties,InvalidParam,ObjectNotFound]
      getObjectProperties' s = liftIO (ioctlGetObjectProperties s hdl) >%~^> \case
         EINVAL -> flowSet InvalidParam
         ENOENT -> flowSet ObjectNotFound
         e      -> unhdlErr "getObjectProperties" e

      extractProperties :: StructGetObjectProperties -> m [RawProperty]
      extractProperties s = do
         let n        = fromIntegral (gopCountProps s)
             propsPtr :: Ptr Word32
             propsPtr = wordPtrToPtr (fromIntegral (gopPropsPtr s))
             valsPtr :: Ptr Word64
             valsPtr  = wordPtrToPtr (fromIntegral (gopValuesPtr s))
         ps <- peekArray n propsPtr
         vs <- peekArray n valsPtr
         return (zipWith RawProperty ps vs)

      -- check that we have allocated enough entries to store the properties
      checkCount :: Int -> StructGetObjectProperties -> Flow m '[StructGetObjectProperties,InvalidCount]
      checkCount n s = do
         let n' = fromIntegral (gopCountProps s)
         if n' > n
            then flowSet (InvalidCount n)
            else flowSet s

-- | Set an object property
setObjectProperty ::
   ( Object o
   , MonadInIO m
   ) => Handle -> o -> PropID -> PropValue -> Flow m '[(),InvalidParam,ObjectNotFound]
setObjectProperty hdl o prop val =
   setObjectProperty' hdl (getObjectID o) (getObjectType o) prop val

-- | Set an object property
setObjectProperty' ::
   ( MonadInIO m
   ) => Handle -> ObjectID -> ObjectType -> PropID -> PropValue -> Flow m '[(),InvalidParam,ObjectNotFound]
setObjectProperty' hdl oid otyp prop val = do
   let s = StructSetObjectProperty val prop oid (fromCEnum otyp)
   liftIO (ioctlSetObjectProperty s hdl)
      >.-.> const ()
      >%~^> \case
         EINVAL -> flowSet InvalidParam
         ENOENT -> flowSet ObjectNotFound
         e      -> unhdlErr "setObjectProperty" e
