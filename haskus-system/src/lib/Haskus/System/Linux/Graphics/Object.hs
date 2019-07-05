{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Object
module Haskus.System.Linux.Graphics.Object
   ( Object(..)
   , ObjectType(..)
   , ObjectNotFound
   , RawProperty (..)
   , showObjectQualifiedID
   , showObjectType
   , getObjectPropertyCount
   , getObjectRawProperties
   , getHandleObjectProperties
   , setObjectRawProperty
   , setObjectIDRawProperty
   )
where

import Haskus.System.Linux.Graphics.Frame
import Haskus.System.Linux.Graphics.Mode
import Haskus.System.Linux.Graphics.State
import Haskus.System.Linux.Graphics.Entities
import Haskus.System.Linux.Internals.Graphics
import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Error
import Haskus.System.Linux.Handle
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.Word
import Foreign.Ptr
import Haskus.Format.Binary.Enum
import Haskus.Utils.Flow

data InvalidCount   = InvalidCount Int
data ObjectNotFound = ObjectNotFound deriving (Show,Eq)


class Object a where
   getObjectType :: a -> ObjectType
   getObjectID   :: a -> Word32

instance Object Controller where
   getObjectType _ = ObjectController
   getObjectID     = unEntityID . controllerID

instance Object Connector where
   getObjectType _ = ObjectConnector
   getObjectID     = unEntityID . connectorID

instance Object Encoder where
   getObjectType _ = ObjectEncoder
   getObjectID     = unEntityID . encoderID

instance Object Mode where
   getObjectType _ = ObjectMode
   getObjectID _   = error "getObjectID unsupported for Mode objects"

instance Object (Frame b) where
   getObjectType _ = ObjectFrame
   getObjectID     = unEntityID . frameID

instance Object StructFrameCommand where
   getObjectType _ = ObjectFrame
   getObjectID     = fcFbId

instance Object Plane where
   getObjectType _ = ObjectPlane
   getObjectID     = unEntityID . planeID

instance Object ControllerID where
   getObjectType _ = ObjectController
   getObjectID     = unEntityID

instance Object ConnectorID where
   getObjectType _ = ObjectConnector
   getObjectID     = unEntityID

instance Object EncoderID where
   getObjectType _ = ObjectEncoder
   getObjectID     = unEntityID

instance Object FrameID where
   getObjectType _ = ObjectFrame
   getObjectID     = unEntityID

instance Object PlaneID where
   getObjectType _ = ObjectPlane
   getObjectID     = unEntityID

-- | Get a string with object type and ID
showObjectQualifiedID :: Object a => a -> String
showObjectQualifiedID a = showObjectType (getObjectType a) ++ " " ++ show (getObjectID a)

-- | Show object type string
showObjectType :: ObjectType -> String
showObjectType = \case
   ObjectController  -> "Controller"
   ObjectConnector   -> "Connector"
   ObjectEncoder     -> "Encoder"
   ObjectMode        -> "Mode"
   ObjectProperty    -> "Property"
   ObjectFrame       -> "Frame"
   ObjectBlob        -> "Blob"
   ObjectPlane       -> "Plane"

-- | Get object's number of properties
getObjectPropertyCount :: (MonadInIO m, Object o) => Handle -> o -> Excepts '[ErrorCode] m Word32
getObjectPropertyCount hdl o = ioctlGetObjectProperties s hdl ||> gopCountProps
   where
      s = StructGetObjectProperties 0 0 0
            (getObjectID o)
            (fromCEnum (getObjectType o))

-- | Return object raw properties
getObjectRawProperties :: forall m o. (MonadInIO m, Object o) => Handle -> o -> Excepts '[InvalidParam,ObjectNotFound] m [RawProperty]
getObjectRawProperties hdl o =
       -- we assume 20 entries is usually enough and we adapt if it isn't. By
       -- using an initial value we avoid a syscall in most cases.
      fixCount go 20
   where
      fixCount f n = f n |> catchRemove (\(InvalidCount n') -> fixCount f n')

      allocaArray' 0 f = f nullPtr
      allocaArray' n f = allocaArray (fromIntegral n) f

      go :: Int -> Excepts '[InvalidCount,InvalidParam,ObjectNotFound] m [RawProperty]
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
            ps <- liftE (getObjectProperties' s)
            liftE (checkCount n ps)
            lift (extractProperties ps)

      getObjectProperties' :: StructGetObjectProperties -> Excepts '[InvalidParam,ObjectNotFound] m StructGetObjectProperties
      getObjectProperties' s = ioctlGetObjectProperties s hdl
                                 |> catchLiftLeft \case
                                       EINVAL -> throwE InvalidParam
                                       ENOENT -> throwE ObjectNotFound
                                       e      -> unhdlErr "getObjectProperties" e

      extractProperties :: StructGetObjectProperties -> m [RawProperty]
      extractProperties s = do
         let n        = fromIntegral (gopCountProps s)
             propsPtr :: Ptr Word32
             propsPtr = wordPtrToPtr (fromIntegral (gopPropsPtr s))
             valsPtr :: Ptr Word64
             valsPtr  = wordPtrToPtr (fromIntegral (gopValuesPtr s))
         ps <- peekArray n propsPtr |||> EntityID
         vs <- peekArray n valsPtr
         return (zipWith RawProperty ps vs)

      -- check that we have allocated enough entries to store the properties
      checkCount :: Int -> StructGetObjectProperties -> Excepts '[InvalidCount] m ()
      checkCount n s = do
         let n' = fromIntegral (gopCountProps s)
         if n' > n
            then failureE (InvalidCount n)
            else return ()

-- | Get object properties
--
-- Use `getObjectProperties` instead as it uses the meta-data cache.
getHandleObjectProperties :: forall m o.
   ( MonadInIO m
   , Object o
   ) => Handle -> o -> Excepts '[InvalidParam,ObjectNotFound] m [Property]
getHandleObjectProperties hdl obj = do
   props <- getObjectRawProperties hdl obj
   forMaybeM props \raw -> do
      getPropertyMeta hdl (rawPropertyID raw)
      ||> Just
      -- we return Nothing in case of error
      |> catchEvalE (const (pure Nothing))
      -- wrap it in a Property
      |||> \meta -> Property meta (rawPropertyValue raw)

-- | Set a raw property of an object
setObjectRawProperty ::
   ( Object o
   , MonadInIO m
   ) => Handle -> o -> RawProperty -> Excepts '[InvalidParam,ObjectNotFound] m ()
setObjectRawProperty hdl o prop =
   setObjectIDRawProperty hdl (getObjectID o) (getObjectType o) prop

-- | Set a property of an object identified by object ID and object type
setObjectIDRawProperty ::
   ( MonadInIO m
   ) => Handle -> ObjectID -> ObjectType -> RawProperty -> Excepts '[InvalidParam,ObjectNotFound] m ()
setObjectIDRawProperty hdl oid otyp prop = do
   let s = StructSetObjectProperty
            { sopValue   = rawPropertyValue prop
            , sopPropId  = unEntityID (rawPropertyID prop)
            , sopObjId   = oid
            , sopObjType = fromCEnum otyp
            }
   void (ioctlSetObjectProperty s hdl)
      |> catchLiftLeft \case
            EINVAL -> throwE InvalidParam
            ENOENT -> throwE ObjectNotFound
            e      -> unhdlErr "setObjectIDRawProperty" e
