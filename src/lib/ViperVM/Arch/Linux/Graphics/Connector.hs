{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Graphic card connector management
module ViperVM.Arch.Linux.Graphics.Connector
   ( Connector (..)
   , Connection (..)
   , ConnectedDevice (..)
   , ConnectorType(..)
   , SubPixel(..)
   , connectorEncoder
   , connectorController
   , getConnectors
   )
where

import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Encoder
import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Graphics.Property
import ViperVM.Arch.Linux.Internals.Graphics
import ViperVM.Format.Binary.Enum
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.Storable
import ViperVM.Utils.Flow
import ViperVM.System.Sys

import Control.Monad (liftM2)

-- | Indicate if a cable is plugged in the connector
data Connection
   = Connected ConnectedDevice -- ^ The connector is connected to a displaying device
   | Disconnected              -- ^ The connector is disconnected
   | ConnectionUnknown         -- ^ The connection state cannot be determined
   deriving (Show)

-- | Information about the connected device
data ConnectedDevice = ConnectedDevice
   { connectedDeviceModes        :: [Mode]     -- ^ Supported modes
   , connectedDeviceWidth        :: Word32     -- ^ Width (in millimeters)
   , connectedDeviceHeight       :: Word32     -- ^ Height (in millimeters)
   , connectedDeviceSubPixel     :: SubPixel   -- ^ Sub-pixel structure
   , connectedDeviceProperties   :: [Property] -- ^ Properties of the connector
   } deriving (Show)
   

-- | A connector on the graphic card
data Connector = Connector
   { connectorID                 :: ConnectorID          -- ^ ID
   , connectorType               :: ConnectorType        -- ^ Type of connector
   , connectorByTypeIndex        :: Word32               -- ^ Identifier within connectors of the same type
   , connectorState              :: Connection           -- ^ Connection state
   , connectorPossibleEncoderIDs :: [EncoderID]          -- ^ IDs of the encoders that can work with this connector
   , connectorEncoderID          :: Maybe EncoderID      -- ^ Currently used encoder
   , connectorHandle             :: Handle               -- ^ Graphic card
   } deriving (Show)

getConnector' :: Handle -> StructGetConnector -> Flow Sys '[StructGetConnector,InvalidParam,EntryNotFound]
getConnector' hdl r = sysIO (ioctlGetConnector r hdl) >%~^> \case
   EINVAL -> flowSet InvalidParam
   ENOENT -> flowSet EntryNotFound
   e      -> unhdlErr "getModeConnector" e

-- | Get connector
getConnectorFromID :: Handle -> ConnectorID -> Flow Sys '[Connector,InvalidParam,EntryNotFound,InvalidProperty]
getConnectorFromID hdl connId@(ConnectorID cid) = getConnector' hdl res >.~^> getValues
   where
      res = StructGetConnector 0 0 0 0 0 0 0 0 cid
               (toEnumField ConnectorTypeUnknown) 0 0 0 0
               (toEnumField SubPixelNone)

      getValues :: StructGetConnector -> Flow Sys '[Connector,InvalidParam,EntryNotFound,InvalidProperty]
      getValues res2 = do
            rawGet hdl res2 >.~^> \(rawRes,conn) ->
               -- we need to check that the number of resources is still the same (as
               -- resources may have appeared between the time we get the number of
               -- resources and the time we get them...)
               -- If not, we redo the whole process
               if   connModesCount    res2 < connModesCount    rawRes
                 || connPropsCount    res2 < connPropsCount    rawRes
                 || connEncodersCount res2 < connEncodersCount rawRes
                  then getConnectorFromID hdl connId
                  else flowSetN @0 conn

rawGet :: Handle -> StructGetConnector -> Flow Sys '[(StructGetConnector,Connector),InvalidParam,InvalidProperty,EntryNotFound]
rawGet hdl res2 = do

   let
      allocaArray' :: (Integral c, Storable a) => c -> (Ptr a -> Sys b) -> Sys b
      allocaArray' n = sysWith (allocaArray (fromIntegral n))


   allocaArray' (connModesCount res2) $ \(ms :: Ptr StructMode) ->
      allocaArray' (connPropsCount res2) $ \(ps :: Ptr Word32) ->
         allocaArray' (connPropsCount res2) $ \(pvs :: Ptr Word64) ->
            allocaArray' (connEncodersCount res2) $ \(es:: Ptr Word32) -> do
               let
                  cv = fromIntegral . ptrToWordPtr
                  res3 = res2 { connEncodersPtr   = cv es
                              , connModesPtr      = cv ms
                              , connPropsPtr      = cv ps
                              , connPropValuesPtr = cv pvs
                              }

               getConnector' hdl res3 
                  >.~^^> \res4 ->
                     parseRes hdl res2 res4 >.-.> (res4,)


parseRes :: Handle -> StructGetConnector -> StructGetConnector -> Flow Sys '[Connector,InvalidParam,InvalidProperty]
parseRes hdl res2 res4 = do
   let
      cv = wordPtrToPtr . fromIntegral

      wrapZero 0 = Nothing
      wrapZero x = Just x

      peekArray' :: (Storable a, Integral c) => c -> Ptr a -> Sys [a]
      peekArray' n ptr = sysIO (peekArray (fromIntegral n) ptr)

   state <- case connConnection_ res4 of
      1 -> do
            -- properties
            rawProps <- liftM2 RawProperty
                        <$> peekArray' (connPropsCount res2) (cv (connPropsPtr res4))
                        <*> peekArray' (connPropsCount res2) (cv (connPropValuesPtr res4))
            props <- flowFor rawProps $ \raw -> do
               --FIXME: store property meta in the card
               getPropertyMeta hdl (rawPropertyMetaID raw)
                  >.-.> \meta -> Property meta (rawPropertyValue raw)

            modes <- fmap fromStructMode <$> peekArray' (connModesCount res2) (cv (connModesPtr res4))

            props .-.> (Connected . ConnectedDevice
               modes
               (connWidth_ res4)
               (connHeight_ res4)
               (fromEnumField (connSubPixel_ res4)))
               
      2 -> flowSetN @0 Disconnected
      _ -> flowSetN @0 ConnectionUnknown

   encs  <- fmap EncoderID <$> peekArray' (connEncodersCount res2) (cv (connEncodersPtr res4))

   state .-.> \st -> Connector
         (ConnectorID (connConnectorID_ res4))
         (fromEnumField (connConnectorType_ res4))
         (connConnectorTypeID_ res4)
         st
         encs
         (EncoderID <$> wrapZero (connEncoderID_ res4))
         hdl


-- | Get connectors
getConnectors :: Handle -> Flow Sys '[[Connector],InvalidParam,EntryNotFound,InvalidProperty,InvalidHandle]
getConnectors hdl = getResources hdl
   >.-.> resConnectorIDs
   >.~^^> flowTraverse (getConnectorFromID hdl)


-- | Encoder attached to the connector, if any
connectorEncoder :: Connector -> Flow Sys '[Maybe Encoder,EntryNotFound,InvalidHandle]
connectorEncoder conn = case connectorEncoderID conn of
   Nothing    -> flowSetN @0 Nothing
   Just encId -> 
      getResources (connectorHandle conn) >.~^> \res ->
         getEncoderFromID (connectorHandle conn) res encId >.-.> Just

-- | Retrieve Controller (and encoder) controling a connector (if any)
connectorController :: Connector -> Flow Sys '[(Maybe Controller, Maybe Encoder),EntryNotFound,InvalidHandle]
connectorController conn =
   connectorEncoder conn >.~^> \enc ->
      case enc of
         Nothing -> flowSetN @0 (Nothing,Nothing)
         Just e  -> encoderController e >.-.> (,enc)
