{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

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
import ViperVM.Utils.Flow
import ViperVM.System.Sys

import Control.Monad (liftM2,forM)
import Data.Word
import Foreign.Marshal.Array (peekArray, allocaArray)
import Foreign.Ptr
import Foreign.Storable

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

-- | Get connector
getConnectorFromID :: Handle -> ConnectorID -> Sys (Flow '[InvalidParam,EntryNotFound,InvalidProperty] Connector)
getConnectorFromID hdl connId@(ConnectorID cid) = runFlowT $ do
      g <- liftFlowT (getConnector' res)
      liftFlowT (getValues g)
   where
      res = StructGetConnector 0 0 0 0 0 0 0 0 cid
               (toEnumField ConnectorTypeUnknown) 0 0 0 0
               (toEnumField SubPixelNone)

      allocaArray' :: (Integral c, Storable a) => c -> (Ptr a -> Sys b) -> Sys b
      allocaArray' n = sysWith (allocaArray (fromIntegral n))

      peekArray' :: (Storable a, Integral c) => c -> Ptr a -> Sys [a]
      peekArray' n ptr = sysIO (peekArray (fromIntegral n) ptr)

      getConnector' :: StructGetConnector -> Sys (Flow '[InvalidParam,EntryNotFound] StructGetConnector)
      getConnector' r = sysIO (ioctlGetConnector r hdl) >>= \case
         Left EINVAL -> flowSet InvalidParam
         Left ENOENT -> flowSet EntryNotFound
         Right g     -> flowRet g
         Left e      -> unhdlErr "getModeConnector" e


      getValues :: StructGetConnector -> Sys (Flow '[InvalidParam,EntryNotFound,InvalidProperty] Connector)
      getValues res2 = runFlowT $ do
            (rawRes,conn) <- liftFlowT rawGet
            -- we need to check that the number of resources is still the same (as
            -- resources may have appeared between the time we get the number of
            -- resources and the time we get them...)
            -- If not, we redo the whole process
            liftFlowT $ if connModesCount    res2 < connModesCount    rawRes
              || connPropsCount    res2 < connPropsCount    rawRes
              || connEncodersCount res2 < connEncodersCount rawRes
               then getConnectorFromID hdl connId
               else flowRet conn
         where
            rawGet :: Sys (Flow '[InvalidParam,EntryNotFound,InvalidProperty] (StructGetConnector, Connector))
            rawGet = allocaArray' (connModesCount res2) $ \(ms :: Ptr StructMode) ->
               allocaArray' (connPropsCount res2) $ \(ps :: Ptr Word32) ->
                  allocaArray' (connPropsCount res2) $ \(pvs :: Ptr Word64) ->
                     allocaArray' (connEncodersCount res2) $ \(es:: Ptr Word32) -> runFlowT $ do
                        let
                           cv = fromIntegral . ptrToWordPtr
                           res3 = res2 { connEncodersPtr   = cv es
                                       , connModesPtr      = cv ms
                                       , connPropsPtr      = cv ps
                                       , connPropValuesPtr = cv pvs
                                       }

                        res4 <- liftFlowT $ getConnector' res3
                        res5 <- liftFlowT $ parseRes res2 res4
                        return (res4, res5)


      wrapZero 0 = Nothing
      wrapZero x = Just x

      parseRes :: StructGetConnector -> StructGetConnector -> Sys (Flow '[InvalidParam,EntryNotFound,InvalidProperty] Connector)
      parseRes res2 res4 = runFlowT $ do
         let cv = wordPtrToPtr . fromIntegral

         state <- case connConnection_ res4 of
            1 -> do
                  -- properties
                  rawProps <- liftFlowM (liftM2 RawProperty
                              <$> peekArray' (connPropsCount res2) (cv (connPropsPtr res4))
                              <*> peekArray' (connPropsCount res2) (cv (connPropValuesPtr res4)))
                  props <- forM rawProps $ \raw -> do
                     --FIXME: store property meta in the card
                     meta <- liftFlowT $ getPropertyMeta hdl (rawPropertyMetaID raw)
                     return (Property meta (rawPropertyValue raw))

                  modes <- fmap fromStructMode <$> liftFlowM (peekArray' (connModesCount res2) (cv (connModesPtr res4)))
                  return $ Connected $ ConnectedDevice
                     modes
                     (connWidth_ res4)
                     (connHeight_ res4)
                     (fromEnumField (connSubPixel_ res4))
                     props
                     
            2 -> return Disconnected
            _ -> return ConnectionUnknown

         encs  <- fmap EncoderID <$> liftFlowM (peekArray' (connEncodersCount res2) (cv (connEncodersPtr res4)))

         return $ Connector
               (ConnectorID (connConnectorID_ res4))
               (fromEnumField (connConnectorType_ res4))
               (connConnectorTypeID_ res4)
               state
               encs
               (EncoderID <$> wrapZero (connEncoderID_ res4))
               hdl


-- | Get connectors (discard errors)
getConnectors :: Handle -> Sys (Flow '[InvalidHandle,InvalidParam,EntryNotFound,InvalidProperty] [Connector])
getConnectors hdl = runFlowT $ do
   res <- liftFlowT $ getResources hdl
   forM (resConnectorIDs res) (liftFlowT . getConnectorFromID hdl)


-- | Encoder attached to the connector, if any
connectorEncoder :: Connector -> Sys (Flow '[InvalidHandle,InvalidParam,EntryNotFound] (Maybe Encoder))
connectorEncoder conn = runFlowT $ do
   res <- liftFlowT $ getResources (connectorHandle conn)
   liftFlowT $ case connectorEncoderID conn of
      Nothing    -> flowRet Nothing
      Just encId -> getEncoderFromID (connectorHandle conn) res encId `flowSeqM` (return . Just)

-- | Retrieve Controller (and encoder) controling a connector (if any)
connectorController :: Connector -> Sys (Flow '[InvalidHandle,InvalidParam,EntryNotFound] (Maybe Controller, Maybe Encoder))
connectorController conn = runFlowT $ do
   enc <- liftFlowT (connectorEncoder conn)
   ctr <- liftFlowT $ case enc of
      Nothing -> flowRet Nothing
      Just e  -> encoderController e
   return (ctr,enc)
