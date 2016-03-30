{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

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

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Handle

import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Encoder
import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Graphics.Property
import ViperVM.Arch.Linux.Internals.Graphics
import ViperVM.Format.Binary.Enum

import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM2,forM)
import Control.Monad.Trans.Either
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
getConnectorFromID :: Handle -> ConnectorID -> SysRet Connector
getConnectorFromID hdl connId@(ConnectorID cid) = runEitherT $ do
   let 
      res = StructGetConnector 0 0 0 0 0 0 0 0 cid
               (toEnumField ConnectorTypeUnknown) 0 0 0 0
               (toEnumField SubPixelNone)

      allocaArray' :: (Integral c, Storable a) => c -> (Ptr a -> IO b) -> IO b
      allocaArray'      = allocaArray . fromIntegral

      peekArray' :: (Storable a, Integral c) => c -> Ptr a -> IO [a]
      peekArray'        = peekArray . fromIntegral

      getModeConnector' r = EitherT (ioctlGetConnector r hdl)

   -- First we get the number of each resource
   res2 <- getModeConnector' res

   -- then we allocate arrays of appropriate sizes
   (rawRes, retRes) <-
      EitherT $ allocaArray' (connModesCount res2) $ \(ms :: Ptr StructMode) ->
         allocaArray' (connPropsCount res2) $ \(ps :: Ptr Word32) ->
            allocaArray' (connPropsCount res2) $ \(pvs :: Ptr Word64) ->
               allocaArray' (connEncodersCount res2) $ \(es:: Ptr Word32) -> runEitherT $ do
                  -- we put them in a new struct
                  let
                     cv = fromIntegral . ptrToWordPtr
                     res3 = res2 { connEncodersPtr   = cv es
                                 , connModesPtr      = cv ms
                                 , connPropsPtr      = cv ps
                                 , connPropValuesPtr = cv pvs
                                 }

                  -- we get the values
                  let wrapZero 0 = Nothing
                      wrapZero x = Just x
                  res4 <- getModeConnector' res3

                  state <- case connConnection_ res4 of
                     1 -> do
                           -- properties
                           rawProps <-liftIO $ liftM2 RawProperty
                                       <$> peekArray' (connPropsCount res2) ps
                                       <*> peekArray' (connPropsCount res2) pvs
                           props <- forM rawProps $ \raw -> do
                              --FIXME: store property meta in the card
                              meta <- EitherT $ getPropertyMeta hdl (rawPropertyMetaID raw)
                              return (Property meta (rawPropertyValue raw))

                           modes <- liftIO (fmap fromStructMode <$> peekArray' (connModesCount res2) ms)
                           return $ Connected $ ConnectedDevice
                              modes
                              (connWidth_ res4)
                              (connHeight_ res4)
                              (fromEnumField (connSubPixel_ res4))
                              props
                              
                     2 -> return Disconnected
                     _ -> return ConnectionUnknown

                  encs  <- liftIO (fmap EncoderID      <$> peekArray' (connEncodersCount res2) es)

                  let res5 = Connector
                        (ConnectorID (connConnectorID_ res4))
                        (fromEnumField (connConnectorType_ res4))
                        (connConnectorTypeID_ res4)
                        state
                        encs
                        (EncoderID <$> wrapZero (connEncoderID_ res4))
                        hdl

                  right (res4, res5)

   -- we need to check that the number of resources is still the same (as
   -- resources may have appeared between the time we get the number of
   -- resources and the time we get them...)
   -- If not, we redo the whole process
   if   connModesCount    res2 < connModesCount    rawRes
     || connPropsCount    res2 < connPropsCount    rawRes
     || connEncodersCount res2 < connEncodersCount rawRes
      then EitherT $ getConnectorFromID hdl connId
      else right retRes



-- | Get connectors (discard errors)
getConnectors :: Handle -> IO [Connector]
getConnectors = getEntities resConnectorIDs getConnectorFromID


-- | Encoder attached to the connector, if any
connectorEncoder :: Connector -> SysRet (Maybe Encoder)
connectorEncoder conn = case connectorEncoderID conn of
   Nothing    -> return (Right Nothing)
   Just encId -> fmap Just <$> getEncoderFromID (connectorHandle conn) encId

-- | Retrieve Controller (and encoder) controling a connector (if any)
connectorController :: Connector -> SysRet (Maybe Controller, Maybe Encoder)
connectorController conn = runEitherT $ 
   EitherT (connectorEncoder conn) >>= \case
      Nothing  -> right (Nothing,Nothing)
      Just enc -> EitherT (encoderController enc) >>= \case
         Nothing   -> right (Nothing,Just enc)
         Just cont -> right (Just cont,Just enc)

