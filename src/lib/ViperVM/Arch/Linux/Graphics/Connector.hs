{-# LANGUAGE ScopedTypeVariables
           , GeneralizedNewtypeDeriving
           , DeriveGeneric
           , RecordWildCards #-}

-- | Graphic card connector management
module ViperVM.Arch.Linux.Graphics.Connector
   ( Connector(..)
   , Connection(..)
   , SubPixel(..)
   , getConnector
   )
where

import ViperVM.Arch.Linux.Graphics.LowLevel 
   ( ConnectorStruct(..)
   , ConnectorType
   )

import Control.Applicative ((<$>), (<*>))
import Data.Word
import Foreign.Storable
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM2)
import Foreign.Marshal.Array (peekArray, allocaArray)
import Foreign.Ptr

import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.IDs

-- | Connector property
data ConnectorProperty = ConnectorProperty Word32 Word64 deriving (Show)

-- | Indicate if a cable is plugged in the connector
data Connection
   = Connected          -- ^ The connector is connected to a displaying device
   | Disconnected       -- ^ The connector is disconnected
   | ConnectionUnknown  -- ^ The connection state cannot be determined
   deriving (Eq,Ord,Show)

-- | Indicate how a pixel is physically subdivised in RGB pixel elements
data SubPixel
   = SubPixelUnknown
   | SubPixelHorizontalRGB
   | SubPixelHorizontalBGR
   | SubPixelVerticalRGB
   | SubPixelVerticalBGR
   | SubPixelNone
   deriving (Eq,Ord,Enum,Show)

-- | A connector on the graphic card
data Connector = Connector
   { connEncoders          :: [EncoderID]
   , connModes             :: [Mode]
   , connProperties        :: [ConnectorProperty]
   , connEncoderID         :: Maybe EncoderID
   , connConnectorID       :: ConnectorID
   , connConnectorType     :: ConnectorType
   , connConnectorTypeID   :: ConnectorTypeID

   , connConnection        :: Connection           -- ^ Connection state
   , connWidth             :: Word32               -- ^ Width (in millimeters)
   , connHeight            :: Word32               -- ^ Height (in millimeters)
   , connSubPixel          :: SubPixel
   } deriving (Show)


-- | Get connector
getConnector :: IOCTL -> FileDescriptor -> ConnectorID -> SysRet Connector
getConnector ioctl fd connId@(ConnectorID cid) = runEitherT $ do
   let 
      res = ConnectorStruct 0 0 0 0 0 0 0 0 cid 0 0 0 0 0 0

      allocaArray' :: (Integral c, Storable a) => c -> (Ptr a -> IO b) -> IO b
      allocaArray'      = allocaArray . fromIntegral

      peekArray' :: (Storable a, Integral c) => c -> Ptr a -> IO [a]
      peekArray'        = peekArray . fromIntegral

      getModeConnector' = EitherT . ioctlReadWrite ioctl 0x64 0xA7 defaultCheck fd

   -- First we get the number of each resource
   res2 <- getModeConnector' res

   -- then we allocate arrays of appropriate sizes
   (rawRes, retRes) <-
      EitherT $ allocaArray' (connModesCount res2) $ \(ms :: Ptr ModeStruct) ->
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
                     isConnected x = case x of
                        1 -> Connected
                        2 -> Disconnected
                        _ -> ConnectionUnknown

                  -- we get the values
                  let wrapZero 0 = Nothing
                      wrapZero x = Just x
                  res4 <- getModeConnector' res3
                  res5 <- liftIO $ Connector
                     <$> (fmap EncoderID <$> peekArray' (connEncodersCount res2) es)
                     <*> (fmap toMode <$> peekArray' (connModesCount res2) ms)
                     <*> (liftM2 ConnectorProperty <$> peekArray' (connPropsCount res2) ps
                                                   <*> peekArray' (connPropsCount res2) pvs)
                     <*> return (EncoderID            <$> wrapZero (connEncoderID_ res4))
                     <*> return (ConnectorID           $ connConnectorID_ res4)
                     <*> return (toEnum . fromIntegral $ connConnectorType_ res4)
                     <*> return (ConnectorTypeID       $ connConnectorTypeID_ res4)
                     <*> return (isConnected           $ connConnection_ res4)
                     <*> return (connWidth_ res4)
                     <*> return (connHeight_ res4)
                     <*> return (toEnum . fromIntegral $ connSubPixel_ res4)

                  right (res4, res5)

   -- we need to check that the number of resources is still the same (a
   -- resources may have appeared between the time we get the number of
   -- resources and the time we get them...)
   -- If not, we redo the whole process
   if   connModesCount    res2 < connModesCount    rawRes
     || connPropsCount    res2 < connPropsCount    rawRes
     || connEncodersCount res2 < connEncodersCount rawRes
      then EitherT $ getConnector ioctl fd connId
      else right retRes
