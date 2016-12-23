{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | State of the graphics system
module ViperVM.Arch.Linux.Graphics.State
   ( GraphicsState (..)
   , readGraphicsState
   -- ** Entities
   , Controller (..)
   , Encoder(..)
   , EncoderType(..)
   , FrameBufferPos (..)
   , Connector (..)
   , Connection (..)
   , ConnectedDevice (..)
   , ConnectorType(..)
   , SubPixel(..)
   , Resources(..)
   -- ** IDs
   , FrameBufferID(..)
   , ControllerID(..)
   , ConnectorID(..)
   , EncoderID(..)
   , setController'
   , switchFrameBuffer'
   , getControllers
   , getControllerGamma
   , setControllerGamma
   , encoderController
   , getEncoders
   , getEncoderFromID
   , getControllerFromID
   , fromStructController
   , connectorEncoder
   , connectorController
   , getConnectors
   , getConnectorFromID
   , getResources
   , getEntities
   , pickEncoders
   , pickControllers
   )
where

import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.Property
import ViperVM.Arch.Linux.Graphics.FrameBuffer
import ViperVM.Arch.Linux.Internals.Graphics
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.Handle
import ViperVM.Utils.Memory (peekArrays,allocaArrays,withArrays)
import ViperVM.Utils.Flow
import ViperVM.Utils.Variant
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.Storable
import ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Format.Binary.Enum
import Control.Monad (liftM2)
import qualified Data.Map as Map
import Data.Map (Map)


-- | Graph of graphics entities
data GraphicsState = GraphicsState
   { graphicsConnectors  :: Map ConnectorID  Connector
   , graphicsEncoders    :: Map EncoderID    Encoder
   , graphicsControllers :: Map ControllerID Controller
   }

-- | Graphic card ressources
data Resources = Resources
   { resFrameBufferIDs  :: [FrameBufferID]   -- ^ Frame buffer IDs
   , resControllerIDs   :: [ControllerID]    -- ^ Controller IDs
   , resConnectorIDs    :: [ConnectorID]     -- ^ Connector IDs
   , resEncoderIDs      :: [EncoderID]       -- ^ Encoder IDs
   , resMinWidth        :: Word32            -- ^ Minimal width
   , resMaxWidth        :: Word32            -- ^ Maximal width
   , resMinHeight       :: Word32            -- ^ Minimal height
   , resMaxHeight       :: Word32            -- ^ Maximal height
   } deriving (Show)

-- | Connector ID
newtype ConnectorID   = ConnectorID Word32 deriving (Show,Eq,Storable,Ord)

-- | Controller ID
newtype ControllerID  = ControllerID Word32 deriving (Show,Eq,Storable,Ord)

-- | Encoder ID
newtype EncoderID     = EncoderID Word32 deriving (Show,Eq,Storable,Ord)

-- | An encoder
--
-- An encoder converts data obtained from the controller (i.e. from the frame
-- buffer associated with the controller) into suitable data for the connector
-- (i.e. for the device connected to the connector). Hence it only supports a
-- set of connectors. In addition, it may not work with all controllers.
data Encoder = Encoder
   { encoderID                  :: EncoderID          -- ^ Encoder identifier
   , encoderType                :: EncoderType        -- ^ Type of the encoder
   , encoderControllerID        :: Maybe ControllerID -- ^ Associated controller
   , encoderPossibleControllers :: [ControllerID]     -- ^ Valid controllers
   , encoderPossibleClones      :: [EncoderID]        -- ^ Valid clone encoders
   , encoderHandle              :: Handle             -- ^ Graphic card
   } deriving (Show)

-- | Video controller
--
-- A controller is used to configure what is displayed on the screen
-- Controllers are called CRTC in original terminology
data Controller = Controller
   { controllerID             :: ControllerID         -- ^ Controller identifier
   , controllerMode           :: Maybe Mode
   , controllerFrameBuffer    :: Maybe FrameBufferPos -- ^ Associated frame buffer and its position (x,y)
   , controllerGammaTableSize :: Word32
   , controllerHandle         :: Handle
   } deriving (Show)

data FrameBufferPos = FrameBufferPos
   { frameBufferPosID :: FrameBufferID        -- ^ Framebuffer identifier
   , frameBufferPosX  :: Word32
   , frameBufferPosY  :: Word32
   } deriving (Show)

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
   { connectorID                 :: ConnectorID     -- ^ Connector identifier
   , connectorType               :: ConnectorType   -- ^ Type of connector
   , connectorByTypeIndex        :: Word32          -- ^ Identifier within connectors of the same type
   , connectorState              :: Connection      -- ^ Connection state
   , connectorPossibleEncoderIDs :: [EncoderID]     -- ^ IDs of the encoders that can work with this connector
   , connectorEncoderID          :: Maybe EncoderID -- ^ Currently used encoder
   , connectorHandle             :: Handle          -- ^ Graphic card
   } deriving (Show)


-- | Get the current graphics state from the kernel
readGraphicsState :: MonadInIO m => Handle -> Flow m '[GraphicsState,InvalidHandle]
readGraphicsState hdl = do
   -- get resource IDs
   mres <- getResources hdl
   case catchVariant @Resources mres of
      Left xs   -> liftVariantM xs
      Right res -> do
         -- read connectors, encoders and controllers
         mconns <- flowTraverse (getConnectorFromID hdl) (resConnectorIDs res)
         mencs  <- flowTraverse (getEncoderFromID hdl res) (resEncoderIDs res)
         mctrls <- flowTraverse (getControllerFromID hdl) (resControllerIDs res)

         case (catchVariant mconns, catchVariant mencs, catchVariant mctrls) of
            (Right conns, Right encs, Right ctrls) ->
               flowSet (buildGraphicsState conns encs ctrls)
            -- on failure we restart the process
            -- TODO: check that we don't loop indefinitely
            _ -> readGraphicsState hdl   


-- | Build GraphicsState
buildGraphicsState :: [Connector] -> [Encoder] -> [Controller] -> GraphicsState
buildGraphicsState conns encs ctrls = GraphicsState conns' encs' ctrls'
   where
      encs'  = Map.fromList <| fmap (\e -> (encoderID e, e))     encs
      conns' = Map.fromList <| fmap (\c -> (connectorID c, c))  conns
      ctrls' = Map.fromList <| fmap (\c -> (controllerID c, c)) ctrls


fromStructGetEncoder :: Resources -> Handle -> StructGetEncoder -> Encoder
fromStructGetEncoder res hdl StructGetEncoder{..} =
      Encoder
         (EncoderID geEncoderId)
         (fromEnumField geEncoderType)
         (if geCrtcId == 0
            then Nothing
            else Just (ControllerID geCrtcId))
         (pickControllers res gePossibleCrtcs)
         (pickEncoders    res gePossibleClones)
         hdl

-- | Get an encoder from its ID
getEncoderFromID :: MonadIO m => Handle -> Resources -> EncoderID -> Flow m '[Encoder,EntryNotFound,InvalidHandle]
getEncoderFromID hdl res (EncoderID encId) = liftIO (ioctlGetEncoder enc hdl)
      >.-.> fromStructGetEncoder res hdl
      >%~^> \case
         EINVAL -> flowSet InvalidHandle
         ENOENT -> flowSet EntryNotFound
         e      -> unhdlErr "getEncoder" e
   where
      enc = StructGetEncoder encId (toEnumField EncoderTypeNone)
               0 BitSet.empty BitSet.empty

-- | Controller attached to the encoder, if any
encoderController :: MonadIO m => Encoder -> Flow m '[Maybe Controller ,EntryNotFound,InvalidHandle]
encoderController enc = case encoderControllerID enc of
   Nothing     -> flowSetN @0 Nothing
   Just contId -> getControllerFromID (encoderHandle enc) contId >.-.> Just

-- | Get encoders (discard errors)
getEncoders :: MonadInIO m => Handle -> Flow m '[[Encoder],EntryNotFound,InvalidHandle]
getEncoders hdl = getResources hdl >.~^> \res ->
   flowTraverse (getEncoderFromID hdl res) (resEncoderIDs res)

emptyStructController :: StructController
emptyStructController = StructController 0 0 0 0 0 0 0 0 emptyStructMode

fromStructController :: Handle -> StructController -> Controller
fromStructController hdl StructController{..} =
   Controller
      (ControllerID contID)
      (if contModeValid /= 0
         then Just (fromStructMode contModeInfo)
         else Nothing)
      (if contFbID /= 0 
         then Just (FrameBufferPos (FrameBufferID contFbID) contFbX contFbY)
         else Nothing)
      contGammaSize
      hdl

      
-- | Get Controller
getControllerFromID :: MonadIO m => Handle -> ControllerID -> Flow m '[Controller,EntryNotFound, InvalidHandle]
getControllerFromID hdl crtcid = liftIO (ioctlGetController crtc hdl)
      >.-.> fromStructController hdl
      >%~^> \case
         EINVAL -> flowSet InvalidHandle
         ENOENT -> flowSet EntryNotFound
         e      -> unhdlErr "getController" e
   where
      ControllerID cid = crtcid
      crtc             = emptyStructController { contID = cid }


setController' :: MonadInIO m => Handle -> ControllerID -> Maybe FrameBufferPos -> [ConnectorID] -> Maybe Mode -> Flow m '[(),ErrorCode]
setController' hdl crtcid fb conns mode = do
   let
      ControllerID cid = crtcid
      conns' = fmap (\(ConnectorID i) -> i) conns

      (fbid,fbx,fby) = case fb of
         Nothing -> (0,0,0)
         Just (FrameBufferPos (FrameBufferID z) x y) -> (z,x,y)

   withArray conns' $ \conArray -> do
      let
         crtc = StructController
            { contID = cid
            , contFbID = fbid
            , contFbX  = fbx
            , contFbY  = fby
            , contModeInfo = case mode of
               Nothing -> emptyStructMode
               Just x  -> toStructMode x
            , contModeValid  = case mode of
               Nothing -> 0
               Just _  -> 1
            , contConnCount  = fromIntegral (length conns)
            , contSetConnPtr = fromIntegral (ptrToWordPtr conArray)
            , contGammaSize = 0
            }

      liftIO (ioctlSetController crtc hdl) >.-.> const ()

-- | Switch to another framebuffer for the given controller
-- without doing a full mode change
--
-- Called "mode_page_flip" in the original terminology
switchFrameBuffer' :: MonadIO m => Handle -> ControllerID -> FrameBufferID -> PageFlipFlags -> Flow m '[(),ErrorCode]
switchFrameBuffer' hdl crtcid fb flags = do
   let
      ControllerID cid = crtcid
      FrameBufferID fid = fb
      s = StructPageFlip cid fid flags 0 0

   liftIO (ioctlPageFlip s hdl) >.-.> const ()

-- | Get controllers
getControllers :: MonadInIO m => Handle -> Flow m '[[Controller],EntryNotFound,InvalidHandle]
getControllers hdl = getResources hdl
   >.-.> resControllerIDs
   >.~^> flowTraverse (getControllerFromID hdl)

-- | Get controller gama look-up table
getControllerGamma :: MonadInIO m => Controller -> Flow m '[([Word16],[Word16],[Word16]),ErrorCode]
getControllerGamma c = do
   let 
      hdl                = controllerHandle c
      (ControllerID cid) = controllerID c
      sz                 = controllerGammaTableSize c
      s                  = StructControllerLut cid sz

   allocaArrays [sz,sz,sz] $ \(as@[r,g,b] :: [Ptr Word16]) -> do
      let f = fromIntegral . ptrToWordPtr
      liftIO (ioctlGetGamma (s (f r) (f g) (f b)) hdl)
         >.~.> (const $ do
            [rs,gs,bs] <- peekArrays [sz,sz,sz] as
            return (rs,gs,bs))

-- | Set controller gama look-up table
setControllerGamma :: MonadInIO m => Controller -> ([Word16],[Word16],[Word16]) -> Flow m '[(),ErrorCode]
setControllerGamma c (rs,gs,bs) = do
   let 
      hdl                = controllerHandle c
      (ControllerID cid) = controllerID c
      sz'                = controllerGammaTableSize c
      sz                 = fromIntegral sz'
      s                  = StructControllerLut cid sz'
      ss                 = [take sz rs,take sz gs, take sz bs]

   withArrays ss $ \[r,g,b] -> do
      let f = fromIntegral . ptrToWordPtr
      liftIO (ioctlSetGamma (s (f r) (f g) (f b)) hdl)
         >.-.> const ()

getConnector' :: MonadIO m => Handle -> StructGetConnector -> Flow m '[StructGetConnector,InvalidParam,EntryNotFound]
getConnector' hdl r = liftIO (ioctlGetConnector r hdl) >%~^> \case
   EINVAL -> flowSet InvalidParam
   ENOENT -> flowSet EntryNotFound
   e      -> unhdlErr "getConnector" e

-- | Get connector
getConnectorFromID :: forall m. MonadInIO m => Handle -> ConnectorID -> Flow m '[Connector,InvalidParam,EntryNotFound,InvalidProperty]
getConnectorFromID hdl connId@(ConnectorID cid) = getConnector' hdl res >.~^> getValues
   where
      res = StructGetConnector 0 0 0 0 0 0 0 0 cid
               (toEnumField ConnectorTypeUnknown) 0 0 0 0
               (toEnumField SubPixelNone)

      getValues :: StructGetConnector -> Flow m '[Connector,InvalidParam,EntryNotFound,InvalidProperty]
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

rawGet :: forall m. MonadInIO m => Handle -> StructGetConnector -> Flow m '[(StructGetConnector,Connector),InvalidParam,InvalidProperty,EntryNotFound]
rawGet hdl res2 = do

   let
      allocaArray' :: (Integral c, Storable a) => c -> (Ptr a -> m b) -> m b
      allocaArray' n = allocaArray (fromIntegral n)


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


parseRes :: forall m. MonadInIO m => Handle -> StructGetConnector -> StructGetConnector -> Flow m '[Connector,InvalidParam,InvalidProperty]
parseRes hdl res2 res4 = do
   let
      cv = wordPtrToPtr . fromIntegral

      wrapZero 0 = Nothing
      wrapZero x = Just x

      peekArray' :: (Storable a, Integral c) => c -> Ptr a -> m [a]
      peekArray' n ptr = peekArray (fromIntegral n) ptr

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
getConnectors :: MonadInIO m => Handle -> Flow m '[[Connector],InvalidParam,EntryNotFound,InvalidProperty,InvalidHandle]
getConnectors hdl = getResources hdl
   >.-.> resConnectorIDs
   >.~^^> flowTraverse (getConnectorFromID hdl)


-- | Encoder attached to the connector, if any
connectorEncoder :: MonadInIO m => Connector -> Flow m '[Maybe Encoder,EntryNotFound,InvalidHandle]
connectorEncoder conn = case connectorEncoderID conn of
   Nothing    -> flowSetN @0 Nothing
   Just encId -> 
      getResources (connectorHandle conn) >.~^> \res ->
         getEncoderFromID (connectorHandle conn) res encId >.-.> Just

-- | Retrieve Controller (and encoder) controling a connector (if any)
connectorController :: MonadInIO m => Connector -> Flow m '[(Maybe Controller, Maybe Encoder),EntryNotFound,InvalidHandle]
connectorController conn =
   connectorEncoder conn >.~^> \enc ->
      case enc of
         Nothing -> flowSetN @0 (Nothing,Nothing)
         Just e  -> encoderController e >.-.> (,enc)


-- | Get graphic card resources
getResources :: forall m. MonadInIO m => Handle -> Flow m '[Resources,InvalidHandle]
getResources hdl = getValues [10,10,10,10] -- try with default values
   where 
      getRes :: StructCardRes -> Flow m '[StructCardRes,InvalidHandle]
      getRes r = liftIO (ioctlGetResources r hdl) >..%~^> \case
         EINVAL -> flowSet InvalidHandle
         e      -> unhdlErr "getResources" e

      extractSize x = [csCountFbs, csCountCrtcs, csCountConns, csCountEncs] <*> [x]

      getValues :: [Word32] -> Flow m '[Resources,InvalidHandle]
      getValues arraySizes = liftWith (allocaArrays arraySizes) $ 
         \([fs,crs,cs,es] :: [Ptr Word32]) -> do
            let 
               [nfb,nct,nco,nen] = arraySizes
               cv = fromIntegral . ptrToWordPtr
               res3 = StructCardRes
                           { csFbIdPtr    = cv fs
                           , csCrtcIdPtr  = cv crs
                           , csEncIdPtr   = cv es
                           , csConnIdPtr  = cv cs
                           , csCountFbs   = nfb
                           , csCountCrtcs = nct
                           , csCountConns = nco
                           , csCountEncs  = nen
                           , csMinHeight  = 0
                           , csMaxHeight  = 0
                           , csMinWidth   = 0
                           , csMaxWidth   = 0
                           }
            getRes res3 >.~^> \r ->
               -- we need to check that the number of resources is still
               -- lower than the size of our arrays (as resources may have
               -- appeared between the time we get the number of resources
               -- and the time we get them...) If not, we redo the whole
               -- process
               if all (uncurry (>)) (arraySizes `zip` extractSize r)
                  then extractValues r >.~^> flowSetN @0
                  else getValues (extractSize r)


      extractValues :: StructCardRes -> Flow m '[Resources]
      extractValues r = do
         let 
            as  = [csFbIdPtr, csCrtcIdPtr, csConnIdPtr, csEncIdPtr] <*> [r]
            as' = fmap (wordPtrToPtr . fromIntegral) as
            arraySizes = extractSize r
         [fbs,ctrls,conns,encs] <- peekArrays arraySizes as'
         flowSetN @0 $ Resources
               (fmap FrameBufferID fbs)
               (fmap ControllerID  ctrls)
               (fmap ConnectorID   conns)
               (fmap EncoderID     encs)
               (csMinWidth  r)
               (csMaxWidth  r)
               (csMinHeight r)
               (csMaxHeight r)


-- | Internal function to retreive card entities from their identifiers
getEntities :: MonadInIO m => (Resources -> [a]) -> (Handle -> a -> m (Either x b)) -> Handle -> m [b]
getEntities getIDs getEntityFromID hdl = do
   res <- getResources hdl
          >..%~!!> (\InvalidHandle -> error "getEntities: invalid handle")
          |> flowRes
   let 
      f (Left _)  xs = xs
      f (Right x) xs = x:xs
      ids            = getIDs res

   xs <- traverse (getEntityFromID hdl) ids
   return (foldr f [] xs)


-- | Pick the elements in es whose indexes are in bs
pickResources :: [a] -> BitSet Word32 Int -> [a]
pickResources es bs = pick es 0 (BitSet.elems bs)
   where
      pick :: [a] -> Int -> [Int] -> [a]
      pick [] _ _ = []
      pick _ _ [] = []
      pick (x:xs) n (i:is)
         | n == i    = x : pick xs (n+1) is
         | otherwise = pick xs (n+1) (i:is)

-- | Pick the controllers whose indexes are in bs
pickControllers :: Resources -> BitSet Word32 Int -> [ControllerID]
pickControllers res = pickResources (resControllerIDs res)

-- | Pick the controllers whose indexes are in bs
pickEncoders :: Resources -> BitSet Word32 Int -> [EncoderID]
pickEncoders res = pickResources (resEncoderIDs res)
