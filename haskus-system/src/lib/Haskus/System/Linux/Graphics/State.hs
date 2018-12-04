{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | State of the graphics system
module Haskus.System.Linux.Graphics.State
   ( GraphicsState (..)
   , readGraphicsState
   -- ** Entities
   , Controller (..)
   , Encoder(..)
   , EncoderType(..)
   , Frame (..)
   , Connector (..)
   , Connection (..)
   , ConnectedDevice (..)
   , ConnectorType(..)
   , SubPixel(..)
   , Resources(..)
   , setController'
   , switchFrameBuffer'
   , getControllers
   , getControllerGamma
   , setControllerGamma
   , getEncoders
   , getEncoderFromID
   , getControllerFromID
   , fromStructController
   , getConnectorFromID
   , getResources
   , getEntities
   , pickEncoders
   , pickControllers
   , getPlaneResources
   , InvalidPlane (..)
   , Plane (..)
   , getPlane
   , setPlane
   , disablePlane
   , InvalidDestRect (..)
   , InvalidSrcRect (..)
   )
where

import Haskus.System.Linux.Graphics.Mode
import Haskus.System.Linux.Graphics.Property
import Haskus.System.Linux.Graphics.FrameSource
import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.System.Linux.Graphics.Entities
import Haskus.System.Linux.Internals.Graphics
import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Error
import Haskus.System.Linux.Handle
import Haskus.Utils.Memory (peekArrays,allocaArrays,withArrays)
import Haskus.Utils.Flow
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Ptr
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.BitSet as BitSet
import Haskus.Format.Binary.Enum
import Haskus.Format.Binary.BitField
import Haskus.Format.Binary.FixedPoint
import Control.Monad (liftM2)
import qualified Data.Map as Map
import Data.Map (Map)


-- | Graph of graphics entities
data GraphicsState = GraphicsState
   { graphicsConnectors   :: Map ConnectorID   Connector  -- ^ Connectors
   , graphicsEncoders     :: Map EncoderID     Encoder    -- ^ Encoders
   , graphicsControllers  :: Map ControllerID  Controller -- ^ Controllers
   , graphicsPlanes       :: Map PlaneID       Plane      -- ^ Planes
   , graphicsFrameBuffers :: [FrameSourceID]              -- ^ Frame sources
   } deriving (Show)

-- | Graphic card ressources
data Resources = Resources
   { resFrameSourceIDs  :: [FrameSourceID]   -- ^ Frame source IDs
   , resControllerIDs   :: [ControllerID]    -- ^ Controller IDs
   , resConnectorIDs    :: [ConnectorID]     -- ^ Connector IDs
   , resEncoderIDs      :: [EncoderID]       -- ^ Encoder IDs
   , resMinWidth        :: Word32            -- ^ Minimal width
   , resMaxWidth        :: Word32            -- ^ Maximal width
   , resMinHeight       :: Word32            -- ^ Minimal height
   , resMaxHeight       :: Word32            -- ^ Maximal height
   } deriving (Show)



-- | Get the current graphics state from the kernel
readGraphicsState :: MonadInIO m => Handle -> FlowT '[InvalidHandle] m GraphicsState
readGraphicsState hdl = go 5
   where
      go n = do
         -- get resource IDs
         res <- getResources hdl
         let fbs = resFrameSourceIDs res
         -- read connectors, encoders and controllers
         mconns <- runFlowT <| traverse (getConnectorFromID hdl) (resConnectorIDs res)
         mencs  <- runFlowT <| traverse (getEncoderFromID hdl res) (resEncoderIDs res)
         mctrls <- runFlowT <| traverse (getControllerFromID hdl) (resControllerIDs res)
         -- read planes
         mplanes <- runFlowT <| (traverse (getPlane hdl) =<< liftFlowT (getPlaneResources hdl))
                                 -- shouldn't happen, planes are invariant
                                 `catchDie` (\(InvalidPlane _)  -> error "Invalid plane" )

         case (fromVariantHead mconns, fromVariantHead mencs, fromVariantHead mctrls, fromVariantHead mplanes) of
            (Just conns, Just encs, Just ctrls, Just planes) ->
               return (buildGraphicsState conns encs ctrls planes fbs)
            -- on failure we restart the process
            -- (we check that we don't loop indefinitely)
            _ -> if n > (0 :: Word)
                  then go (n-1)
                  else throwE InvalidHandle


-- | Build GraphicsState
buildGraphicsState :: [Connector] -> [Encoder] -> [Controller] -> [Plane] -> [FrameSourceID] -> GraphicsState
buildGraphicsState conns encs ctrls planes fbs = GraphicsState conns' encs' ctrls' planes' fbs
   where
      encs'   = Map.fromList <| fmap (\e -> (encoderID e, e))    encs
      conns'  = Map.fromList <| fmap (\c -> (connectorID c, c))  conns
      ctrls'  = Map.fromList <| fmap (\c -> (controllerID c, c)) ctrls
      planes' = Map.fromList <| fmap (\p -> (planeID p, p))      planes


fromStructGetEncoder :: Resources -> Handle -> StructGetEncoder -> Encoder
fromStructGetEncoder res hdl StructGetEncoder{..} =
      Encoder
         (EntityID geEncoderId)
         (fromEnumField geEncoderType)
         (if geCrtcId == 0
            then Nothing
            else Just (EntityID geCrtcId))
         (pickControllers res gePossibleCrtcs)
         (pickEncoders    res gePossibleClones)
         hdl

-- | Get an encoder from its ID
getEncoderFromID :: MonadInIO m => Handle -> Resources -> EncoderID -> FlowT '[EntryNotFound,InvalidHandle] m Encoder
getEncoderFromID hdl res encId =
   (ioctlGetEncoder enc hdl ||> fromStructGetEncoder res hdl)
      `catchLiftLeft` \case
         EINVAL -> throwE InvalidHandle
         ENOENT -> throwE EntryNotFound
         e      -> unhdlErr "getEncoder" e
   where
      enc = StructGetEncoder (unEntityID encId) (toEnumField EncoderTypeNone)
               0 BitSet.empty BitSet.empty

-- | Get encoders (discard errors)
getEncoders :: MonadInIO m => Handle -> FlowT '[EntryNotFound,InvalidHandle] m [Encoder]
getEncoders hdl = do
   res <- liftFlowT <| getResources hdl
   traverse (getEncoderFromID hdl res) (resEncoderIDs res)

emptyStructController :: StructController
emptyStructController = StructController 0 0 0 0 0 0 0 0 emptyStructMode

fromStructController :: Handle -> StructController -> Controller
fromStructController hdl StructController{..} =
   Controller
      (EntityID contID)
      (if contModeValid /= 0
         then Just (fromStructMode contModeInfo)
         else Nothing)
      (if contFbID /= 0 
         then Just (Frame (EntityID contFbID) contFbX contFbY)
         else Nothing)
      contGammaSize
      hdl

      
-- | Get Controller
getControllerFromID :: MonadInIO m => Handle -> ControllerID -> FlowT '[EntryNotFound,InvalidHandle] m Controller
getControllerFromID hdl eid =
   (ioctlGetController crtc hdl ||> fromStructController hdl)
      `catchLiftLeft` \case
         EINVAL -> throwE InvalidHandle
         ENOENT -> throwE EntryNotFound
         e      -> unhdlErr "getController" e
   where
      crtc = emptyStructController { contID = unEntityID eid }


setController' :: MonadInIO m => Handle -> ControllerID -> Maybe Frame -> [ConnectorID] -> Maybe Mode -> FlowT '[ErrorCode] m ()
setController' hdl eid fb conns mode = do
   let
      conns' = fmap unEntityID conns

      (fbid,fbx,fby) = case fb of
         Nothing -> (0,0,0)
         Just (Frame (EntityID z) x y) -> (z,x,y)

   void $ withArray conns' $ \conArray -> do
      let
         crtc = StructController
            { contID   = unEntityID eid
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

      ioctlSetController crtc hdl

-- | Switch to another framebuffer for the given controller
-- without doing a full mode change
--
-- Called "mode_page_flip" in the original terminology
switchFrameBuffer' :: MonadInIO m => Handle -> ControllerID -> FrameSourceID -> PageFlipFlags -> Word64 -> FlowT '[ErrorCode] m ()
switchFrameBuffer' hdl cid fsid flags udata = do
   let
      s = StructPageFlip (unEntityID cid) (unEntityID fsid) flags 0 udata

   void <| ioctlPageFlip s hdl

-- | Get controllers
getControllers :: MonadInIO m => Handle -> FlowT '[EntryNotFound,InvalidHandle] m [Controller]
getControllers hdl = do
   res <- liftFlowT <| getResources hdl
   traverse (getControllerFromID hdl) (resControllerIDs res)

-- | Get controller gama look-up table
getControllerGamma :: MonadInIO m => Controller -> FlowT '[ErrorCode] m ([Word16],[Word16],[Word16])
getControllerGamma c = do
   let 
      hdl = controllerHandle c
      sz  = controllerGammaTableSize c
      s   = StructControllerLut (unEntityID (controllerID c)) sz

   allocaArrays [sz,sz,sz] $ \(as@[r,g,b] :: [Ptr Word16]) -> do
      let f = fromIntegral . ptrToWordPtr
      void (ioctlGetGamma (s (f r) (f g) (f b)) hdl)
      [rs,gs,bs] <- liftIO <| peekArrays [sz,sz,sz] as
      return (rs,gs,bs)

-- | Set controller gama look-up table
setControllerGamma :: MonadInIO m => Controller -> ([Word16],[Word16],[Word16]) -> FlowT '[ErrorCode] m ()
setControllerGamma c (rs,gs,bs) = do
   let 
      hdl = controllerHandle c
      sz' = controllerGammaTableSize c
      sz  = fromIntegral sz'
      s   = StructControllerLut (unEntityID (controllerID c)) sz'
      ss  = [take sz rs,take sz gs, take sz bs]

   void $ withArrays ss $ \[r,g,b] -> do
      let f = fromIntegral . ptrToWordPtr
      ioctlSetGamma (s (f r) (f g) (f b)) hdl

getConnector' :: MonadInIO m => Handle -> StructGetConnector -> FlowT '[EntryNotFound,InvalidParam] m StructGetConnector
getConnector' hdl r =
   ioctlGetConnector r hdl
      `catchLiftLeft` \case
         EINVAL -> throwE InvalidParam
         ENOENT -> throwE EntryNotFound
         e      -> unhdlErr "getConnector" e

-- | Get connector
getConnectorFromID :: forall m. MonadInIO m => Handle -> ConnectorID -> FlowT '[InvalidParam,EntryNotFound,InvalidProperty] m Connector
getConnectorFromID hdl eid = liftFlowT (getConnector' hdl res) >>= getValues
   where
      res = StructGetConnector 0 0 0 0 0 0 0 0 (unEntityID eid)
               (toEnumField ConnectorTypeUnknown) 0 0 0 0
               (toEnumField SubPixelNone)

      getValues :: StructGetConnector -> FlowT '[InvalidParam,EntryNotFound,InvalidProperty] m Connector
      getValues res2 = do
            (rawRes,conn) <- liftFlowT (rawGet hdl res2)
            -- we need to check that the number of resources is still the same (as
            -- resources may have appeared between the time we get the number of
            -- resources and the time we get them...)
            -- If not, we redo the whole process
            if   connModesCount    res2 < connModesCount    rawRes
              || connPropsCount    res2 < connPropsCount    rawRes
              || connEncodersCount res2 < connEncodersCount rawRes
               then getConnectorFromID hdl eid
               else return conn

rawGet :: MonadInIO m => Handle -> StructGetConnector -> FlowT '[InvalidParam,InvalidProperty,EntryNotFound] m (StructGetConnector,Connector)
rawGet hdl res2 = do

   let
      allocaArray' :: (MonadInIO m, Integral c, Storable a) => c -> (Ptr a -> m b) -> m b
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

               res4 <- liftFlowT <| getConnector' hdl res3 
               (res4,) <|| liftFlowT <| parseRes hdl res2 res4


parseRes :: MonadInIO m => Handle -> StructGetConnector -> StructGetConnector -> FlowT '[InvalidParam,InvalidProperty] m Connector
parseRes hdl res2 res4 = do
   let
      cv = wordPtrToPtr . fromIntegral

      wrapZero 0 = Nothing
      wrapZero x = Just x

      peekArray' :: (MonadIO m, Storable a, Integral c) => c -> Ptr a -> m [a]
      peekArray' n ptr = peekArray (fromIntegral n) ptr

   state <- case connConnection_ res4 of
      1 -> do
            -- properties
            rawProps <- liftM2 RawProperty
                        <$> peekArray' (connPropsCount res2) (cv (connPropsPtr res4))
                        <*> peekArray' (connPropsCount res2) (cv (connPropValuesPtr res4))
            props <- forM rawProps $ \raw -> do
               --FIXME: store property meta in the card
               getPropertyMeta hdl (rawPropertyMetaID raw)
                  ||> \meta -> Property meta (rawPropertyValue raw)

            modes <- fmap fromStructMode <$> peekArray' (connModesCount res2) (cv (connModesPtr res4))

            return (Connected (ConnectedDevice
               modes
               (connWidth_ res4)
               (connHeight_ res4)
               (fromEnumField (connSubPixel_ res4))
               props))
               
      2 -> return Disconnected
      _ -> return ConnectionUnknown

   encs  <- fmap EntityID <$> peekArray' (connEncodersCount res2) (cv (connEncodersPtr res4))

   return <| Connector
               (EntityID (connConnectorID_ res4))
               (fromEnumField (connConnectorType_ res4))
               (connConnectorTypeID_ res4)
               state
               encs
               (EntityID <$> wrapZero (connEncoderID_ res4))
               hdl


-- | Get graphic card resources
getResources :: forall m. MonadInIO m => Handle -> FlowT '[InvalidHandle] m Resources
getResources hdl = getValues [10,10,10,10] -- try with default values
   where 
      getRes :: StructCardRes -> FlowT '[InvalidHandle] m StructCardRes
      getRes r = ioctlGetResources r hdl
                  `catchLiftLeft` \case
                     EINVAL -> throwE InvalidHandle
                     e      -> unhdlErr "getResources" e

      extractSize x = [csCountFbs, csCountCrtcs, csCountConns, csCountEncs] <*> [x]

      getValues :: [Word32] -> FlowT '[InvalidHandle] m Resources
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
            r <- getRes res3
            -- we need to check that the number of resources is still
            -- lower than the size of our arrays (as resources may have
            -- appeared between the time we get the number of resources
            -- and the time we get them...) If not, we redo the whole
            -- process
            if all (uncurry (>)) (arraySizes `zip` extractSize r)
               then liftFlowT <| extractValues r
               else getValues (extractSize r)


      extractValues :: StructCardRes -> FlowT '[] m Resources
      extractValues r = do
         let 
            as  = [csFbIdPtr, csCrtcIdPtr, csConnIdPtr, csEncIdPtr] <*> [r]
            as' = fmap (wordPtrToPtr . fromIntegral) as
            arraySizes = extractSize r
         [fbs,ctrls,conns,encs] <- peekArrays arraySizes as'
         return $ Resources
               (fmap EntityID fbs)
               (fmap EntityID ctrls)
               (fmap EntityID conns)
               (fmap EntityID encs)
               (csMinWidth  r)
               (csMaxWidth  r)
               (csMinHeight r)
               (csMaxHeight r)


-- | Internal function to retreive card entities from their identifiers
getEntities :: MonadInIO m => (Resources -> [a]) -> (Handle -> a -> m (Either x b)) -> Handle -> m [b]
getEntities getIDs getEntityFromID hdl = do
   res <- variantToValue <|| runFlowT <|
            (getResources hdl `catchDie` (\InvalidHandle -> error "getEntities: invalid handle"))
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


-- | Error invalid plane ID
data InvalidPlane = InvalidPlane PlaneID deriving (Show)

-- | Get the IDs of the supported planes
getPlaneResources :: forall m. MonadInIO m => Handle -> FlowT '[InvalidHandle] m [PlaneID]
getPlaneResources hdl = getCount >>= getIDs
   where
      gpr s = ioctlGetPlaneResources s hdl

      -- get the number of planes (invariant for a given device)
      getCount :: FlowT '[InvalidHandle] m Word32
      getCount = (gpr (StructGetPlaneRes 0 0) ||> gprsCountPlanes)
                  `catchLiftLeft` \case
                     EINVAL -> throwE InvalidHandle
                     e      -> unhdlErr "getPlaneResources" e
   
      -- get the plane IDs (invariant for a given device)
      getIDs :: Word32 -> FlowT '[InvalidHandle] m [PlaneID]
      getIDs 0 = return []
      getIDs n = allocaArray (fromIntegral n) $ \(p :: Ptr Word32) -> do
         let p' = fromIntegral (ptrToWordPtr p)
         void (gpr (StructGetPlaneRes p' n))
            `catchLiftLeft` \case
               EINVAL -> throwE InvalidHandle
               e      -> unhdlErr "getPlaneResources" e
         fmap EntityID <$> peekArray (fromIntegral n) p

-- | Get plane information
getPlane :: forall m. MonadInIO m => Handle -> PlaneID -> FlowT '[InvalidHandle,InvalidPlane] m Plane
getPlane hdl pid = getCount >>= getInfo
   where

      gpr :: StructGetPlane -> FlowT '[InvalidHandle,InvalidPlane] m StructGetPlane
      gpr s = ioctlGetPlane s hdl
               `catchLiftLeft` \case
                  EINVAL -> throwE InvalidHandle
                  ENOENT -> throwE (InvalidPlane (EntityID (gpPlaneId s)))
                  e      -> unhdlErr "getPlane" e

      toMaybe _ 0 = Nothing
      toMaybe f x = Just (f x)

      -- get the number of formats (invariant for a given plane)
      getCount :: FlowT '[InvalidHandle,InvalidPlane] m Word32
      getCount = gpr (StructGetPlane (unEntityID pid) 0 0 BitSet.empty 0 0 0)
                  ||> gpCountFmtTypes 

      -- get the plane info (invariant for a given plane)
      getInfo :: Word32 -> FlowT '[InvalidHandle,InvalidPlane] m Plane
      getInfo n = allocaArray (fromIntegral n) $ \(p :: Ptr Word32) -> do
         let 
            p' = fromIntegral (ptrToWordPtr p)
            si = StructGetPlane (unEntityID pid) 0 0 BitSet.empty 0 n p'
         gpr si >>= \StructGetPlane{..} -> liftFlowT (getResources hdl) >>= \res -> do
               -- TODO: controllers are invariant, we should store them
               -- somewhere to avoid getResources
               fmts <- fmap (PixelFormat . BitFields) <$> peekArray (fromIntegral n) p
               return <| Plane
                  { planeID                  = pid
                  , planeControllerId        = toMaybe EntityID gpCrtcId
                  , planeFrameSourceId       = toMaybe EntityID gpFbId
                  , planePossibleControllers = pickControllers res gpPossibleCrtcs
                  , planeGammaSize           = gpGammaSize
                  , planeFormats             = fmts
                  }

-- | Invalid destination rectangle
data InvalidDestRect = InvalidDestRect deriving (Show,Eq)

-- | Invalid source rectangle
data InvalidSrcRect  = InvalidSrcRect deriving (Show,Eq)

-- | Set plane
--
-- If the source/destination rectangles are not the same, scaling support is
-- required. Devices not supporting scaling will fail with InvalidParam.
--
-- The fractional part in SrcRect is for devices supporting sub-pixel plane
-- coordinates.
setPlane :: MonadInIO m => Handle -> PlaneID -> Maybe (ControllerID, FrameSourceID, SrcRect, DestRect) -> FlowT '[InvalidParam,EntryNotFound,InvalidDestRect,InvalidSrcRect] m ()
setPlane hdl pid opts = do

   let 
      makeS cid fsid = StructSetPlane (unEntityID pid) (unEntityID cid)
                                      (unEntityID fsid) BitSet.empty

      e16 = toFixedPoint (0 :: Float)

      s = case opts of
            Nothing -> -- disable the plane
               makeS (EntityID 0) (EntityID 0)
                  0 0 0 0 e16 e16 e16 e16

            Just (cid,fbid,SrcRect{..},DestRect{..}) ->
               makeS cid fbid
                  destX destY destWidth destHeight
                  srcX srcY srcHeight srcWidth

   void (ioctlSetPlane s hdl)
      `catchLiftLeft` \case
         EINVAL -> throwE InvalidParam
         ENOENT -> throwE EntryNotFound
         ERANGE -> throwE InvalidDestRect
         ENOSPC -> throwE InvalidSrcRect
         e      -> unhdlErr "setPlane" e

-- | Disable a plane
disablePlane :: MonadInIO m => Handle -> PlaneID -> FlowT '[InvalidParam,EntryNotFound] m ()
disablePlane hdl p = setPlane hdl p Nothing
   -- these errors should not be triggered when we disable a plane
   `catchDie` (\InvalidDestRect -> unhdlErr "disablePlane" InvalidDestRect)
   `catchDie` (\InvalidSrcRect  -> unhdlErr "disablePlane" InvalidSrcRect)
