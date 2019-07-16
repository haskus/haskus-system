{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}

-- | State of the graphics system
module Haskus.System.Linux.Graphics.State
   ( EntitiesIDs (..)
   , EntitiesMap (..)
   , Entities (..)
   , getHandleEntitiesIDs
   , getHandleEntities
   , getHandleEntitiesMap
   , toEntitiesMap
   , Controller (..)
   , Encoder(..)
   , EncoderType(..)
   , Frame (..)
   , Connector (..)
   , Connection (..)
   , Display (..)
   , ConnectorType(..)
   , SubPixel(..)
   , Resources(..)
   , setController'
   , switchFrame'
   , getHandleControllers
   , getControllerGamma
   , setControllerGamma
   , getHandleEncoders
   , getEncoderFromID
   , getControllerFromID
   , fromStructController
   , getConnectorFromID
   , getResources
   , pickEncoders
   , pickControllers
   , getPlaneIDs
   , InvalidPlaneID (..)
   , InvalidControllerID (..)
   , InvalidConnectorID (..)
   , InvalidEncoderID (..)
   , Plane (..)
   , getPlaneFromID
   , setPlane
   , disablePlane
   , InvalidDestRect (..)
   , InvalidSrcRect (..)
   , DrmInfo (..)
   , handleGetInfo
   -- * Blob
   , createBlob
   , destroyBlob
   , withBlob
   , withHandleModeBlob
   , createHandleModeBlob
   -- * Property
   , getPropertyMeta
   , getAllPropertyMeta
   , showProperty
   , showPropertyEx
   , showPropertyMeta
   , setAtomic
   , AtomicErrors
   , InvalidProperty (..)
   )
where

import Haskus.System.Linux.Graphics.Mode
import Haskus.System.Linux.Graphics.Frame
import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.System.Linux.Graphics.Entities
import Haskus.System.Linux.Internals.Graphics
import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Error
import Haskus.System.Linux.Handle
import Haskus.Memory.Utils (peekArrays,allocaArrays,withArrays)
import Haskus.Utils.Flow
import Haskus.Utils.Maybe
import qualified Haskus.Utils.List as List
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.BitSet as BitSet
import Haskus.Format.Binary.Enum
import Haskus.Format.Binary.BitField
import Haskus.Format.Binary.FixedPoint
import Haskus.Format.Binary.Buffer
import Haskus.Format.String 
import Haskus.Memory.Ptr

import Foreign.Ptr
import Foreign.Marshal.Alloc(free,mallocBytes)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- | Entities (only IDs)
data EntitiesIDs = EntitiesIDs
   { entitiesConnectorsIDs   :: [ConnectorID]   -- ^ Connectors
   , entitiesControllersIDs  :: [ControllerID]  -- ^ Controllers
   , entitiesPlanesIDs       :: [PlaneID]       -- ^ Planes
   , entitiesFramesIDs       :: [FrameID]       -- ^ Frames
   }
   deriving (Show,Eq)

-- | Entities map from their ID
data EntitiesMap = EntitiesMap
   { entitiesConnectorsMap  :: Map ConnectorID   Connector          -- ^ Connectors
   , entitiesControllersMap :: Map ControllerID  Controller         -- ^ Controllers
   , entitiesPlanesMap      :: Map PlaneID       Plane              -- ^ Planes
   , entitiesFramesMap      :: Map FrameID       StructFrameCommand -- ^ Frames
   } deriving (Show)

-- | Entities
data Entities = Entities
   { entitiesConnectors  :: [Connector]          -- ^ Connectors
   , entitiesControllers :: [Controller]         -- ^ Controllers
   , entitiesPlanes      :: [Plane]              -- ^ Planes
   , entitiesFrames      :: [StructFrameCommand] -- ^ Frames
   } deriving (Show)


-- | Graphic card ressources
data Resources = Resources
   { resFrameIDs        :: [FrameID]         -- ^ Frame IDs
   , resControllerIDs   :: [ControllerID]    -- ^ Controller IDs
   , resConnectorIDs    :: [ConnectorID]     -- ^ Connector IDs
   , resEncoderIDs      :: [EncoderID]       -- ^ Encoder IDs
   , resMinWidth        :: Word32            -- ^ Minimal width
   , resMaxWidth        :: Word32            -- ^ Maximal width
   , resMinHeight       :: Word32            -- ^ Minimal height
   , resMaxHeight       :: Word32            -- ^ Maximal height
   } deriving (Show)


-- | Get card entities
getHandleEntitiesIDs :: MonadInIO m => Handle -> Excepts '[InvalidHandle] m EntitiesIDs
getHandleEntitiesIDs hdl = do
   res <- getResources hdl
   planeIDs <- getPlaneIDs hdl

   -- Read Note [Avoiding Encoders] to understand why we don't return Encoder IDs
   pure <| EntitiesIDs
      { entitiesConnectorsIDs   = resConnectorIDs res
      , entitiesControllersIDs  = resControllerIDs res
      , entitiesFramesIDs       = resFrameIDs res
      , entitiesPlanesIDs       = planeIDs
      }

-- | Get the current entities
getHandleEntities :: MonadInIO m => Handle -> Excepts '[InvalidHandle] m Entities
getHandleEntities hdl = go 5
   where
      getPlaneFromID' i
         = getPlaneFromID hdl i
            -- shouldn't happen, planes are invariant
            |> catchDieE (\(InvalidPlaneID _) -> error "getHandleEntities: unexpected invalid plane ID" )

      getControllerFromID' i
         = getControllerFromID hdl i
            -- shouldn't happen, controllers are invariant
            |> catchDieE (\(InvalidControllerID _) -> error "getHandleEntities: unexpected invalid controller ID" )

      go n = do
         ids <- getHandleEntitiesIDs hdl
         -- Read connectors, controllers and planes.
         -- Connectors may fail as they can be removed between the time we get
         -- the ID and the time we get the details from the ID. Controllers and
         -- planes IDs are invariant for a given device.
         mframes <- runE <| traverse (getFrameFromID hdl)       (entitiesFramesIDs      ids)
         mconns  <- runE <| traverse (getConnectorFromID hdl)   (entitiesConnectorsIDs  ids)
         mctrls  <- runE <| traverse getControllerFromID'       (entitiesControllersIDs ids)
         mplanes <- runE <| traverse getPlaneFromID'            (entitiesPlanesIDs      ids)

         case (mconns, mctrls, mplanes, mframes) of
            (VRight conns, VRight ctrls, VRight planes, VRight frames) ->
               return <| Entities conns ctrls planes frames
            -- on failure we restart the process
            -- (we check that we don't loop indefinitely)
            _ -> if n > (0 :: Word)
                  then go (n-1)
                  else throwE InvalidHandle


-- | Make a Map for each kind of entity with their IDs as keys
toEntitiesMap :: Entities -> EntitiesMap
toEntitiesMap Entities{..} = EntitiesMap
   { entitiesConnectorsMap  = Map.fromList (List.zipLeftWith connectorID        entitiesConnectors)
   , entitiesControllersMap = Map.fromList (List.zipLeftWith controllerID       entitiesControllers)
   , entitiesPlanesMap      = Map.fromList (List.zipLeftWith planeID            entitiesPlanes)
   , entitiesFramesMap      = Map.fromList (List.zipLeftWith (EntityID. fcFbId) entitiesFrames)
   }

-- | Get entities Map with their IDs as keys
getHandleEntitiesMap :: MonadInIO m => Handle -> Excepts '[InvalidHandle] m EntitiesMap
getHandleEntitiesMap hdl = getHandleEntities hdl ||> toEntitiesMap

-- | Convert a StructGetEncoder
--
-- If you don't pass Resources, you won't get possibleClones and
-- possibleControllers (which should be avoided anyway)
fromStructGetEncoder :: Maybe Resources -> Handle -> StructGetEncoder -> Encoder
fromStructGetEncoder mres hdl StructGetEncoder{..} =
      Encoder
         (EntityID geEncoderId)
         (fromEnumField geEncoderType)
         (if geCrtcId == 0
            then Nothing
            else Just (EntityID geCrtcId))
         (mres ||> (`pickControllers` gePossibleCrtcs)  |> fromMaybe [])
         (mres ||> (`pickEncoders`    gePossibleClones) |> fromMaybe [])
         hdl

-- | Get an encoder from its ID
--
-- If you don't pass Resources, you won't get possibleClones and
-- possibleControllers (which should be avoided anyway)
getEncoderFromID :: MonadInIO m => Handle -> Maybe Resources -> EncoderID -> Excepts '[InvalidHandle,InvalidEncoderID] m Encoder
getEncoderFromID hdl mres encId =
   (ioctlGetEncoder enc hdl ||> fromStructGetEncoder mres hdl)
      |> catchLiftLeft \case
            EINVAL -> throwE InvalidHandle
            ENOENT -> throwE (InvalidEncoderID encId)
            e      -> unhdlErr "getEncoderFromID" e
   where
      enc = StructGetEncoder (unEntityID encId) (toEnumField EncoderTypeNone)
               0 BitSet.empty BitSet.empty

-- | Get encoders
getHandleEncoders :: MonadInIO m => Handle -> Excepts '[InvalidHandle] m [Encoder]
getHandleEncoders hdl = do
   res <- liftE <| getResources hdl
   let
      getEncoderFromID' i = getEncoderFromID hdl (Just res) i
         -- shouldn't happen, encoders are invariant
         |> catchDieE (\(InvalidEncoderID _) -> error "getHandleEncoders: unexpected invalid encoder ID" )
   traverse getEncoderFromID' (resEncoderIDs res)

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
         then Just (FrameView (EntityID contFbID) contFbX contFbY)
         else Nothing)
      contGammaSize
      hdl

      
-- | Get Controller
getControllerFromID :: MonadInIO m => Handle -> ControllerID -> Excepts '[InvalidHandle,InvalidControllerID] m Controller
getControllerFromID hdl eid = do
   let
      controllerStruct = emptyStructController { contID = unEntityID eid }
   ioctlGetController controllerStruct hdl
      ||> fromStructController hdl
      |> catchLiftLeft \case
            EINVAL -> throwE InvalidHandle
            ENOENT -> throwE (InvalidControllerID eid)
            e      -> unhdlErr "getControllerFromID" e

-- | Get Frame info
--
-- WARNING: if the Frame uses several buffers there is no Linux API yet to
-- retrieve information from it (we miss drm_mode_getfb2 IOCTL).
getFrameFromID :: MonadInIO m => Handle -> FrameID -> Excepts '[InvalidHandle,InvalidFrameID] m StructFrameCommand
getFrameFromID hdl eid = do
   let
      frameStruct = StructFrameCommand
         { fcFbId   = unEntityID eid
         , fcWidth  = 0
         , fcHeight = 0
         , fcPitch  = 0
         , fcBPP    = 0
         , fcDepth  = 0
         , fcHandle = 0
         }
   ioctlGetFrame frameStruct hdl
      |> catchLiftLeft \case
            EINVAL -> -- TODO: we can't know if it failed because of an invalid
                      -- handle or because the frame usees several buffers.
                      -- We would need drm_mode_getfb2 which doesn't exist
                      -- yet...
                      -- For now we return a mostly empty struct. In the future: throwE InvalidHandle
                      pure frameStruct
            ENOENT -> throwE (InvalidFrameID eid)
            e      -> unhdlErr "getFrameFromID" e

setController' :: MonadInIO m => Handle -> ControllerID -> Maybe FrameView -> [ConnectorID] -> Maybe Mode -> Excepts '[ErrorCode] m ()
setController' hdl eid fb conns mode = do
   let
      conns' = fmap unEntityID conns

      (fbid,fbx,fby) = case fb of
         Nothing -> (0,0,0)
         Just (FrameView (EntityID z) x y) -> (z,x,y)

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

-- | Switch to another frame for the given controller without doing a full mode
-- change
switchFrame' :: MonadInIO m => Handle -> ControllerID -> FrameID -> SwitchFrameFlags -> Word64 -> Excepts '[ErrorCode] m ()
switchFrame' hdl cid fsid flags udata = do
   let
      s = StructSwitchFrame (unEntityID cid) (unEntityID fsid) flags 0 udata

   void <| ioctlSwitchFrame s hdl

-- | Get controllers
getHandleControllers :: MonadInIO m => Handle -> Excepts '[InvalidHandle,InvalidControllerID] m [Controller]
getHandleControllers hdl = do
   res <- liftE <| getResources hdl
   traverse (getControllerFromID hdl) (resControllerIDs res)

-- | Get controller gama look-up table
getControllerGamma :: MonadInIO m => Controller -> Excepts '[ErrorCode] m ([Word16],[Word16],[Word16])
getControllerGamma c = do
   let 
      hdl = controllerHandle c
      sz  = controllerGammaTableSize c
      s   = StructControllerLut (unEntityID (controllerID c)) sz

   allocaArrays [sz,sz,sz] $ \(as@[r,g,b] :: [Ptr Word16]) -> do
      let f = fromIntegral . ptrToWordPtr
      void (ioctlGetGamma (s (f r) (f g) (f b)) hdl)
      ~[rs,gs,bs] <- liftIO <| peekArrays [sz,sz,sz] as
      return (rs,gs,bs)

-- | Set controller gama look-up table
setControllerGamma :: MonadInIO m => Controller -> ([Word16],[Word16],[Word16]) -> Excepts '[ErrorCode] m ()
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

-- | Get connector
getConnectorFromID :: forall m.
   ( MonadInIO m
   ) => Handle -> ConnectorID -> Excepts '[InvalidParam,InvalidConnectorID,InvalidProperty] m Connector
getConnectorFromID hdl eid = do
   let
      allocaArray' :: forall n c a b. (MonadInIO n, Integral c, Storable a) => c -> (Ptr a -> n b) -> n b
      allocaArray' n = allocaArray (fromIntegral n)

      getConnector' :: MonadInIO m => StructGetConnector -> Excepts '[InvalidConnectorID,InvalidParam] m StructGetConnector
      getConnector' r =
         ioctlGetConnector r hdl
            |> catchLiftLeft \case
                  EINVAL -> throwE InvalidParam
                  ENOENT -> throwE (InvalidConnectorID eid)
                  e      -> unhdlErr "getConnectorFromID" e

      request = StructGetConnector 0 0 0 0 0 0 0 0 (unEntityID eid)
                  (toEnumField ConnectorTypeUnknown) 0 0 0 0
                  (toEnumField SubPixelNone)

   -- request the Connector info array sizes
   response <- liftE (getConnector' request)

   -- get the full response with arrays filled
   (fullResponse,conn) <- liftE do
      allocaArray' (connModesCount response) $ \(ms :: Ptr StructMode) ->
         allocaArray' (connPropsCount response) $ \(ps :: Ptr Word32) ->
            allocaArray' (connPropsCount response) $ \(pvs :: Ptr Word64) ->
               allocaArray' (connEncodersCount response) $ \(es:: Ptr Word32) -> do
                  -- build a full request this time (with allocated arrays)
                  let
                     cv = fromIntegral . ptrToWordPtr
                     fullRequest = response
                        { connEncodersPtr   = cv es
                        , connModesPtr      = cv ms
                        , connPropsPtr      = cv ps
                        , connPropValuesPtr = cv pvs
                        }
                  fullResponse <- getConnector' fullRequest
                           |> liftE @'[InvalidParam,InvalidConnectorID,InvalidProperty]
                  toConnector hdl response fullResponse
                     ||> (fullResponse,)  -- don't make the recursive call here:
                     |> liftE             -- it's better to free the arrays before

   -- we need to check that the number of resources is still the same (as
   -- resources may have appeared between the time we get the number of
   -- resources and the time we get them...)
   -- If not, we redo the whole process
   if   connModesCount    response < connModesCount    fullResponse
     || connPropsCount    response < connPropsCount    fullResponse
     || connEncodersCount response < connEncodersCount fullResponse
      then getConnectorFromID hdl eid
      else return conn


-- | Convert two StructGetConnector into a Connector
--
-- * "response" contains the actual array sizes
-- * "fullResponse" contains the array values and potentially different array
-- sizes! (checked later by the caller)
toConnector ::
   ( Functor m
   , MonadInIO m
   ) => Handle -> StructGetConnector -> StructGetConnector -> Excepts '[InvalidParam,InvalidProperty] m Connector
toConnector hdl response fullResponse = do
   let
      cv = wordPtrToPtr . fromIntegral
   
      wrapZero 0 = Nothing
      wrapZero x = Just x

      peekArray' :: forall n c a. (MonadIO n, Storable a, Integral c) => c -> Ptr a -> n [a]
      peekArray' n ptr = peekArray (fromIntegral n) ptr
   
   -- read connection state
   state <- case connConnection_ fullResponse of
      1 -> do
            -- properties
            ptrs <- peekArray' (connPropsCount response) (cv (connPropsPtr      fullResponse))
            vals <- peekArray' (connPropsCount response) (cv (connPropValuesPtr fullResponse))
            let rawProps = zipWith RawProperty ptrs vals
            modes <- fmap fromStructMode <$> peekArray' (connModesCount response) (cv (connModesPtr fullResponse))
   
            return (Connected (Display
               modes
               (connWidth_ fullResponse)
               (connHeight_ fullResponse)
               (fromEnumField (connSubPixel_ fullResponse))
               rawProps))
   
      2 -> return Disconnected
      _ -> return ConnectionUnknown
   
   encs  <- fmap EntityID <$> peekArray' (connEncodersCount response) (cv (connEncodersPtr fullResponse))
   
   let mencID = EntityID <$> wrapZero (connEncoderID_ fullResponse)

   -- try to get the controller ID through the Encoder
   mctrlID <- join <|| forM mencID \encId -> do
      getEncoderFromID hdl Nothing encId
         ||> encoderControllerID
         |> catchEvalE (const (pure Nothing))

   return <| Connector
               (EntityID (connConnectorID_ fullResponse))
               (fromEnumField (connConnectorType_ fullResponse))
               (connConnectorTypeID_ fullResponse)
               state
               encs
               mencID
               mctrlID
               hdl




-- | Get graphic card resources
getResources :: forall m. MonadInIO m => Handle -> Excepts '[InvalidHandle] m Resources
getResources hdl = getValues [10,10,10,10] -- try with default values
   where 
      getRes :: StructCardRes -> Excepts '[InvalidHandle] m StructCardRes
      getRes r = ioctlGetResources r hdl
                  |> catchLiftLeft \case
                        EINVAL -> throwE InvalidHandle
                        e      -> unhdlErr "getResources" e

      extractSize x = [csCountFbs, csCountCrtcs, csCountConns, csCountEncs] <*> [x]

      getValues :: [Word32] -> Excepts '[InvalidHandle] m Resources
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
               then liftE <| extractValues r
               else getValues (extractSize r)


      extractValues :: StructCardRes -> Excepts '[] m Resources
      extractValues r = do
         let 
            as  = [csFbIdPtr, csCrtcIdPtr, csConnIdPtr, csEncIdPtr] <*> [r]
            as' = fmap (wordPtrToPtr . fromIntegral) as
            arraySizes = extractSize r
         ~[fbs,ctrls,conns,encs] <- peekArrays arraySizes as'
         return $ Resources
               (fmap EntityID fbs)
               (fmap EntityID ctrls)
               (fmap EntityID conns)
               (fmap EntityID encs)
               (csMinWidth  r)
               (csMaxWidth  r)
               (csMinHeight r)
               (csMaxHeight r)


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


data InvalidPlaneID      = InvalidPlaneID      PlaneID      deriving (Show)
data InvalidControllerID = InvalidControllerID ControllerID deriving (Show)
data InvalidConnectorID  = InvalidConnectorID  ConnectorID  deriving (Show)
data InvalidEncoderID    = InvalidEncoderID    EncoderID    deriving (Show)
data InvalidFrameID      = InvalidFrameID      FrameID      deriving (Show)

-- | Get the IDs of the supported planes
getPlaneIDs :: forall m. MonadInIO m => Handle -> Excepts '[InvalidHandle] m [PlaneID]
getPlaneIDs hdl = getCount >>= getIDs
   where
      gpr s = ioctlGetPlaneResources s hdl

      -- get the number of planes (invariant for a given device)
      getCount :: Excepts '[InvalidHandle] m Word32
      getCount = (gpr (StructGetPlaneRes 0 0) ||> gprsCountPlanes)
                  |> catchLiftLeft \case
                        EINVAL -> throwE InvalidHandle
                        e      -> unhdlErr "getPlaneIDs" e
   
      -- get the plane IDs (invariant for a given device)
      getIDs :: Word32 -> Excepts '[InvalidHandle] m [PlaneID]
      getIDs 0 = return []
      getIDs n = allocaArray (fromIntegral n) $ \(p :: Ptr Word32) -> do
         let p' = fromIntegral (ptrToWordPtr p)
         void (gpr (StructGetPlaneRes p' n))
            |> catchLiftLeft \case
                  EINVAL -> throwE InvalidHandle
                  e      -> unhdlErr "getPlaneIDs" e
         fmap EntityID <$> peekArray (fromIntegral n) p

-- | Get plane information
getPlaneFromID :: forall m. MonadInIO m => Handle -> PlaneID -> Excepts '[InvalidHandle,InvalidPlaneID] m Plane
getPlaneFromID hdl pid = getCount >>= getInfo
   where

      gpr :: StructGetPlane -> Excepts '[InvalidHandle,InvalidPlaneID] m StructGetPlane
      gpr s = ioctlGetPlane s hdl
               |> catchLiftLeft \case
                     EINVAL -> throwE InvalidHandle
                     ENOENT -> throwE (InvalidPlaneID pid)
                     e      -> unhdlErr "getPlaneFromID" e

      toMaybe _ 0 = Nothing
      toMaybe f x = Just (f x)

      -- get the number of formats (invariant for a given plane)
      getCount :: Excepts '[InvalidHandle,InvalidPlaneID] m Word32
      getCount = gpr (StructGetPlane (unEntityID pid) 0 0 BitSet.empty 0 0 0)
                  ||> gpCountFmtTypes 

      -- get the plane info
      getInfo :: Word32 -> Excepts '[InvalidHandle,InvalidPlaneID] m Plane
      getInfo n = allocaArray (fromIntegral n) $ \(p :: Ptr Word32) -> do
         let 
            p' = fromIntegral (ptrToWordPtr p)
            si = StructGetPlane (unEntityID pid) 0 0 BitSet.empty 0 n p'
         gpr si >>= \StructGetPlane{..} -> liftE (getResources hdl) >>= \res -> do
               fmts <- fmap (PixelFormat . BitFields) <$> peekArray (fromIntegral n) p
               return <| Plane
                  { planeID                  = pid
                  , planeControllerId        = toMaybe EntityID gpCrtcId
                  , planeFrameId             = toMaybe EntityID gpFbId
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
setPlane :: MonadInIO m => Handle -> PlaneID -> Maybe (PlaneSource,PlaneTarget) -> Excepts '[InvalidParam,EntryNotFound,InvalidDestRect,InvalidSrcRect] m ()
setPlane hdl pid opts = do

   let 
      makeS cid fsid = StructSetPlane (unEntityID pid) (unEntityID cid)
                                      (unEntityID fsid) BitSet.empty

      e16 = toFixedPoint (0 :: Float)

      s = case opts of
            Nothing -> -- disable the plane
               makeS (EntityID 0) (EntityID 0)
                  0 0 0 0 e16 e16 e16 e16

            Just (PlaneSource{..},PlaneTarget{..}) ->
               makeS planeTargetControllerID planeSourceFrameID
                  planeTargetX planeTargetY planeTargetWidth planeTargetHeight
                  planeSourceX planeSourceY planeSourceHeight planeSourceWidth

   void (ioctlSetPlane s hdl)
      |> catchLiftLeft \case
            EINVAL -> throwE InvalidParam
            ENOENT -> throwE EntryNotFound
            ERANGE -> throwE InvalidDestRect
            ENOSPC -> throwE InvalidSrcRect
            e      -> unhdlErr "setPlane" e

-- | Disable a plane
disablePlane :: MonadInIO m => Handle -> PlaneID -> Excepts '[InvalidParam,EntryNotFound] m ()
disablePlane hdl p = setPlane hdl p Nothing
   -- these errors should not be triggered when we disable a plane
   |> catchDieE (\InvalidDestRect -> unhdlErr "disablePlane" InvalidDestRect)
   |> catchDieE (\InvalidSrcRect  -> unhdlErr "disablePlane" InvalidSrcRect)

-------------------------------------------------------------------------------
-- Blob
-------------------------------------------------------------------------------

-- | Create a blob
createBlob :: MonadInIO m => Handle -> Ptr t -> Word32 -> Excepts '[ErrorCode] m (BlobID t)
createBlob hdl ptr sz = do

   let req = StructCreateBlob
               { cbData   = fromIntegral (ptrToWordPtr ptr)
               , cbLength = sz
               , cbBlobID = 0
               }
   ioctlCreateBlob req hdl
      ||> cbBlobID
      ||> EntityID

-- | Destroy a blob
destroyBlob :: MonadInIO m => Handle -> BlobID t -> Excepts '[ErrorCode] m ()
destroyBlob hdl blobID = do

   let req = StructDestroyBlob (unEntityID blobID)
   void (ioctlDestroyBlob req hdl)

-- | Temporarily create a blob
withBlob :: forall es m a t.
   ( MonadInIO m
   ) => Handle -> Ptr t -> Word32 -> (BlobID t -> Excepts es m a) -> Excepts (ErrorCode ': es) m a
withBlob hdl ptr sz action = do
   bid <- appendE @es (createBlob hdl ptr sz)
   r <- prependE @'[ErrorCode] (action bid)
   appendE @es (destroyBlob hdl bid)
   pure r

-- | Temporarily create a Mode blob
withHandleModeBlob :: forall es m a.
   ( MonadInIO m
   ) => Handle -> Mode -> (BlobID Mode -> Excepts es m a) -> Excepts (ErrorCode ': es) m a
withHandleModeBlob hdl mode action = do
   let struct = toStructMode mode
   with struct \modePtr ->
      withBlob hdl modePtr (sizeOfT' @StructMode) \bid ->
         action (EntityID (unEntityID bid))

-- | Create a Mode blob
createHandleModeBlob ::
   ( MonadInIO m
   ) => Handle -> Mode -> Excepts '[ErrorCode] m (BlobID Mode)
createHandleModeBlob hdl mode = do
   let struct = toStructMode mode
   with struct \modePtr -> do
      createBlob hdl modePtr (sizeOfT' @StructMode)
         ||> castEntityID

-------------------------------------------------------------------------------
-- Property
-------------------------------------------------------------------------------

-- | Show property meta-data
showPropertyMeta :: Bool -> PropertyMeta -> String
showPropertyMeta showpid meta = showPropertyEx showpid meta Nothing

-- | Display a property in a user readable way
showProperty :: Bool -> Property -> String
showProperty showpid (Property meta value) = showPropertyEx showpid meta (Just value)

-- | Display a property-meta in a user readable way (with or without value)
showPropertyEx :: Bool -> PropertyMeta -> Maybe Word64 -> String
showPropertyEx showpid meta mvalue = mconcat
   [ if propertyImmutable meta then "val " else "var "
   , if showpid then "{" ++ show (unEntityID (propertyID meta)) ++ "} " else ""
   , propertyName meta
   , case mvalue of
      Nothing  -> ""
      Just value -> mconcat
         [ " = "
         , case propertyType meta of
            _ | isFP16_16     -> show (fromFixedPointBase (fromIntegral value) :: FP16_16)
            PropRange [0,1]   -> if value == 0 then "False" else "True"
            PropSignedRange _ -> show (fromIntegral value :: Int64)
            PropEnum xs       -> Map.fromList xs Map.! value
            _                 -> show value
         ]
   , " :: "
   , case propertyType meta of
      PropObject         -> "Object"
      PropRange xs
         | isFP16_16              -> "FP16_16"
         | xs == [0,1]            -> "Bool"
         | checkBounds @Word8  xs -> "Word8"
         | checkBounds @Word16 xs -> "Word16"
         | checkBounds @Word32 xs -> "Word32"
         | checkBounds @Word64 xs -> "Word64"
         | otherwise              -> "Range " ++ show xs
      PropSignedRange xs
         | checkBounds @Int8  xs -> "Int8"
         | checkBounds @Int16 xs -> "Int16"
         | checkBounds @Int32 xs -> "Int32"
         | checkBounds @Int64 xs -> "Int64"
         | otherwise             -> "Range " ++ show xs
      PropEnum xs        -> "Enum [" ++ mconcat (List.intersperse "," (fmap snd xs)) ++ "]"
      t                  -> show t
   ]

   where
      -- some properties are known to be in FP16_16 format
      isFP16_16 = case propertyType meta of
         PropRange xs -> checkBounds @Word32 xs
                          && propertyName meta `List.elem` ["SRC_X","SRC_Y","SRC_W","SRC_H"]
         _            -> False

      checkBounds :: forall b a. (Num a, Integral b, Bounded b, Eq a) => [a] -> Bool
      checkBounds [mi,ma] = mi == fromIntegral (minBound @b) && ma == fromIntegral (maxBound @b)
      checkBounds _ = False


data InvalidProperty = InvalidProperty deriving (Show,Eq)

type AtomicErrors = '[InvalidHandle,InvalidParam,MemoryError,InvalidRange,EntryNotFound]


-- | Return meta-information from a property type ID
getPropertyMeta :: forall m. MonadInIO m => Handle -> PropertyID -> Excepts '[InvalidParam,InvalidProperty] m PropertyMeta
getPropertyMeta fd (EntityID pid) = do
   let
      getProperty' :: StructGetProperty -> Excepts '[InvalidParam,InvalidProperty] m StructGetProperty
      getProperty' r = ioctlGetProperty r fd
                        |> catchLiftLeft \case
                              EINVAL -> throwE InvalidParam
                              ENOENT -> throwE InvalidProperty
                              e      -> unhdlErr "getPropertyMeta" e

      req = StructGetProperty
            { gpsValuesPtr   = 0
            , gpsEnumBlobPtr = 0
            , gpsPropId      = pid
            , gpsFlags       = 0
            , gpsName        = emptyCStringBuffer
            , gpsCountValues = 0
            , gpsCountEnum   = 0
            }
   
   -- get value size/number of elements/etc.
   resp <- getProperty' req

   let
      allocaArray' 0 f = f nullPtr
      allocaArray' n f = allocaArray (fromIntegral n) f

      getBlobStruct :: StructGetBlob -> Excepts '[InvalidParam,InvalidProperty] m StructGetBlob
      getBlobStruct r = ioctlGetBlob r fd
                           |> catchLiftLeft \case
                                 EINVAL -> throwE InvalidParam
                                 ENOENT -> throwE InvalidProperty
                                 e      -> unhdlErr "getBlobStruct" e

      -- | Get a blob
      getBlob :: Word32 -> Excepts '[InvalidParam,InvalidProperty] m Buffer
      getBlob bid = do
         let gb = StructGetBlob
                     { gbBlobId = bid
                     , gbLength = 0
                     , gbData   = 0
                     }

         gb' <- getBlobStruct gb
         ptr <- liftIO . mallocBytes . fromIntegral . gbLength $ gb'
         void (getBlobStruct (gb' { gbData = fromIntegral (ptrToWordPtr ptr) }))
            -- free ptr on error
            |> onE_ (liftIO (free ptr))
         -- otherwise return a bytestring
         bufferPackPtr (fromIntegral (gbLength gb')) ptr


      withBuffers :: (Storable a, Storable b) => Word32 -> Word32 -> (Ptr a -> Ptr b ->  Excepts '[InvalidParam,InvalidProperty] m c) -> Excepts '[InvalidParam,InvalidProperty] m c
      withBuffers valueCount blobCount f =
         liftWith (allocaArray' valueCount) $ \valuePtr ->
            liftWith (allocaArray' blobCount) $ \blobPtr -> do
               let gp' = StructGetProperty
                           { gpsValuesPtr   = fromIntegral (ptrToWordPtr valuePtr)
                           , gpsEnumBlobPtr = fromIntegral (ptrToWordPtr blobPtr)
                           , gpsPropId      = pid
                           , gpsFlags       = 0
                           , gpsName        = emptyCStringBuffer
                           , gpsCountValues = valueCount
                           , gpsCountEnum   = blobCount
                           }
               -- nothing changes, except for the two buffers
               _ <- getProperty' gp'
               f valuePtr blobPtr

      withValueBuffer :: Storable a => Word32 -> ([a] -> Excepts '[InvalidParam,InvalidProperty] m c) -> Excepts '[InvalidParam,InvalidProperty] m c
      withValueBuffer n f = withBuffers n 0 $ \ptr (_ :: Ptr Word) ->
         f =<< peekArray (fromIntegral n) ptr
      withBlobBuffer  n f = withBuffers 0 n $ \(_ :: Ptr Word) ptr ->
         f =<< peekArray (fromIntegral n) ptr
      withBuffers' n m f = withBuffers n m $ \p1 p2 -> do
         vs <- peekArray (fromIntegral n) p1
         bs <- peekArray (fromIntegral m) p2
         f vs bs
         
   let
      nval  = gpsCountValues resp
      nblob = gpsCountEnum   resp

   typ <- case getPropertyTypeType resp of
      PropTypeObject      -> return PropObject
      PropTypeRange       -> withValueBuffer nval (return . PropRange)
      PropTypeSignedRange -> withValueBuffer nval (return . PropSignedRange)
      PropTypeEnum        -> withBlobBuffer nblob $ \es ->
         return (PropEnum [(peValue e, fromCStringBuffer $ peName e) | e <- es])
      PropTypeBitmask     -> withBlobBuffer nblob $ \es ->
         return (PropBitmask [(peValue e, fromCStringBuffer $ peName e) | e <- es])

      PropTypeBlob        -> withBuffers' nblob nblob $ \ids bids -> do
         traverse getBlob bids
            ||> (PropBlob . (ids `zip`))
      

   pure (PropertyMeta (EntityID pid) (isImmutable resp) (fromCStringBuffer (gpsName resp)) typ)


-- | Retrieve all the property meta-data
getAllPropertyMeta :: forall m. MonadInIO m => Handle -> m (Map PropertyID PropertyMeta)
getAllPropertyMeta hdl = go 1 Map.empty
   where
      go n !m = do
         mmeta <- getPropertyMeta hdl (EntityID n)
            ||> Just
            |> catchEvalE (const (pure Nothing))
         case mmeta of
            Nothing   -> pure m
            Just meta -> go (n+1) (Map.insert (EntityID n) meta m)

-- | Set object properties atomically
setAtomic :: MonadInIO m => Handle -> AtomicFlags -> [(ObjectID, [RawProperty])] -> Excepts AtomicErrors m ()
setAtomic hdl flags kvs = do

   let
      objs   = fmap fst    kvs                     :: [Word32]
      pvs    = fmap snd    kvs                     :: [[RawProperty]]
      nprops = fmap (fromIntegral . length) pvs    :: [Word32]
      props  = fmap rawPropertyID (concat pvs)     :: [PropertyID]
      vals   = fmap rawPropertyValue  (concat pvs) :: [Word64]

   withArray objs $ \pobjs ->
      withArray nprops $ \pnprops ->
         withArray props $ \pprops ->
            withArray vals $ \pvals -> do
               let
                  toPtr = fromIntegral . ptrToWordPtr
                  s = StructAtomic
                     { atomFlags         = flags
                     , atomCountObjects  = fromIntegral (length objs)
                     , atomObjectsPtr    = toPtr pobjs
                     , atomCountPropsPtr = toPtr pnprops
                     , atomPropsPtr      = toPtr pprops
                     , atomPropValuesPtr = toPtr pvals
                     , atomReserved      = 0 -- must be zero
                     , atomUserData      = 0 -- used for event generation
                     }
               void (ioctlAtomic s hdl)
                  |> catchLiftLeft \case
                        EBADF  -> throwE InvalidHandle
                        EINVAL -> throwE InvalidParam
                        ENOMEM -> throwE MemoryError
                        ENOENT -> throwE EntryNotFound
                        ERANGE -> throwE InvalidRange
                        ENOSPC -> throwE InvalidRange
                        e      -> unhdlErr "setAtomic" e

-- | Info about the DRM driver
data DrmInfo = DrmInfo
   { drmVersionMajor      :: Int32
   , drmVersionMinor      :: Int32
   , drmVersionPatchLevel :: Int32
   , drmDriverName       :: String
   , drmDriverDate       :: String
   , drmDriverDesc       :: String
   } deriving (Show)

-- | Get DRM version
handleGetInfo :: MonadInIO m => Handle -> Excepts '[ErrorCode] m DrmInfo
handleGetInfo hdl = do
   let
      nameSz = 200
      dateSz = 200
      descSz = 2000
   allocaArrays [nameSz,dateSz,descSz] \[namePtr,datePtr,descPtr] -> do
      let req = StructVersion
                  { verMajor      = 0
                  , verMinor      = 0
                  , verPatchLevel = 0
                  , verNameLen    = nameSz
                  , verNamePtr    = fromIntegral (ptrToWordPtr namePtr)
                  , verDateLen    = dateSz
                  , verDatePtr    = fromIntegral (ptrToWordPtr datePtr)
                  , verDescLen    = descSz
                  , verDescPtr    = fromIntegral (ptrToWordPtr descPtr)
                  }
      resp <- ioctlGetVersion req hdl
      nameVal <- peekCStringMaxLen (fromIntegral nameSz) namePtr
      dateVal <- peekCStringMaxLen (fromIntegral dateSz) datePtr
      descVal <- peekCStringMaxLen (fromIntegral descSz) descPtr
      pure <| DrmInfo
         { drmVersionMajor      = verMajor resp
         , drmVersionMinor      = verMinor resp
         , drmVersionPatchLevel = verPatchLevel resp
         , drmDriverName        = nameVal
         , drmDriverDate        = dateVal
         , drmDriverDesc        = descVal
         }

