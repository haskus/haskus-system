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
import Haskus.Utils.Variant
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
readGraphicsState :: MonadInIO m => Handle -> Flow m '[GraphicsState,InvalidHandle]
readGraphicsState hdl = do
   -- get resource IDs
   mres <- getResources hdl
   case popVariant @Resources mres of
      Left xs   -> return (liftVariant xs)
      Right res -> do
         let fbs = resFrameSourceIDs res
         -- read connectors, encoders and controllers
         mconns <- flowTraverse (getConnectorFromID hdl) (resConnectorIDs res)
         mencs  <- flowTraverse (getEncoderFromID hdl res) (resEncoderIDs res)
         mctrls <- flowTraverse (getControllerFromID hdl) (resControllerIDs res)
         -- read planes
         mplanes <- getPlaneResources hdl
            >.~^> flowTraverse (getPlane hdl)
            -- shouldn't happen, planes are invariant
            >..%~!!> (\(InvalidPlane _)  -> error "Invalid plane" )

         case (popVariant mconns, popVariant mencs, popVariant mctrls, popVariant mplanes) of
            (Right conns, Right encs, Right ctrls, Right planes) ->
               flowSet (buildGraphicsState conns encs ctrls planes fbs)
            -- on failure we restart the process
            -- TODO: check that we don't loop indefinitely
            _ -> readGraphicsState hdl   


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
getEncoderFromID :: MonadIO m => Handle -> Resources -> EncoderID -> Flow m '[Encoder,EntryNotFound,InvalidHandle]
getEncoderFromID hdl res encId = liftIO (ioctlGetEncoder enc hdl)
      >.-.> fromStructGetEncoder res hdl
      >%~^> \case
         EINVAL -> flowSet InvalidHandle
         ENOENT -> flowSet EntryNotFound
         e      -> unhdlErr "getEncoder" e
   where
      enc = StructGetEncoder (unEntityID encId) (toEnumField EncoderTypeNone)
               0 BitSet.empty BitSet.empty

-- | Get encoders (discard errors)
getEncoders :: MonadInIO m => Handle -> Flow m '[[Encoder],EntryNotFound,InvalidHandle]
getEncoders hdl = getResources hdl >.~^> \res ->
   flowTraverse (getEncoderFromID hdl res) (resEncoderIDs res)

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
getControllerFromID :: MonadIO m => Handle -> ControllerID -> Flow m '[Controller,EntryNotFound, InvalidHandle]
getControllerFromID hdl eid = liftIO (ioctlGetController crtc hdl)
      >.-.> fromStructController hdl
      >%~^> \case
         EINVAL -> flowSet InvalidHandle
         ENOENT -> flowSet EntryNotFound
         e      -> unhdlErr "getController" e
   where
      crtc = emptyStructController { contID = unEntityID eid }


setController' :: MonadInIO m => Handle -> ControllerID -> Maybe Frame -> [ConnectorID] -> Maybe Mode -> Flow m '[(),ErrorCode]
setController' hdl eid fb conns mode = do
   let
      conns' = fmap unEntityID conns

      (fbid,fbx,fby) = case fb of
         Nothing -> (0,0,0)
         Just (Frame (EntityID z) x y) -> (z,x,y)

   withArray conns' $ \conArray -> do
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

      liftIO (ioctlSetController crtc hdl) >.-.> const ()

-- | Switch to another framebuffer for the given controller
-- without doing a full mode change
--
-- Called "mode_page_flip" in the original terminology
switchFrameBuffer' :: MonadIO m => Handle -> ControllerID -> FrameSourceID -> PageFlipFlags -> Word64 -> Flow m '[(),ErrorCode]
switchFrameBuffer' hdl cid fsid flags udata = do
   let
      s = StructPageFlip (unEntityID cid) (unEntityID fsid) flags 0 udata

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
      hdl = controllerHandle c
      sz  = controllerGammaTableSize c
      s   = StructControllerLut (unEntityID (controllerID c)) sz

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
      hdl = controllerHandle c
      sz' = controllerGammaTableSize c
      sz  = fromIntegral sz'
      s   = StructControllerLut (unEntityID (controllerID c)) sz'
      ss  = [take sz rs,take sz gs, take sz bs]

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
getConnectorFromID hdl eid = getConnector' hdl res >.~^> getValues
   where
      res = StructGetConnector 0 0 0 0 0 0 0 0 (unEntityID eid)
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
                  then getConnectorFromID hdl eid
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

   encs  <- fmap EntityID <$> peekArray' (connEncodersCount res2) (cv (connEncodersPtr res4))

   state .-.> \st -> Connector
         (EntityID (connConnectorID_ res4))
         (fromEnumField (connConnectorType_ res4))
         (connConnectorTypeID_ res4)
         st
         encs
         (EntityID <$> wrapZero (connEncoderID_ res4))
         hdl


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


-- | Error invalid plane ID
data InvalidPlane = InvalidPlane PlaneID deriving (Show)

-- | Get the IDs of the supported planes
getPlaneResources :: forall m. MonadInIO m => Handle -> Flow m '[[PlaneID], InvalidHandle]
getPlaneResources hdl = getCount >.~^> getIDs
   where
      gpr s = liftIO (ioctlGetPlaneResources s hdl)

      -- get the number of planes (invariant for a given device)
      getCount :: Flow m '[Word32,InvalidHandle]
      getCount = gpr (StructGetPlaneRes 0 0)
         >.-.> gprsCountPlanes
         >..%~^> \case
            EINVAL -> flowSet InvalidHandle
            e      -> unhdlErr "getPlaneResources" e
   
      -- get the plane IDs (invariant for a given device)
      getIDs :: Word32 -> Flow m '[[PlaneID],InvalidHandle]
      getIDs 0 = flowSetN @0 []
      getIDs n = allocaArray (fromIntegral n) $ \(p :: Ptr Word32) -> do
         let p' = fromIntegral (ptrToWordPtr p)
         gpr (StructGetPlaneRes p' n)
            >..%~^> \case
               EINVAL -> flowSet InvalidHandle
               e      -> unhdlErr "getPlaneResources" e
            >.~.> \_ -> fmap EntityID <$> peekArray (fromIntegral n) p

-- | Get plane information
getPlane :: forall m. MonadInIO m => Handle -> PlaneID -> Flow m '[Plane,InvalidHandle,InvalidPlane]
getPlane hdl pid = getCount >.~^> getInfo
   where

      gpr :: StructGetPlane -> Flow m '[StructGetPlane,InvalidHandle,InvalidPlane]
      gpr s = liftIO (ioctlGetPlane s hdl)
         >..%~^> \case
            EINVAL -> flowSet InvalidHandle
            ENOENT -> flowSet (InvalidPlane (EntityID (gpPlaneId s)))
            e      -> unhdlErr "getPlane" e

      toMaybe _ 0 = Nothing
      toMaybe f x = Just (f x)

      -- get the number of formats (invariant for a given plane)
      getCount :: Flow m '[Word32,InvalidHandle,InvalidPlane]
      getCount = gpr (StructGetPlane (unEntityID pid) 0 0 BitSet.empty 0 0 0)
         >.-.> gpCountFmtTypes 

      -- get the plane info (invariant for a given plane)
      getInfo :: Word32 -> Flow m '[Plane,InvalidHandle,InvalidPlane]
      getInfo n = allocaArray (fromIntegral n) $ \(p :: Ptr Word32) -> do
         let 
            p' = fromIntegral (ptrToWordPtr p)
            si = StructGetPlane (unEntityID pid) 0 0 BitSet.empty 0 n p'
         gpr si
            >.~^> \StructGetPlane{..} -> getResources hdl >.~^> \res -> do
               -- TODO: controllers are invariant, we should store them
               -- somewhere to avoid getResources
               fmts <- fmap (PixelFormat . BitFields) <$> peekArray (fromIntegral n) p
               flowSet Plane
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
setPlane :: MonadIO m => Handle -> PlaneID -> Maybe (ControllerID, FrameSourceID, SrcRect, DestRect) -> Flow m '[(),InvalidParam,EntryNotFound,InvalidDestRect,InvalidSrcRect]
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

   liftIO (ioctlSetPlane s hdl)
      >.-.>   const ()
      >..%~^> \case
         EINVAL -> flowSet InvalidParam
         ENOENT -> flowSet EntryNotFound
         ERANGE -> flowSet InvalidDestRect
         ENOSPC -> flowSet InvalidSrcRect
         e      -> unhdlErr "setPlane" e

-- | Disable a plane
disablePlane :: MonadIO m => Handle -> PlaneID -> Flow m '[(),InvalidParam,EntryNotFound]
disablePlane hdl p = setPlane hdl p Nothing
   -- these errors should not be triggered when we disable a plane
   >..%~!!> (\InvalidDestRect -> unhdlErr "disablePlane" InvalidDestRect)
   >..%~!!> (\InvalidSrcRect  -> unhdlErr "disablePlane" InvalidSrcRect)
