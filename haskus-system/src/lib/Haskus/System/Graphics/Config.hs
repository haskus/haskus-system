{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Mode-setting configuration
module Haskus.System.Graphics.Config
   ( configureGraphics
   , CommitOrTest (..)
   , VSync (..)
   , FullModeSet (..)
   -- * Config monad
   , getConfigCard
   , Config (..)
   , setConnectorSource
   , setPlaneTarget
   , setProperty
   , setRawProperty
   , setPlaneSourcePosition
   , setPlaneSourceSize
   , setPlaneSourceSubSize
   , setPlanePosition
   , setPlaneSize
   , setPlaneSource
   , enableController
   , enableVariableRefreshRate
   , setOutFencePtr
   , setInFenceHandle
   , setMode
   , lookupPropertyID
   , getPropertyID
   )
where

import Haskus.System.Linux.Graphics.Entities
import Haskus.System.Linux.Graphics.Mode
import Haskus.System.Linux.Graphics.State
import Haskus.System.Linux.Internals.Graphics
import Haskus.System.Graphics
import Haskus.Utils.Flow
import Haskus.Utils.List as List
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.FixedPoint
import qualified Haskus.Format.Binary.BitSet as BitSet
import Haskus.Memory.Ptr

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad.Trans.State.Strict

-- | Config monad state
data ConfigState = ConfigState
   { cfgCard       :: GraphicCard                      -- ^ Graphic card (stored for convenience)
   , cfgProperties :: Map (ObjectID,PropertyID) Word64 -- ^ Properties to set
   }

-- | Configuration monad
--
-- Commands are only used to perform mode-setting (connecting entities, setting
-- properties). They don't allocate any resource (Frame, FrameBuffer, Mode, etc.).
-- This is left for a calling function which should allocate these resources
-- beforehand.
newtype Config a = Config (State ConfigState a) deriving (Functor,Applicative,Monad)

emptyConfigState :: GraphicCard -> ConfigState
emptyConfigState card = ConfigState card Map.empty

-- | Get config properties
configProperties :: GraphicCard -> Config a -> Map (ObjectID,PropertyID) Word64
configProperties card (Config s) = cfgProperties (execState s (emptyConfigState card))

-- | Retrieve the graphic card
getConfigCard :: Config GraphicCard
getConfigCard = Config (state \s -> (cfgCard s, s))

-- | Set a property
setProperty :: ObjectID -> PropertyID -> Word64 -> Config ()
setProperty objid pid val = Config <| state \s ->
   ( ()
   , s { cfgProperties = Map.insert (objid,pid) val (cfgProperties s) }
   )

-- | Set object property
setObjectProperty :: Object o => o -> PropertyID -> Word64 -> Config ()
setObjectProperty o pid val = setProperty (getObjectID o) pid val

-- | Get a property ID by name
--
-- Fail if missing
getPropertyID :: String -> Config PropertyID
getPropertyID n = do
   card <- getConfigCard
   pure (propertyID (graphicCardMetaPropertiesByName card Map.! n))

-- | Get a property ID by name
lookupPropertyID :: String -> Config (Maybe PropertyID)
lookupPropertyID n = do
   card <- getConfigCard
   pure (propertyID <|| Map.lookup n (graphicCardMetaPropertiesByName card))

-- | Set a raw property
setRawProperty :: Object o => o -> RawProperty -> Config ()
setRawProperty o p = setObjectProperty o (rawPropertyID p) (rawPropertyValue p)

-- | Link a connector to a controller
setConnectorSource :: (IsConnector cn, IsController ct) => cn -> ct -> Config ()
setConnectorSource cn ct = do
   pid <- getPropertyID "CRTC_ID"
   setObjectProperty (getConnectorID cn) pid (fromIntegral (unEntityID (getControllerID ct)))

-- | Link a plane to a controller
setPlaneTarget :: (IsPlane p, IsController c) => p -> c -> Config ()
setPlaneTarget p c = do
   pid <- getPropertyID "CRTC_ID"
   setObjectProperty (getPlaneID p) pid (fromIntegral (unEntityID (getControllerID c)))

-- | Set Plane source position
setPlaneSourcePosition :: IsPlane p => p -> FP16_16 -> FP16_16 -> Config ()
setPlaneSourcePosition p x y = do
   srcX <- getPropertyID "SRC_X"
   srcY <- getPropertyID "SRC_Y"
   setObjectProperty (getPlaneID p) srcX (fromIntegral <| getFixedPointBase x)
   setObjectProperty (getPlaneID p) srcY (fromIntegral <| getFixedPointBase y)

-- | Set Plane source size allowing subpixel indexing
setPlaneSourceSubSize :: IsPlane p => p -> FP16_16 -> FP16_16 -> Config ()
setPlaneSourceSubSize p w h = do
   srcW <- getPropertyID "SRC_W"
   srcH <- getPropertyID "SRC_H"
   setObjectProperty (getPlaneID p) srcW (fromIntegral <| getFixedPointBase w)
   setObjectProperty (getPlaneID p) srcH (fromIntegral <| getFixedPointBase h)

-- | Set Plane source size
setPlaneSourceSize :: IsPlane p => p -> Word32 -> Word32 -> Config ()
setPlaneSourceSize p w h =
   setPlaneSourceSubSize p (fromIntegral w) (fromIntegral h)

-- | Set Plane position
setPlanePosition :: (IsPlane p) => p -> Int32 -> Int32 -> Config ()
setPlanePosition p x y = do
   srcX <- getPropertyID "CRTC_X"
   srcY <- getPropertyID "CRTC_Y"
   setObjectProperty (getPlaneID p) srcX (fromIntegral x)
   setObjectProperty (getPlaneID p) srcY (fromIntegral y)

-- | Set Plane size
setPlaneSize :: (IsPlane p) => p -> Word32 -> Word32 -> Config ()
setPlaneSize p w h = do
   srcW <- getPropertyID "CRTC_W"
   srcH <- getPropertyID "CRTC_H"
   setObjectProperty (getPlaneID p) srcW (fromIntegral w)
   setObjectProperty (getPlaneID p) srcH (fromIntegral h)

-- | Set Plane source
setPlaneSource :: (IsPlane p, IsFrame f) => p -> f -> Config ()
setPlaneSource p f = do
   fbid <- getPropertyID "FB_ID"
   setObjectProperty (getPlaneID p) fbid (fromIntegral <| unEntityID (getFrameID f))

-- | Enable or disable a controller
enableController :: (IsController c) => c -> Bool -> Config ()
enableController c b = do
   act <- getPropertyID "ACTIVE"
   setObjectProperty (getControllerID c) act (if b then 1 else 0)

-- | Enable or disable variable refresh rate
enableVariableRefreshRate :: IsController c => c -> Bool -> Config ()
enableVariableRefreshRate c b = do
   prop <- getPropertyID "VRR_ENABLED"
   setObjectProperty (getControllerID c) prop (if b then 1 else 0)

-- | Set out fence barrier ptr
setOutFencePtr :: IsController c => c -> RawPtr -> Config ()
setOutFencePtr c ptr = do
   prop <- getPropertyID "OUT_FENCE_PTR"
   setObjectProperty (getControllerID c) prop (fromIntegral (ptrToWordPtr ptr))

-- | Set in fence handle (FD)
--
-- Use Nothing to disable
setInFenceHandle :: IsPlane p => p -> Maybe Word32 -> Config ()
setInFenceHandle p mhdl = do
   prop <- getPropertyID "IN_FENCE_FD"
   setObjectProperty (getPlaneID p) prop
      case mhdl of
         Nothing -> fromIntegral (-1 :: Int)
         Just x  -> fromIntegral x

-- | Set display mode
setMode :: IsController c => c -> BlobID Mode -> Config ()
setMode c mid = do
   prop <- getPropertyID "MODE_ID"
   setObjectProperty (getControllerID c) prop (fromIntegral (unEntityID mid))

-- | Test the configuration or commit it
data CommitOrTest
   = TestOnly  -- ^ Test only
   | Commit    -- ^ Test and commit

-- | Commit during VBLANK interval (synchronous) or as-soon-as-possible
-- (asynchronous, may not be supported)
data VSync
   = EnableVSync  -- ^ Vertical sync
   | DisableVSync -- ^ No vertical sync (may not be supported, check the capability)
   deriving (Show,Eq,Ord)

-- | Do we allow full mode-setting
-- 
-- This flag is useful for devices such as tablets whose screen is often
-- shutdown: we can use a degraded mode (scaled, etc.) for a while to save power
-- and only perform the full modeset when the screen is reactivated.
data FullModeSet
   = EnableFullModeset    -- ^ Allow full mode-setting
   | DisableFullModeset   -- ^ Don't allow full mode-setting

-- | Configure the graphics pipeline by submitting a batch of commands
configureGraphics :: MonadInIO m => GraphicCard -> CommitOrTest -> VSync -> FullModeSet -> Config a -> Excepts AtomicErrors m ()
configureGraphics card testMode asyncMode modesetMode cfg = do
   let
      !flags = BitSet.fromList <| concat <|
         [ case testMode of
            TestOnly            -> [AtomicFlagTestOnly]
            Commit              -> []
         , case asyncMode of
            EnableVSync         -> []
            DisableVSync        -> [AtomicFlagSwitchFrameAsync]
         , case modesetMode of
            EnableFullModeset   -> [AtomicFlagAllowModeset]
            DisableFullModeset  -> []
         , [ AtomicFlagSwitchFrameGenerateEvent -- always generate event
           , AtomicFlagNonBlock                 -- always non-blocking?
           ]
         ]

      props = configProperties card cfg
               |> Map.assocs
               ||> (\((o,p),v) -> (o, [RawProperty p v]))
               |> groupOn fst -- group properties by object
               ||> (\xs -> (fst (head xs), concat (fmap snd xs)))

   setAtomic (graphicCardHandle card) flags props
