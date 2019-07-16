{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Mode-setting configuration
module Haskus.System.Graphics.Config
   ( Command (..)
   , configureGraphics
   , CommitOrTest (..)
   , AsyncMode (..)
   , AllowModeSet (..)
   , makePropertyMap
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
import qualified Data.Map.Strict as Map
import Haskus.Memory.Ptr

-- | Configuration command
--
-- Configuration commands are only used to perform mode-setting (connecting
-- entities, setting properties). They don't allocate any resource (Frame,
-- FrameBuffer, etc.). This is left for a calling function which would allocate
-- these resources beforehand.
data Command
   = CmdConnectorCustom                ConnectorID  RawProperty
   | CmdConnectorController            ConnectorID  ControllerID
   | CmdControllerCustom               ControllerID RawProperty
   | CmdControllerActive               ControllerID Bool
   | CmdControllerMode                 ControllerID (BlobID Mode)
   | CmdControllerVariableRefreshRate  ControllerID Bool
   | CmdControllerOutFencePtr          ControllerID RawPtr
   | CmdPlaneCustom                    PlaneID      RawProperty
   | CmdPlaneSource                    PlaneID      FrameID FP16_16 FP16_16 FP16_16 FP16_16
   | CmdPlanePosition                  PlaneID      Int32 Int32
   | CmdPlaneSize                      PlaneID      Word32 Word32
   | CmdPlaneSourcePosition            PlaneID      FP16_16 FP16_16
   | CmdPlaneSourceSize                PlaneID      FP16_16 FP16_16
   | CmdPlaneTarget                    PlaneID      ControllerID Int32 Int32 Word32 Word32
   | CmdPlaneInFenceHandle             PlaneID      (Maybe Word32)
   deriving (Show)

-- | Get raw property settings from a Command
fromCommand :: GraphicCard -> Command -> [(ObjectID,PropertyID,Word64)]
fromCommand card cmd = props
   where
      props = case cmd of
         CmdConnectorCustom cid raw ->
            [ (unEntityID cid,rawPropertyID raw,rawPropertyValue raw)
            ]
         CmdControllerCustom cid raw ->
            [ (unEntityID cid,rawPropertyID raw,rawPropertyValue raw)
            ]
         CmdPlaneCustom pid raw ->
            [ (unEntityID pid,rawPropertyID raw,rawPropertyValue raw)
            ]
         CmdConnectorController cnid ctid ->
            [ (unEntityID cnid, metaId "CRTC_ID", fromIntegral (unEntityID ctid))
            ]
         CmdControllerActive cid b ->
            [ (unEntityID cid, metaId "ACTIVE", fromBool b)
            ]
         CmdControllerVariableRefreshRate cid b ->
            [ (unEntityID cid, metaId "VRR_ENABLED", fromBool b)
            ]
         CmdControllerMode cid mid ->
            [ (unEntityID cid, metaId "MODE_ID", fromIntegral (unEntityID mid))
            ]
         CmdPlaneSource pid fid x y w h ->
            [ (unEntityID pid, metaId "FB_ID", fromIntegral <| unEntityID fid)
            , (unEntityID pid, metaId "SRC_X", fromIntegral <| getFixedPointBase x)
            , (unEntityID pid, metaId "SRC_Y", fromIntegral <| getFixedPointBase y)
            , (unEntityID pid, metaId "SRC_W", fromIntegral <| getFixedPointBase w)
            , (unEntityID pid, metaId "SRC_H", fromIntegral <| getFixedPointBase h)
            ]
         CmdPlaneTarget pid cid x y w h ->
            [ (unEntityID pid, metaId "CRTC_ID", fromIntegral (unEntityID cid))
            , (unEntityID pid, metaId "CRTC_X", fromIntegral x)
            , (unEntityID pid, metaId "CRTC_Y", fromIntegral y)
            , (unEntityID pid, metaId "CRTC_W", fromIntegral w)
            , (unEntityID pid, metaId "CRTC_H", fromIntegral h)
            ]
         CmdControllerOutFencePtr cid ptr ->
            [ (unEntityID cid, metaId "OUT_FENCE_PTR", fromIntegral (ptrToWordPtr ptr))
            ]
         CmdPlaneInFenceHandle pid mhdl ->
            [ (unEntityID pid, metaId "IN_FENCE_FD", fromMaybeValue mhdl)
            ]
         CmdPlanePosition pid x y ->
            [ (unEntityID pid, metaId "CRTC_X", fromIntegral x)
            , (unEntityID pid, metaId "CRTC_Y", fromIntegral y)
            ]
         CmdPlaneSize pid w h ->
            [ (unEntityID pid, metaId "CRTC_W", fromIntegral w)
            , (unEntityID pid, metaId "CRTC_H", fromIntegral h)
            ]
         CmdPlaneSourcePosition pid x y ->
            [ (unEntityID pid, metaId "SRC_X", fromIntegral <| getFixedPointBase x)
            , (unEntityID pid, metaId "SRC_Y", fromIntegral <| getFixedPointBase y)
            ]
         CmdPlaneSourceSize pid w h ->
            [ (unEntityID pid, metaId "SRC_W", fromIntegral <| getFixedPointBase w)
            , (unEntityID pid, metaId "SRC_H", fromIntegral <| getFixedPointBase h)
            ]

      fromMaybeValue :: forall a. Integral a => Maybe a -> Word64
      fromMaybeValue Nothing  = fromIntegral (-1 :: Int)
      fromMaybeValue (Just x) = fromIntegral x

      fromBool False = 0
      fromBool True  = 1

      metaId n = propertyID (graphicCardMetaPropertiesByName card Map.! n)

-- | Test the configuration or commit it
data CommitOrTest
   = TestOnly  -- ^ Test only
   | Commit    -- ^ Test and commit

-- | Commit during VBLANK interval (synchronous) or as-soon-as-possible
-- (asynchronous, may not be supported)
data AsyncMode
   = Synchronous   -- ^ Synchronous commit
   | Asynchronous  -- ^ Asynchronous commit (may not be supported)

-- | Do we allow full mode-setting
-- 
-- This flag is useful for devices such as tablets whose screen is often
-- shutdown: we can use a degraded mode (scaled, etc.) for a while to save power
-- and only perform the full modeset when the screen is reactivated.
data AllowModeSet
   = AllowFullModeset      -- ^ Allow full mode-setting
   | DisallowFullModeset   -- ^ Don't allow full mode-setting

-- | Convert commands into Object property Map
makePropertyMap :: GraphicCard -> [Command] -> [(ObjectID, [RawProperty])]
makePropertyMap card cmds =
   concatMap (fromCommand card) cmds
      ||> (\(o,p,v) -> (o, [RawProperty p v]))
      |> groupOn fst
      ||> (\xs -> (fst (head xs), concat (fmap snd xs)))


-- | Perform mode-setting
--
-- We use the "atomic" API which should become the standard
configureGraphics :: MonadInIO m => GraphicCard -> CommitOrTest -> AsyncMode -> AllowModeSet -> [Command] -> Excepts AtomicErrors m ()
configureGraphics card testMode asyncMode modesetMode cmds = do
   let
      !flags = BitSet.fromList <| concat <|
         [ case testMode of
            TestOnly            -> [AtomicFlagTestOnly]
            Commit              -> []
         , case asyncMode of
            Synchronous         -> []
            Asynchronous        -> [AtomicFlagNonBlock]
         , case modesetMode of
            AllowFullModeset    -> [AtomicFlagAllowModeset]
            DisallowFullModeset -> []
         ]

   setAtomic (graphicCardHandle card) flags (makePropertyMap card cmds)
