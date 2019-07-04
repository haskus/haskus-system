{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

-- | Mode-setting configuration
module Haskus.System.Graphics.Config
   ( Command (..)
   , configureGraphics
   , CommitOrTest (..)
   , AsyncMode (..)
   , AllowModeSet (..)
   )
where

import Haskus.System.Linux.Graphics.Entities
import Haskus.System.Linux.Graphics.Property
import Haskus.System.Linux.Graphics.Mode
import Haskus.System.Linux.Graphics.Object
import Haskus.System.Linux.Internals.Graphics
import Haskus.System.Graphics
import Haskus.Utils.Flow
import Haskus.Utils.List
import Haskus.Format.Binary.Word
import qualified Haskus.Format.Binary.BitSet as BitSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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
   | CmdControllerMode                 ControllerID Mode
   | CmdControllerVariableRefreshRate  ControllerID Bool
   | CmdPlaneCustom                    PlaneID      RawProperty
   | CmdPlaneSource                    PlaneID      FrameID Word32 Word32 Word32 Word32
   | CmdPlanePosition                  PlaneID      Int32 Int32
   | CmdPlaneTarget                    PlaneID      ControllerID Int32 Int32 Word32 Word32
   | CmdPlaneInFenceHandle             PlaneID      (Maybe Word32)
   | CmdPlaneOutFencePtr               PlaneID      Word64
   deriving (Show)

type PropMap = Map (ObjectID,Word32) Word64

-- | Insert a command in the map
insertCommand :: PropMap -> Command -> PropMap
insertCommand m = \case
   CmdConnectorCustom cid raw ->
      Map.insert (getObjectID cid,rawPropertyMetaID raw) (rawPropertyValue raw) m
   CmdControllerCustom cid raw ->
      Map.insert (getObjectID cid,rawPropertyMetaID raw) (rawPropertyValue raw) m
   CmdPlaneCustom pid raw ->
      Map.insert (getObjectID pid,rawPropertyMetaID raw) (rawPropertyValue raw) m
   
-- | Test the configuration or commit it
data CommitOrTest
   = TestOnly  -- ^ Test only
   | Commit    -- ^ Test and commit

-- | Asynchronous commit?
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

      -- properties:  (object id, property id) -> property value
      -- (used to get unique property assignements)
      propMap = foldl' insertCommand Map.empty cmds

      -- properties: object id -> (property id, property value)
      props = propMap
               |> Map.toList
               ||> (\((objId,propId),val) -> (objId,[RawProperty propId val]))
               |> Map.fromListWith (++)

   setAtomic (graphicCardHandle card) flags props
