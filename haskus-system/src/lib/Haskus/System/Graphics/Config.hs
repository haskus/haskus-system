-- | Mode-setting configuration
module Haskus.System.Graphics.Config
   ( Config (..)
   , ConfigError
   , setConfig
   )
where

import Haskus.System.Linux.Graphics.Entities
import Haskus.System.Linux.Graphics.Property
import Haskus.System.Linux.Graphics.Mode
import Haskus.System.Linux.Graphics.Object
import Haskus.System.Linux.Graphics.State
import Haskus.System.Graphics
import Haskus.Utils.Flow

-- | This datatype represents a declarative mode-setting configuration.
-- It indicates which entities should be connected to which other entities,
-- which properties to set, etc.
-- Then we can use this to perform mode-setting (either with the legacy non-atomic
-- interface or with the atomic interface). We can also use it to check that the
-- configuration is valid.
--
-- This config is only used to perform mode-setting. It doesn't allocate
-- any resource (Frame, FrameBuffer, etc.). This is left for a
-- calling function which would allocate these resources beforehand. The calling
-- function can choose to allocate accelerated buffers or not, etc.
data Config = Config
   { configController :: [( ControllerID
                          , Maybe Mode
                          , [ConnectorID]
                          )] -- ^ Controller config
   , configPlane      :: [( PlaneID
                          , Maybe
                            ( ControllerID
                            , FrameID
                            , SrcRect
                            , DestRect
                            )
                          )] -- ^ Plane config
   , configProperties :: [( ObjectID
                          , ObjectType
                          , PropID
                          , PropValue
                          )] -- ^ Set properties
   }

-------------------------------------------------------------------------------
-- Generic config
-------------------------------------------------------------------------------

type ConfigError = ()

-- | Apply the given config
setConfig :: GraphicCard -> Config -> IO ConfigError
setConfig card config = do
   -- TODO: support atomic config
   let isAtomicSupported _ = False

   if isAtomicSupported card
      then setConfigAtomic card config
      else setConfigLegacy card config

-------------------------------------------------------------------------------
-- Legacy config
-------------------------------------------------------------------------------

-- | Apply the given config with the legacy interface
setConfigLegacy :: GraphicCard -> Config -> IO ConfigError
setConfigLegacy card config = do
   let hdl = graphicCardHandle card

   -- FIXME: we should perform error checking and report errors to the caller

   ----------------------------------------------------------------------
   -- disconnect entities being modified....
   ----------------------------------------------------------------------

   -- disable planes
   forM_ (configPlane config) <| \(pid,_) ->
      runE_ <| setPlane hdl pid Nothing

   ----------------------------------------------------------------------
   -- ...and then reconnect them in order and set properties
   ----------------------------------------------------------------------

   -- configure controllers
   forM_ (configController config) <| \(cid,mmode,conns) ->
      case (mmode,conns) of
         (Nothing,[]) -> return () -- nothing to do
         _            -> runE_ <| setController' hdl cid Nothing conns mmode

   -- attach planes
   forM_ (configPlane config) <| \(pid,mopts) ->
      case mopts of
         Nothing -> return () -- nothing to do
         _       -> runE_ <| setPlane hdl pid mopts

   -- set properties
   forM_ (configProperties config) <| \(oid,otype,propid,val) ->
      runE_ <| setObjectProperty' hdl oid otype propid val

-------------------------------------------------------------------------------
-- Atomic config
-------------------------------------------------------------------------------

-- | Apply the given config with the atomic interface
setConfigAtomic :: GraphicCard -> Config -> IO ConfigError
setConfigAtomic _card _config = undefined
