{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Graphics configuration
--
-- DRM now has a single entry-point for changing the configuration: the atomic
-- ioctl. We can test and commit a whole configuration without going through
-- intermediate states. Legacy object properties are accessible through object
-- properties. An atomic modification is a list of (object, property, value)
-- tuples.
module Haskus.System.Linux.Graphics.AtomicConfig
   ( Atomic (..)
   , CommitOrTest (..)
   , AsyncMode (..)
   , ModesetMode (..)
   , graphicsConfig
   , commitConfig
   , getPropertyMetaM
   , setPropertyM
   , getPropertyM
   )
where

import Haskus.Utils.Flow
import Haskus.Utils.Monad
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Internals.Graphics
import Haskus.System.Linux.Error
import Haskus.System.Linux.Graphics.Property
import Haskus.System.Linux.Graphics.Object
import qualified Haskus.Format.Binary.BitSet as BitSet

import Data.Map as Map
import Control.Monad.State

-- | PropertyM state
data ConfigState = ConfigState
   { configHandle :: Handle                                     -- ^ Card handle
   , configProps  :: Map (ObjectType,ObjectID,PropID) PropValue -- ^ Properties to set
   , configMeta   :: Map PropertyMetaID PropertyMeta            -- ^ Property meta
   }

-- | Configuration monad
newtype ConfigM m a
   = ConfigM (StateT ConfigState m a)
   deriving (Applicative, Monad, MonadIO, MonadInIO, MonadTrans)

deriving instance Functor m => Functor (ConfigM m)
deriving instance Monad m => MonadState ConfigState (ConfigM m)

-- | Get config state
getConfig :: Monad m => ConfigM m ConfigState
getConfig = ConfigM get

data Atomic
   = Atomic
   | NonAtomic

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
data ModesetMode
   = AllowFullModeset      -- ^ Allow full mode-setting
   | DisallowFullModeset   -- ^ Don't allow full mode-setting


-- | Configure a graphics device (atomically if supported)
graphicsConfig :: MonadInIO m => Handle -> ConfigM m a -> m a
graphicsConfig handle (ConfigM f) = evalStateT f s
   where
      s = ConfigState handle Map.empty Map.empty

-- | Lookup in the property meta cache
getPropertyFromCache :: MonadInIO m => PropertyMetaID -> ConfigM m (Maybe PropertyMeta)
getPropertyFromCache pid = ConfigM <| do
   s <- get
   return (Map.lookup pid (configMeta s))

-- | Store in the property meta cache
storePropertyIntoCache :: MonadInIO m => PropertyMetaID -> PropertyMeta -> ConfigM m ()
storePropertyIntoCache pid val = ConfigM <|
   modify (\st -> st { configMeta = Map.insert pid val (configMeta st) })


-- | Get property meta-data
getPropertyMetaM :: MonadInIO m => PropertyMetaID -> FlowT '[InvalidParam, InvalidProperty] (ConfigM m) PropertyMeta
getPropertyMetaM pid = do
   mp <- lift (getPropertyFromCache pid)
   case mp of
      Just x  -> return x
      Nothing -> do
         s <- lift getConfig
         r <- getPropertyMeta (configHandle s) pid
         lift (storePropertyIntoCache pid r)
         return r


-- | Set property
setPropertyM ::
   ( MonadInIO m
   , Object o
   ) => o -> PropID -> PropValue -> ConfigM m ()
setPropertyM obj prop val = ConfigM $ do
   modify (\s ->
      let
         key   = (getObjectType obj, getObjectID obj, prop)
         props = Map.insert key val (configProps s)
      in s { configProps = props })

-- | Get properties
getPropertyM :: forall m o.
   ( MonadInIO m
   , Object o
   ) => o -> FlowT '[InvalidParam,ObjectNotFound] (ConfigM m) [Property]
getPropertyM obj = do
   s <- lift getConfig
   props <- getObjectProperties (configHandle s) obj
   let
      getPropertyFromRaw :: RawProperty -> FlowT '[InvalidParam,InvalidProperty] (ConfigM m) (Maybe Property)
      getPropertyFromRaw (RawProperty x y) = getPropertyMetaM x ||> (\m -> Just (Property m y))

      fromRawProp :: RawProperty -> ConfigM m (Maybe Property)
      fromRawProp p = getPropertyFromRaw p
                        -- in case of failure to get the meta, we just skip the property
                        -- (i.e. we return Nothing)
                        |> evalCatchFlowT (const (return Nothing))

   mapMaybeM (\x -> lift (fromRawProp x)) props


-- | Commit atomic state
commitConfig :: MonadInIO m => Atomic -> CommitOrTest -> AsyncMode -> ModesetMode -> FlowT (ObjectNotFound ': AtomicErrors) (ConfigM m) ()
commitConfig atomic testMode asyncMode modesetMode = do
   s <- lift getConfig

   let hdl = configHandle s

   case atomic of
      Atomic -> do
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

            props = Map.toList (configProps s)
                     ||> (\((_objType,objId,propId),val) -> (objId,[(propId,val)]))
                     |> Map.fromListWith (++)

         liftFlowT <| setAtomic hdl flags props

      NonAtomic -> do
         forM_ (Map.toList (configProps s)) <| \((objType,objId,propId),val) -> do
            void (liftFlowT <| setObjectProperty' hdl objId objType propId val)
