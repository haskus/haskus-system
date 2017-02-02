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
module Haskus.Arch.Linux.Graphics.Config
   ( ConfigM
   , Atomic (..)
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
import Haskus.Utils.Variant
import Haskus.Utils.Monad
import Haskus.Arch.Linux.Handle
import Haskus.Arch.Linux.Internals.Graphics
import Haskus.Arch.Linux.Error
import Haskus.Arch.Linux.Graphics.Property
import Haskus.Arch.Linux.Graphics.Object
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
getConfig :: (MonadState ConfigState (ConfigM m), Monad m) => ConfigM m ConfigState
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

-- | Get property meta-data
getPropertyMetaM :: MonadInIO m => PropertyMetaID -> ConfigM m (Variant '[PropertyMeta,InvalidParam,InvalidProperty])
getPropertyMetaM pid = ConfigM $ do
   s <- get
   let meta = configMeta s
   -- lookup in the cache
   case Map.lookup pid meta of
      Just x  -> flowSet x
      Nothing -> liftIO (getPropertyMeta (configHandle s) pid)
         -- add the result to the cache
         >.~=> (\r -> modify (\st -> st { configMeta = Map.insert pid r meta }))


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
getPropertyM ::
   ( MonadInIO m
   , Object o
   , MonadState ConfigState (ConfigM m)
   ) => o -> ConfigM m (Variant '[[Property],ObjectNotFound,InvalidParam])
getPropertyM obj = do
   s <- getConfig
   getObjectProperties (configHandle s) obj
      >.~.> (mapMaybeM (\(RawProperty x y) ->
         getPropertyMetaM x
            >.-.> (\m -> Just (Property m y))
            >..-.> const Nothing
         )
      )


-- | Commit atomic state
commitConfig :: MonadInIO m => Atomic -> CommitOrTest -> AsyncMode -> ModesetMode -> ConfigM m (Variant (() ': AtomicErrors))
commitConfig atomic testMode asyncMode modesetMode = ConfigM $ do
   s <- get

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

         liftIO <| setAtomic hdl flags props

      NonAtomic -> do
         forM_ (Map.toList (configProps s)) <| \((objType,objId,propId),val) -> do
            void (setObjectProperty' hdl objId objType propId val)
         flowSet ()
