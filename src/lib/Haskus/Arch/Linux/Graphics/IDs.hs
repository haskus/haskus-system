{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Haskus.Arch.Linux.Graphics.IDs
   ( FrameBufferID(..)
   , ControllerID(..)
   , ConnectorID(..)
   , EncoderID(..)
   , PlaneID (..)
   )
where

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Storable

-- | Framebuffer ID
newtype FrameBufferID
   = FrameBufferID Word32
   deriving (Show,Eq,Storable,Ord)

-- | Connector ID
newtype ConnectorID
   = ConnectorID Word32
   deriving (Show,Eq,Storable,Ord)

-- | Controller ID
newtype ControllerID
   = ControllerID Word32
   deriving (Show,Eq,Storable,Ord)

-- | Encoder ID
newtype EncoderID
   = EncoderID Word32
   deriving (Show,Eq,Storable,Ord)

-- | Plane identifier
newtype PlaneID
   = PlaneID Word32
   deriving (Show,Eq,Storable,Ord)

