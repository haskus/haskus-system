{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Unique identifiers for graphic objects (used in several modules)
module ViperVM.Arch.Linux.Graphics.IDs
   ( ConnectorID(..)
   , ConnectorTypeID(..)
   , ControllerID(..)
   , EncoderID(..)
   , FrameBufferID(..)
   )
where

import Data.Word
import Foreign.Storable

newtype ConnectorID    = ConnectorID Word32 deriving (Show,Eq,Storable)

newtype ConnectorTypeID = ConnectorTypeID Word32 deriving (Show)

newtype ControllerID   = ControllerID Word32 deriving (Show,Eq,Storable)

newtype EncoderID   = EncoderID Word32 deriving (Show,Eq,Storable)

newtype FrameBufferID  = FrameBufferID Word32 deriving (Show,Eq,Storable)
