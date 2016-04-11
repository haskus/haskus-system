{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

-- | Encoders management
--
-- An encoder converts data obtained from the controller (i.e. from the frame
-- buffer associated with the controller) into suitable data for the connector
-- (i.e. for the device connected to the connector). Hence it only supports a
-- set of connectors. In addition, it may not work with all controllers.
module ViperVM.Arch.Linux.Graphics.Encoder
   ( Encoder(..)
   , EncoderType(..)
   , encoderController
   , getEncoders
   , getEncoderFromID
   )
where

import ViperVM.System.Sys
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Internals.Graphics
import ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Format.Binary.Enum
import ViperVM.Utils.Flow

import Control.Monad (forM)

-- | An encoder
data Encoder = Encoder
   { encoderID                  :: EncoderID          -- ^ Encoder identifier
   , encoderType                :: EncoderType        -- ^ Type of the encoder
   , encoderControllerID        :: Maybe ControllerID -- ^ Associated controller
   , encoderPossibleControllers :: [ControllerID]     -- ^ Valid controllers
   , encoderPossibleClones      :: [EncoderID]        -- ^ Valid clone encoders
   , encoderHandle              :: Handle             -- ^ Graphic card
   } deriving (Show)

fromStructGetEncoder :: Resources -> Handle -> StructGetEncoder -> Encoder
fromStructGetEncoder res hdl StructGetEncoder{..} =
      Encoder
         (EncoderID geEncoderId)
         (fromEnumField geEncoderType)
         (if geCrtcId == 0
            then Nothing
            else Just (ControllerID geCrtcId))
         (pickControllers res gePossibleCrtcs)
         (pickEncoders    res gePossibleClones)
         hdl

-- | Get an encoder from its ID
getEncoderFromID :: Handle -> Resources -> EncoderID -> Sys (Flow '[EntryNotFound,InvalidHandle] Encoder)
getEncoderFromID hdl res (EncoderID encId) = sysIO (ioctlGetEncoder enc hdl) >>= \case
      Right e     -> flowRet (fromStructGetEncoder res hdl e)
      Left EINVAL -> flowSet (InvalidHandle hdl)
      Left ENOENT -> flowSet EntryNotFound
      Left e      -> unhdlErr "getEncoder" e
   where
      enc = StructGetEncoder encId (toEnumField EncoderTypeNone)
               0 BitSet.empty BitSet.empty

-- | Controller attached to the encoder, if any
encoderController :: Encoder -> Sys (Flow '[EntryNotFound,InvalidHandle] (Maybe Controller))
encoderController enc = case encoderControllerID enc of
   Nothing     -> flowRet Nothing
   Just contId -> getControllerFromID (encoderHandle enc) contId `flowSeqM` (return . Just)

-- | Get encoders (discard errors)
getEncoders :: Handle -> Sys (Flow '[InvalidHandle,InvalidParam,EntryNotFound] [Encoder])
getEncoders hdl = runFlowT $ do
   res <- liftFlowT $ getResources hdl
   forM (resEncoderIDs res) (liftFlowT . getEncoderFromID hdl res)
