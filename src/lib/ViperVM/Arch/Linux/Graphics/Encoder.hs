{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

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

import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Internals.Graphics
import ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Format.Binary.Enum
import ViperVM.Utils.Flow

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
getEncoderFromID :: MonadIO m => Handle -> Resources -> EncoderID -> Flow m '[Encoder,EntryNotFound,InvalidHandle]
getEncoderFromID hdl res (EncoderID encId) = liftIO (ioctlGetEncoder enc hdl)
      >.-.> fromStructGetEncoder res hdl
      >%~^> \case
         EINVAL -> flowSet InvalidHandle
         ENOENT -> flowSet EntryNotFound
         e      -> unhdlErr "getEncoder" e
   where
      enc = StructGetEncoder encId (toEnumField EncoderTypeNone)
               0 BitSet.empty BitSet.empty

-- | Controller attached to the encoder, if any
encoderController :: MonadIO m => Encoder -> Flow m '[Maybe Controller ,EntryNotFound,InvalidHandle]
encoderController enc = case encoderControllerID enc of
   Nothing     -> flowSetN @0 Nothing
   Just contId -> getControllerFromID (encoderHandle enc) contId >.-.> Just

-- | Get encoders (discard errors)
getEncoders :: MonadInIO m => Handle -> Flow m '[[Encoder],EntryNotFound,InvalidHandle]
getEncoders hdl = getResources hdl >.~^> \res ->
   flowTraverse (getEncoderFromID hdl res) (resEncoderIDs res)
