{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

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
   , cardEncoders
   , cardEncoderFromID
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Graphics.Internals
import ViperVM.Format.Binary.BitSet as BitSet

-- | An encoder
data Encoder = Encoder
   { encoderID                  :: EncoderID          -- ^ Encoder identifier
   , encoderType                :: EncoderType        -- ^ Type of the encoder
   , encoderControllerID        :: Maybe ControllerID -- ^ Associated controller
   , encoderPossibleControllers :: [ControllerID]     -- ^ Valid controllers
   , encoderPossibleClones      :: [EncoderID]        -- ^ Valid clone encoders
   , encoderCard                :: Card               -- ^ Graphic card
   } deriving (Show)

fromStructGetEncoder :: Card -> StructGetEncoder -> Encoder
fromStructGetEncoder card StructGetEncoder{..} =
      Encoder
         (EncoderID geEncoderId)
         (toEnum (fromIntegral geEncoderType))
         (if geCrtcId == 0
            then Nothing
            else Just (ControllerID geCrtcId))
         (pick' (cardControllerIDs card) gePossibleCrtcs)
         (pick' (cardEncoderIDs card) gePossibleClones)
         card
   where
      -- pick the elements in es whose indexes are in bs
      pick' es bs = pick es 0 (BitSet.elems bs)

      pick :: [a] -> Int -> [Int] -> [a]
      pick [] _ _ = []
      pick _ _ [] = []
      pick (x:xs) n (i:is)
         | n == i    = x : pick xs (n+1) is
         | otherwise = pick xs (n+1) (i:is)

-- | Get an encoder from its ID
cardEncoderFromID :: Card -> EncoderID -> SysRet Encoder
cardEncoderFromID card (EncoderID encId) = do
   let res = StructGetEncoder encId 0 0 BitSet.empty BitSet.empty
   fmap (fromStructGetEncoder card) <$> ioctlGetEncoder (cardHandle card) res

-- | Controller attached to the encoder, if any
encoderController :: Encoder -> SysRet (Maybe Controller)
encoderController enc = case encoderControllerID enc of
   Nothing    -> return (Right Nothing)
   Just contId -> fmap Just <$> cardControllerFromID (encoderCard enc) contId

-- | Get encoders (discard errors)
cardEncoders :: Card -> IO [Encoder]
cardEncoders = cardEntities cardEncoderIDs cardEncoderFromID
