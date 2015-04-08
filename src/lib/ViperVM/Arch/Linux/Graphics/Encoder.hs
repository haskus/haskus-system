{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Encoders management
--
-- An encoder converts data obtained from the controller (i.e. from the frame
-- buffer associated with the controller) into suitable data for the connector
-- (i.e. for the device connected to the connector). Hence it only supports a
-- set of connectors. In addition, it may not work with all controllers.
module ViperVM.Arch.Linux.Graphics.Encoder
   ( Encoder(..)
   , EncoderType(..)
   , encoderControllerIDs
   , encoderConnectorIDs
   , encoderController
   , cardEncoders
   , cardEncoderFromID
   )
where

import Data.Bits
import Data.Word
import Foreign.CStorable
import Foreign.Storable
import GHC.Generics (Generic)

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Ioctl

-- | An encoder
data Encoder = Encoder
   { encoderID                   :: EncoderID            -- ^ Encoder identifier
   , encoderType                 :: EncoderType          -- ^ Type of the encoder
   , encoderControllerID         :: Maybe ControllerID   -- ^ Associated controller
   , encoderPossibleControllers  :: Word32               -- ^ Bitset of valid controllers
   , encoderPossibleConnectors   :: Word32               -- ^ Bitset of valid connectors
   , encoderCard                 :: Card                 -- ^ Graphic card
   } deriving (Show)


-- | Type of the encoder
data EncoderType
   = EncoderTypeNone
   | EncoderTypeDAC
   | EncoderTypeTMDS
   | EncoderTypeLVDS
   | EncoderTypeTVDAC
   deriving (Eq,Ord,Show,Enum)

-- | Data matching the C structure drm_mode_get_encoder
data EncoderStruct = EncoderStruct
   { geEncoderId      :: Word32
   , geEncoderType    :: Word32
   , geCrtcId         :: Word32
   , gePossibleCrtcs  :: Word32
   , gePossibleClones :: Word32
   } deriving Generic

instance CStorable EncoderStruct
instance Storable EncoderStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

fromEncoderStruct :: Card -> EncoderStruct -> Encoder
fromEncoderStruct card (EncoderStruct {..}) =
   Encoder
      (EncoderID geEncoderId)
      (toEnum (fromIntegral geEncoderType))
      (if geCrtcId == 0
         then Nothing
         else Just (ControllerID geCrtcId))
      gePossibleCrtcs
      gePossibleClones
      card

-- | Get an encoder from its ID
cardEncoderFromID :: Card -> EncoderID -> SysRet Encoder
cardEncoderFromID card (EncoderID encId) = withCard card $ \ioctl fd -> do
   let res = EncoderStruct encId 0 0 0 0
   fmap (fromEncoderStruct card) <$> ioctlReadWrite ioctl 0x64 0xA6 defaultCheck fd res

-- | Select elements in the list if the bit corresponding to their index is set in the mask
fromMaskedList :: Bits a => a -> [b] -> [b]
fromMaskedList mask xs = foldr f [] xs'
   where
      xs' = [0..] `zip` xs
      f (k,v) vs
         | testBit mask k = v:vs
         | otherwise      = vs

-- | Retrieve Controllers that can work with the given encoder
encoderControllerIDs :: Encoder -> [ControllerID]
encoderControllerIDs enc = let card = encoderCard enc in
   fromMaskedList (encoderPossibleControllers enc) (cardControllerIDs card)

-- | Retrieve Connectors that can work with the given encoder
encoderConnectorIDs :: Encoder -> [ConnectorID]
encoderConnectorIDs enc = let card = encoderCard enc in
   fromMaskedList (encoderPossibleConnectors enc) (cardConnectorIDs card)

-- | Controller attached to the encoder, if any
encoderController :: Encoder -> SysRet (Maybe Controller)
encoderController enc = case encoderControllerID enc of
   Nothing    -> return (Right Nothing)
   Just contId -> fmap Just <$> cardControllerFromID (encoderCard enc) contId

-- | Get encoders (discard errors)
cardEncoders :: Card -> IO [Encoder]
cardEncoders = cardEntities cardEncoderIDs cardEncoderFromID
