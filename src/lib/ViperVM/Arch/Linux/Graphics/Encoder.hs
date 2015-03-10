{-# LANGUAGE RecordWildCards
           , GeneralizedNewtypeDeriving #-}

-- | Encoders management
--
-- An encoder converts data obtained from the controller (i.e. from the frame
-- buffer associated with the controller) into suitable data for the connector
-- (i.e. for the device connected to the connector). Hence it only supports a
-- set of connectors. In addition, it may not work with all controllers.
module ViperVM.Arch.Linux.Graphics.Encoder
   ( Encoder(..)
   , EncoderType(..)
   , cardEncoders
   , cardEncoderFromID
   , cardEncoderControllers
   , cardEncoderConnectors
   )
where

import Control.Applicative ((<$>), (<*>))
import Foreign.Storable
import Data.Word
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Data.Bits

import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Graphics.LowLevel.IDs
import ViperVM.Arch.Linux.Graphics.Card

import ViperVM.Arch.Linux.Graphics.LowLevel.Encoder

-- | Get encoder from its ID
cardEncoderFromID :: IOCTL -> Card -> EncoderID -> SysRet Encoder
cardEncoderFromID ioctl card encId = getEncoder ioctl fd encId
   where
      fd = cardFileDescriptor card

-- | Get encoders (discard errors)
cardEncoders :: IOCTL -> Card -> IO [Encoder]
cardEncoders ioctl card = do
   let 
      f (Left _)  xs = xs
      f (Right x) xs = x:xs
      fd = cardFileDescriptor card
      ids = cardEncoderIDs card
   
   xs <- traverse (cardEncoderFromID ioctl card) ids
   return (foldr f [] xs)

-- | Select elements in the list if the bit corresponding to their index is set in the mask
fromMaskedList :: Bits a => a -> [b] -> [b]
fromMaskedList mask xs = foldr f [] xs'
   where
      xs' = [0..] `zip` xs
      f (k,v) vs
         | testBit mask k = v:vs
         | otherwise      = vs

-- | Retrieve Controllers that can work with the given encoder
cardEncoderControllers :: Card -> Encoder -> [ControllerID]
cardEncoderControllers card enc = 
   fromMaskedList (encoderPossibleControllers enc) (cardControllerIDs card)

-- | Retrieve Connectors that can work with the given encoder
cardEncoderConnectors :: Card -> Encoder -> [ConnectorID]
cardEncoderConnectors card enc = 
   fromMaskedList (encoderPossibleConnectors enc) (cardConnectorIDs card)
