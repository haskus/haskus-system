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

import Data.Traversable (traverse)
import Data.Bits

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Graphics.LowLevel.IDs
import ViperVM.Arch.Linux.Graphics.Card

import ViperVM.Arch.Linux.Graphics.LowLevel.Encoder
import ViperVM.Arch.Linux.Graphics.LowLevel.Card

-- | Get encoder from its ID
cardEncoderFromID :: Card -> EncoderID -> SysRet Encoder
cardEncoderFromID card encId = withCard card getEncoder encId

-- | Get encoders (discard errors)
cardEncoders :: Card -> IO [Encoder]
cardEncoders card = do
   let 
      f (Left _)  xs = xs
      f (Right x) xs = x:xs
      ids = cardEncoderIDs card
   
   xs <- traverse (cardEncoderFromID card) ids
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
