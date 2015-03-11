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
   , encoderControllerIDs
   , encoderConnectorIDs
   , encoderController
   )
where

import Data.Bits
import Control.Applicative ((<$>))

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Graphics.LowLevel.IDs
import ViperVM.Arch.Linux.Graphics.Card

import ViperVM.Arch.Linux.Graphics.LowLevel.Encoder
import ViperVM.Arch.Linux.Graphics.LowLevel.Controller

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
