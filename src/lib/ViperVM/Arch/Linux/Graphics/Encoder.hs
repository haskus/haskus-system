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
   , getEncoder
   , cardEncoders
   )
where

import Control.Applicative ((<$>), (<*>))
import Foreign.Storable
import Data.Word
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)

import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Graphics.IDs
import ViperVM.Arch.Linux.Graphics.Card

import ViperVM.Arch.Linux.Graphics.LowLevel (EncoderType(..))

-- | An encoder
data Encoder = Encoder
   { encoderID                   :: EncoderID            -- ^ Encoder identifier
   , encoderType                 :: EncoderType          -- ^ Type of the encoder
   , encoderControllerID         :: Maybe ControllerID   -- ^ Associated controller
   , encoderPossibleControllers  :: Word32               -- ^ Bitset of valid controllers
   , encoderPossibleConnectors   :: Word32               -- ^ Bitset of valid connectors
   } deriving (Show)


instance Storable Encoder where
   sizeOf _    = 5*4
   alignment _ = 8
   peek ptr    = do
      let wrapZero 0 = Nothing
          wrapZero x = Just x
      Encoder
         <$> peekByteOff ptr 0

         <*> (toEnum' <$> peekByteOff ptr 4)
         <*> (fmap ControllerID . wrapZero <$> peekByteOff ptr 8)
         <*> peekByteOff ptr 12
         <*> peekByteOff ptr 16
      where
         toEnum' :: Enum a => Word32 -> a
         toEnum' = toEnum . fromIntegral

   poke ptr (Encoder {..}) = do
      pokeByteOff ptr 0 encoderID
      pokeByteOff ptr 4 (fromEnum' encoderType)
      pokeByteOff ptr 8 (fromMaybe (ControllerID 0) encoderControllerID)
      pokeByteOff ptr 12 encoderPossibleControllers
      pokeByteOff ptr 16 encoderPossibleConnectors
      where
         fromEnum' :: Enum a => a -> Word32
         fromEnum' = fromIntegral . fromEnum

-- | Get encoder
getEncoder :: IOCTL -> FileDescriptor -> EncoderID -> SysRet Encoder
getEncoder ioctl fd encId = do
   
   let res = Encoder encId EncoderTypeNone Nothing 0 0

   ioctlReadWrite ioctl 0x64 0xA6 defaultCheck fd res

-- | Get encoders (discard errors)
cardEncoders :: IOCTL -> Card -> IO [Encoder]
cardEncoders ioctl card = do
   let 
      f (Left _)  xs = xs
      f (Right x) xs = x:xs
      fd = cardFileDescriptor card
      ids = cardEncoderIDs card
   
   xs <- traverse (getEncoder ioctl fd) ids
   return (foldr f [] xs)
