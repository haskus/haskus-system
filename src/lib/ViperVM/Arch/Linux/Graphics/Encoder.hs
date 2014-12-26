{-# LANGUAGE RecordWildCards
           , GeneralizedNewtypeDeriving #-}

-- | Encoders
module ViperVM.Arch.Linux.Graphics.Encoder
   ( Encoder(..)
   , getEncoder
   )
where

import Control.Applicative ((<$>), (<*>))
import Foreign.Storable
import Data.Word
import Data.Maybe (fromMaybe)

import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Graphics.IDs


newtype EncoderType = EncoderType Word32 deriving (Storable,Show)

data Encoder = Encoder
   { encoderID                   :: EncoderID
   , encoderType                 :: EncoderType
   , encoderControllerID         :: Maybe ControllerID
   , encoderPossibleControllers  :: Word32
   , encoderPossibleClones       :: Word32
   } deriving (Show)


instance Storable Encoder where
   sizeOf _    = 5*4
   alignment _ = 8
   peek ptr    = do
      let wrapZero 0 = Nothing
          wrapZero x = Just x
      Encoder
         <$> peekByteOff ptr 0

         <*> (EncoderType <$> peekByteOff ptr 4)
         <*> (fmap ControllerID . wrapZero <$> peekByteOff ptr 8)
         <*> peekByteOff ptr 12
         <*> peekByteOff ptr 16

   poke ptr (Encoder {..}) = do
      pokeByteOff ptr 0 encoderID
      pokeByteOff ptr 4 encoderType
      pokeByteOff ptr 8 (fromMaybe (ControllerID 0) encoderControllerID)
      pokeByteOff ptr 12 encoderPossibleControllers
      pokeByteOff ptr 16 encoderPossibleClones

-- | Get encoder
getEncoder :: IOCTL -> FileDescriptor -> EncoderID -> SysRet Encoder
getEncoder ioctl fd encId = do
   
   let res = Encoder encId (EncoderType 0) Nothing 0 0

   ioctlReadWrite ioctl 0x64 0xA6 defaultCheck fd res


