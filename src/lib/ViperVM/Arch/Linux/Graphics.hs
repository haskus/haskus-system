{-# LANGUAGE DeriveGeneric #-}
-- | Interface to Linux graphics API
--
-- Linux currently uses KMS/DRM interface
module ViperVM.Arch.Linux.Graphics
   ( Capability(..)
   , drmIoctl
   , getCapability
   , cardCapability
   , getConnector
   , getConnectorController
   , getEncoderControllers
   , getEncoderConnectors
   )
where

import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Graphics.Encoder
import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Graphics.Connector
import ViperVM.Arch.Linux.Graphics.IDs
import ViperVM.Arch.Linux.Graphics.Card

import Data.Traversable (traverse)
import Foreign.Storable
import Foreign.CStorable
import Data.Word
import Data.Bits (Bits,testBit)
import GHC.Generics (Generic)

-- | IOCTL for DRM is restarted on interruption
-- Apply this function to your preferred ioctl function
drmIoctl :: IOCTL -> IOCTL
drmIoctl = repeatIoctl

data Capability
   = CapDumbBuffer
   | CapVBlankHighController
   | CapDumbPreferredDepth
   | CapDumbPreferShadow
   | CapPrime
   | CapTimestampMonotonic
   | CapAsyncPageFlip
   deriving (Show,Eq)

instance Enum Capability where
   fromEnum x = case x of 
      CapDumbBuffer           -> 1
      CapVBlankHighController -> 2
      CapDumbPreferredDepth   -> 3
      CapDumbPreferShadow     -> 4
      CapPrime                -> 5
      CapTimestampMonotonic   -> 6
      CapAsyncPageFlip        -> 7
   toEnum x = case x of 
      1 -> CapDumbBuffer
      2 -> CapVBlankHighController
      3 -> CapDumbPreferredDepth
      4 -> CapDumbPreferShadow
      5 -> CapPrime
      6 -> CapTimestampMonotonic
      7 -> CapAsyncPageFlip
      _ -> error "Unknown capability"

-- | Parameter for getCapability IOCTL (capability id, return value)
data GetCapability =
   GetCapability Word64 Word64
   deriving (Generic)

instance CStorable GetCapability
instance Storable GetCapability where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke

-- | Indicate if the given capability is supported
getCapability :: IOCTL -> FileDescriptor -> Capability -> SysRet Word64
getCapability ioctl fd cap = do
   let param = GetCapability (fromIntegral $ fromEnum cap) 0
   ret <- ioctlReadWrite ioctl 0x64 0x0c defaultCheck fd param
   case ret of
      Left err -> return (Left err)
      Right (GetCapability _ value) -> return (Right value)


-- | Get card capability
cardCapability :: IOCTL -> Card -> Capability -> SysRet Word64
cardCapability ioctl card cap = getCapability ioctl (cardFileDescriptor card) cap


-- | Retrieve Controller (and encoder) controling a connector (if any)
getConnectorController :: IOCTL -> FileDescriptor -> Connector -> SysRet (Maybe Controller, Maybe Encoder)
getConnectorController ioctl fd conn = do
   -- Maybe EncoderID
   let encId = connEncoderID conn

   -- Maybe Encoder
   enc <- traverse (getEncoder ioctl fd) encId

   case enc of
      Nothing        -> return (Right (Nothing,Nothing))
      Just (Left err)-> return (Left err)
      Just (Right e) -> do
         -- Maybe ControllerID
         let crtcId = encoderControllerID e

         -- Maybe Controller
         crtc <- traverse (getController ioctl fd) crtcId
         return $ case crtc of
            Nothing        -> Right (Nothing,Just e)
            Just (Left err)-> Left err
            Just (Right c) -> Right (Just c,Just e)

-- | Select elements in the list if the bit corresponding to their index is set in the mask
fromMaskedList :: Bits a => a -> [b] -> [b]
fromMaskedList mask xs = foldr f [] xs'
   where
      xs' = [0..] `zip` xs
      f (k,v) vs
         | testBit mask k = v:vs
         | otherwise      = vs

-- | Retrieve Controllers that can work with the given encoder
getEncoderControllers :: Card -> Encoder -> [ControllerID]
getEncoderControllers res enc = 
   fromMaskedList (encoderPossibleControllers enc) (cardControllerIDs res)

-- | Retrieve Connectors that can work with the given encoder
getEncoderConnectors :: Card -> Encoder -> [ConnectorID]
getEncoderConnectors res enc = 
   fromMaskedList (encoderPossibleConnectors enc) (cardConnectorIDs res)
