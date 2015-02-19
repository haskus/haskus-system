-- | Interface to Linux graphics API
--
-- Linux currently uses KMS/DRM interface
module ViperVM.Arch.Linux.Graphics
   ( Capability(..)
   , drmIoctl
   , getCapability
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

import Control.Applicative ((<$>), (<*>))
import Data.Traversable (traverse)
import Foreign.Storable
import Foreign.Ptr
import Data.Word
import Data.Bits (Bits,testBit)

-- | IOCTL for DRM is restarted on interruption
-- Apply this function to your preferred ioctl function
drmIoctl :: IOCTL -> IOCTL
drmIoctl = repeatIoctl

data Capability
   = CapDUMMY     -- Added to easily derive Enum (start at 0x01...)
   | CapDumbBuffer
   | CapVBlankHighController
   | CapDumbPreferredDepth
   | CapDumbPreferShadow
   | CapPrime
   | CapTimestampMonotonic
   | CapAsyncPageFlip
   deriving (Show,Eq,Enum)

-- | Parameter for getCapability IOCTL (capability id, return value)
data GetCapability = GetCapability Word64 Word64

instance Storable GetCapability where
   sizeOf _ = 16
   alignment _ = 8
   peek ptr = 
      let p = castPtr ptr :: Ptr Word64 in
         GetCapability <$> peekElemOff p 0 <*> peekElemOff p 1
   poke ptr (GetCapability x y) = do
      let p = castPtr ptr :: Ptr Word64
      pokeElemOff p 0 x
      pokeElemOff p 1 y

-- | Indicate if the given capability is supported
getCapability :: IOCTL -> FileDescriptor -> Capability -> SysRet Word64
getCapability ioctl fd cap = do
   let param = GetCapability (fromIntegral $ fromEnum cap) 0
   ret <- ioctlReadWrite ioctl 0x64 0x0c defaultCheck fd param
   case ret of
      Left err -> return (Left err)
      Right (GetCapability _ value) -> return (Right value)



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
