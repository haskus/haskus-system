{-# LANGUAGE ScopedTypeVariables
           , GeneralizedNewtypeDeriving
           , RecordWildCards #-}

-- | Interface to Linux graphics API
--
-- Linux currently uses KMS/DRM interface
module ViperVM.Arch.Linux.Graphics
   ( Capability(..)
   , Card(..)
   , Connector(..)
   , Connection(..)
   , SubPixel(..)
   , drmIoctl
   , getCapability
   , getCard
   , getConnector
   , getConnectorController
   , getEncoderControllers
   , getEncoderConnectors
   )
where

import ViperVM.Arch.Linux.Graphics.LowLevel (CardStruct(..))

import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Graphics.Encoder
import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Graphics.Connector
import ViperVM.Arch.Linux.Graphics.IDs

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>), (<*>))
import Data.Traversable (traverse)
import Foreign.Marshal.Array (peekArray, allocaArray)
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


data Card = Card
   { cardFrameBuffers    :: [FrameBufferID]
   , cardControllers     :: [ControllerID]
   , cardConnectors      :: [ConnectorID]
   , cardEncoders        :: [EncoderID]
   , cardMinWidth        :: Word32
   , cardMaxWidth        :: Word32
   , cardMinHeight       :: Word32
   , cardMaxHeight       :: Word32
   } deriving (Show)

-- | Get graphic card info
--
-- It seems like the kernel fills *Count fields and min/max fields.  If *Ptr
-- fields are not NULL, the kernel fills the pointed arrays with up to *Count
-- elements.
-- 
getCard :: IOCTL -> FileDescriptor -> SysRet Card
getCard ioctl fd = runEitherT $ do
   let 
      res          = CardStruct 0 0 0 0 0 0 0 0 0 0 0 0
      allocaArray' = allocaArray . fromIntegral
      peekArray'   = peekArray . fromIntegral
      getCard'     = EitherT . ioctlReadWrite ioctl 0x64 0xA0 defaultCheck fd

   -- First we get the number of each resource
   res2 <- getCard' res

   -- then we allocate arrays of appropriate sizes
   (rawRes, retRes) <-
      EitherT $ allocaArray' (csCountFbs res2) $ \(fs :: Ptr Word32) ->
         allocaArray'(csCountCrtcs res2) $ \(crs :: Ptr Word32) ->
            allocaArray' (csCountConns res2) $ \(cs:: Ptr Word32) ->
               allocaArray' (csCountEncs res2) $ \(es:: Ptr Word32) -> runEitherT $ do
                  -- we put them in a new struct
                  let
                     cv = fromIntegral . ptrToWordPtr
                     res3 = res2 { csFbIdPtr   = cv fs
                                 , csCrtcIdPtr = cv crs
                                 , csEncIdPtr  = cv es
                                 , csConnIdPtr = cv cs
                                 }
                  -- we get the values
                  res4 <- getCard' res3
                  res5 <- liftIO $ Card
                     <$> (fmap FrameBufferID <$> peekArray' (csCountFbs res2) fs)
                     <*> (fmap ControllerID <$> peekArray' (csCountCrtcs res2) crs)
                     <*> (fmap ConnectorID <$> peekArray' (csCountConns res2) cs)
                     <*> (fmap EncoderID <$> peekArray' (csCountEncs res2) es)
                     <*> return (csMinWidth res4)
                     <*> return (csMaxWidth res4)
                     <*> return (csMinHeight res4)
                     <*> return (csMaxHeight res4)

                  right (res4, res5)

   -- we need to check that the number of resources is still the same (a
   -- resources may have appeared between the time we get the number of
   -- resources an the time we get them...)
   -- If not, we redo the whole process
   if   csCountFbs   res2 < csCountFbs   rawRes
     || csCountCrtcs res2 < csCountCrtcs rawRes
     || csCountConns res2 < csCountConns rawRes
     || csCountEncs  res2 < csCountEncs  rawRes
      then EitherT $ getCard ioctl fd
      else right retRes





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
   fromMaskedList (encoderPossibleControllers enc) (cardControllers res)

-- | Retrieve Connectors that can work with the given encoder
getEncoderConnectors :: Card -> Encoder -> [ConnectorID]
getEncoderConnectors res enc = 
   fromMaskedList (encoderPossibleConnectors enc) (cardConnectors res)
