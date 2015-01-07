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
   , ConnectorType(..)
   , drmIoctl
   , getCapability
   , getCard
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

-- | Parameter for MODE_GETRESOURCES IOCTL
data ModeCard = ModeCard
   { fbPtr           :: Word64
   , crtcPtr         :: Word64
   , connectorPtr    :: Word64
   , encoderPtr      :: Word64

   , fbCount         :: Word32
   , crtcCount       :: Word32
   , connectorCount  :: Word32
   , encoderCount    :: Word32

   , minWidth_       :: Word32
   , maxWidth_       :: Word32
   , minHeight_      :: Word32
   , maxHeight_      :: Word32
   } deriving (Show)

instance Storable ModeCard where
   sizeOf _    = 4*(8+4) + 4*4
   alignment _ = 8
   peek ptr    = ModeCard
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr 8
      <*> peekByteOff ptr 16
      <*> peekByteOff ptr 24
      <*> peekByteOff ptr 32
      <*> peekByteOff ptr 36
      <*> peekByteOff ptr 40
      <*> peekByteOff ptr 44
      <*> peekByteOff ptr 48
      <*> peekByteOff ptr 52
      <*> peekByteOff ptr 56
      <*> peekByteOff ptr 60
   poke ptr res = do
      pokeByteOff ptr 0  (fbPtr           res)
      pokeByteOff ptr 8  (crtcPtr         res)
      pokeByteOff ptr 16 (connectorPtr    res)
      pokeByteOff ptr 24 (encoderPtr      res)
      pokeByteOff ptr 32 (fbCount         res)
      pokeByteOff ptr 36 (crtcCount       res)
      pokeByteOff ptr 40 (connectorCount  res)
      pokeByteOff ptr 44 (encoderCount    res)
      pokeByteOff ptr 48 (minWidth_       res)
      pokeByteOff ptr 52 (maxWidth_       res)
      pokeByteOff ptr 56 (minHeight_      res)
      pokeByteOff ptr 60 (maxHeight_      res)


-- | Get graphic card info
--
-- It seems like the kernel fills *Count fields and min/max fields.  If *Ptr
-- fields are not NULL, the kernel fills the pointed arrays with up to *Count
-- elements.
-- 
getCard :: IOCTL -> FileDescriptor -> SysRet Card
getCard ioctl fd = runEitherT $ do
   let 
      res = ModeCard 0 0 0 0 0 0 0 0 0 0 0 0
      allocaArray'      = allocaArray . fromIntegral
      peekArray'        = peekArray . fromIntegral
      getModeResources' = EitherT . ioctlReadWrite ioctl 0x64 0xA0 defaultCheck fd

   -- First we get the number of each resource
   res2 <- getModeResources' res

   -- then we allocate arrays of appropriate sizes
   (rawRes, retRes) <-
      EitherT $ allocaArray' (fbCount res2) $ \(fs :: Ptr Word32) ->
         allocaArray'(crtcCount res2) $ \(crs :: Ptr Word32) ->
            allocaArray' (connectorCount res2) $ \(cs:: Ptr Word32) ->
               allocaArray' (encoderCount res2) $ \(es:: Ptr Word32) -> runEitherT $ do
                  -- we put them in a new struct
                  let
                     cv = fromIntegral . ptrToWordPtr
                     res3 = res2 { fbPtr        = cv fs
                                 , crtcPtr      = cv crs
                                 , encoderPtr   = cv es
                                 , connectorPtr = cv cs
                                 }
                  -- we get the values
                  res4 <- getModeResources' res3
                  res5 <- liftIO $ Card
                     <$> (fmap FrameBufferID <$> peekArray' (fbCount res2) fs)
                     <*> (fmap ControllerID <$> peekArray' (crtcCount res2) crs)
                     <*> (fmap ConnectorID <$> peekArray' (connectorCount res2) cs)
                     <*> (fmap EncoderID <$> peekArray' (encoderCount res2) es)
                     <*> return (minWidth_ res4)
                     <*> return (maxWidth_ res4)
                     <*> return (minHeight_ res4)
                     <*> return (maxHeight_ res4)

                  right (res4, res5)

   -- we need to check that the number of resources is still the same (a
   -- resources may have appeared between the time we get the number of
   -- resources an the time we get them...)
   -- If not, we redo the whole process
   if   fbCount        res2 < fbCount        rawRes
     || crtcCount      res2 < crtcCount      rawRes
     || connectorCount res2 < connectorCount rawRes
     || encoderCount   res2 < encoderCount   rawRes
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
