{-# LANGUAGE ScopedTypeVariables #-}
-- | Interface to Linux graphics API
--
-- Linux currently uses KMS/DRM interface
module ViperVM.Arch.Linux.Graphics
   ( Capability(..)
   , CardResources(..)
   , drmIoctl
   , getCapability
   , getModeResources
   )
where

import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>), (<*>))
import Foreign.Marshal.Array (peekArray, allocaArray)
import Foreign.Storable
import Foreign.Ptr
import Data.Word


-- | IOCTL for DRM is restarted on interruption
-- Apply this function to your preferred ioctl function
drmIoctl :: IOCTL -> IOCTL
drmIoctl = repeatIoctl

data Capability
   = CapDUMMY     -- Added to easily derive Enum (start at 0x01...)
   | CapDumbBuffer
   | CapVBlankHighCRTC
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

data CardResources = CardResources
   { framebuffers    :: [Word32]
   , crtcs           :: [Word32]
   , connectors      :: [Word32]
   , encoders        :: [Word32]
   , minWidth        :: Word32
   , maxWidth        :: Word32
   , minHeight       :: Word32
   , maxHeight       :: Word32
   }

-- | Parameter for MODE_GETRESOURCES IOCTL
data ModeCardRes = ModeCardRes
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
   }

instance Storable ModeCardRes where
   sizeOf _    = 4*(8+4) + 4*4
   alignment _ = 8
   peek ptr    = ModeCardRes
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


-- | Get mode resources
--
-- It seems like the kernel fills *Count fields and min/max fields.  If *Ptr
-- fields are not NULL, the kernel fills the pointed arrays with up to *Count
-- elements.
-- 
getModeResources :: IOCTL -> FileDescriptor -> SysRet CardResources
getModeResources ioctl fd = runEitherT $ do
   let 
      res = ModeCardRes 0 0 0 0 0 0 0 0 0 0 0 0
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
                  res5 <- liftIO $ CardResources
                     <$> peekArray' (fbCount res2) fs
                     <*> peekArray' (crtcCount res2) fs
                     <*> peekArray' (connectorCount res2) fs
                     <*> peekArray' (encoderCount res2) fs
                     <*> return (minWidth_ res4)
                     <*> return (maxWidth_ res4)
                     <*> return (minHeight_ res4)
                     <*> return (minHeight_ res4)

                  right (res4, res5)

   -- we need to check that the number of resources is still the same (a
   -- resources may have appeared between the time we get the number of
   -- resources an the time we get them...)
   -- If not, we redo the whole process
   if   fbCount        res2 < fbCount        rawRes
     || crtcCount      res2 < crtcCount      rawRes
     || connectorCount res2 < connectorCount rawRes
     || encoderCount   res2 < encoderCount   rawRes
      then EitherT $ getModeResources ioctl fd
      else right retRes

