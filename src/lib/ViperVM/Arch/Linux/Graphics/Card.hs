{-# LANGUAGE ScopedTypeVariables #-}

-- | Graphic card management
module ViperVM.Arch.Linux.Graphics.Card
   ( Card(..)
   , getCard
   )
where

import ViperVM.Arch.Linux.Graphics.LowLevel (CardStruct(..))
import ViperVM.Arch.Linux.Graphics.IDs
import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>), (<*>))
import Foreign.Ptr (Ptr,ptrToWordPtr)
import Foreign.Marshal.Array (peekArray, allocaArray)
import Data.Word

-- | Graphic card ressources
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
