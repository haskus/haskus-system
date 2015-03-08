{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Graphic card management
module ViperVM.Arch.Linux.Graphics.LowLevel.Card
   ( Card(..)
   , getCard
   )
where

import ViperVM.Arch.Linux.Graphics.LowLevel.IDs
import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>), (<*>))
import Foreign.Ptr (Ptr,ptrToWordPtr)
import Foreign.Marshal.Array (peekArray, allocaArray)
import Data.Word
import Foreign.Storable
import Foreign.CStorable
import GHC.Generics (Generic)

-- | Graphic card ressources
data Card = Card
   { cardFrameBufferIDs  :: [FrameBufferID]
   , cardControllerIDs   :: [ControllerID]
   , cardConnectorIDs    :: [ConnectorID]
   , cardEncoderIDs      :: [EncoderID]
   , cardMinWidth        :: Word32
   , cardMaxWidth        :: Word32
   , cardMinHeight       :: Word32
   , cardMaxHeight       :: Word32
   , cardFileDescriptor  :: FileDescriptor
   } deriving (Show)

-- | Data matching the C structure drm_mode_card_res
data CardStruct = CardStruct
   { csFbIdPtr    :: Word64
   , csCrtcIdPtr  :: Word64
   , csConnIdPtr  :: Word64
   , csEncIdPtr   :: Word64
   , csCountFbs   :: Word32
   , csCountCrtcs :: Word32
   , csCountConns :: Word32
   , csCountEncs  :: Word32
   , csMinWidth   :: Word32
   , csMaxWidth   :: Word32
   , csMinHeight  :: Word32
   , csMaxHeight  :: Word32
   } deriving Generic

instance CStorable CardStruct
instance Storable CardStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek



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
 
      -- allocate several arrays with the same type at once, call f on the list of arrays
      allocaArrays' sizes f = go [] sizes
         where
            go as []     = f (reverse as)
            go as (x:xs) = allocaArray (fromIntegral x) $ \a -> go (a:as) xs

      peekArray'   = peekArray . fromIntegral
      getCard'     = EitherT . ioctlReadWrite ioctl 0x64 0xA0 defaultCheck fd

   -- First we get the number of each resource
   res2 <- getCard' res

   -- then we allocate arrays of appropriate sizes
   let arraySizes = [csCountFbs, csCountCrtcs, csCountConns, csCountEncs] <*> [res2]
   (rawRes, retRes) <- EitherT $ allocaArrays' arraySizes $ 
      \([fs,crs,cs,es] :: [Ptr Word32]) -> runEitherT $ do
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
            <*> (fmap ControllerID  <$> peekArray' (csCountCrtcs res2) crs)
            <*> (fmap ConnectorID   <$> peekArray' (csCountConns res2) cs)
            <*> (fmap EncoderID     <$> peekArray' (csCountEncs res2) es)
            <*> return (csMinWidth res4)
            <*> return (csMaxWidth res4)
            <*> return (csMinHeight res4)
            <*> return (csMaxHeight res4)
            <*> return fd

         right (res4, res5)

   -- we need to check that the number of resources is still the same (as
   -- resources may have appeared between the time we get the number of
   -- resources and the time we get them...)
   -- If not, we redo the whole process
   if   csCountFbs   res2 < csCountFbs   rawRes
     || csCountCrtcs res2 < csCountCrtcs rawRes
     || csCountConns res2 < csCountConns rawRes
     || csCountEncs  res2 < csCountEncs  rawRes
      then EitherT $ getCard ioctl fd
      else right retRes

