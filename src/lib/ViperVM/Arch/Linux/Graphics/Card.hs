{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Graphic card management
module ViperVM.Arch.Linux.Graphics.Card
   ( Card(..)
   -- * Identifiers
   , FrameBufferID(..)
   , ControllerID(..)
   , ConnectorID(..)
   , EncoderID(..)
   , getCardResources
   , cardEntities
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Graphics.Internals
import ViperVM.Utils.Memory (allocaArrays,peekArrays)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Data.Word
import Foreign.Ptr (Ptr,ptrToWordPtr)
import Foreign.Storable

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
   , cardHandle          :: FileDescriptor
   } deriving (Show)

newtype ConnectorID   = ConnectorID Word32 deriving (Show,Eq,Storable)

newtype ControllerID  = ControllerID Word32 deriving (Show,Eq,Storable)

newtype EncoderID     = EncoderID Word32 deriving (Show,Eq,Storable)

newtype FrameBufferID = FrameBufferID Word32 deriving (Show,Eq,Storable)

-- | Get graphic card info
--
-- It seems like the kernel fills *Count fields and min/max fields.  If *Ptr
-- fields are not NULL, the kernel fills the pointed arrays with up to *Count
-- elements.
-- 
getCardResources :: FileDescriptor -> SysRet Card
getCardResources fd = runEitherT $ do
   let 
      res          = StructCardRes 0 0 0 0 0 0 0 0 0 0 0 0
 
      getCard'     = EitherT . ioctlGetResources fd

   -- First we get the number of each resource
   res2 <- getCard' res

   -- then we allocate arrays of appropriate sizes
   let arraySizes = [csCountFbs, csCountCrtcs, csCountConns, csCountEncs] <*> [res2]
   (rawRes, retRes) <- EitherT $ allocaArrays arraySizes $ 
      \(as@[fs,crs,cs,es] :: [Ptr Word32]) -> runEitherT $ do
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

         [fbs,ctrls,conns,encs] <- liftIO $ peekArrays arraySizes as

         let res5 = Card
                     (fmap FrameBufferID fbs)
                     (fmap ControllerID  ctrls)
                     (fmap ConnectorID   conns)
                     (fmap EncoderID     encs)
                     (csMinWidth res4)
                     (csMaxWidth res4)
                     (csMinHeight res4)
                     (csMaxHeight res4)
                     fd

         right (res4, res5)

   -- we need to check that the number of resources is still the same (as
   -- resources may have appeared between the time we get the number of
   -- resources and the time we get them...)
   -- If not, we redo the whole process
   if   csCountFbs   res2 < csCountFbs   rawRes
     || csCountCrtcs res2 < csCountCrtcs rawRes
     || csCountConns res2 < csCountConns rawRes
     || csCountEncs  res2 < csCountEncs  rawRes
      then EitherT $ getCardResources fd
      else right retRes


-- | Internal function to retreive card entities from their identifiers
cardEntities :: (Card -> [a]) -> (Card -> a -> IO (Either x b)) -> Card -> IO [b]
cardEntities getIDs getEntityFromID card = do
   let 
      f (Left _)  xs = xs
      f (Right x) xs = x:xs
      ids = getIDs card
   
   xs <- traverse (getEntityFromID card) ids
   return (foldr f [] xs)
