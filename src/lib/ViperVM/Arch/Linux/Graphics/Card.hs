{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Graphic card resources
module ViperVM.Arch.Linux.Graphics.Card
   ( Resources(..)
   , FrameBufferID(..)
   , ControllerID(..)
   , ConnectorID(..)
   , EncoderID(..)
   , getResources
   , getEntities
   )
where

import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Internals.Graphics
import ViperVM.Utils.Memory (allocaArrays,peekArrays)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Data.Word
import Foreign.Ptr (Ptr,ptrToWordPtr)
import Foreign.Storable

-- | Graphic card ressources
data Resources = Resources
   { resFrameBufferIDs  :: [FrameBufferID]   -- ^ Frame buffer IDs
   , resControllerIDs   :: [ControllerID]    -- ^ Controller IDs
   , resConnectorIDs    :: [ConnectorID]     -- ^ Connector IDs
   , resEncoderIDs      :: [EncoderID]       -- ^ Encoder IDs
   , resMinWidth        :: Word32            -- ^ Minimal width
   , resMaxWidth        :: Word32            -- ^ Maximal width
   , resMinHeight       :: Word32            -- ^ Minimal height
   , resMaxHeight       :: Word32            -- ^ Maximal height
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
getResources :: Handle -> SysRet Resources
getResources fd = runEitherT $ do
   let 
      res          = StructCardRes 0 0 0 0 0 0 0 0 0 0 0 0
 
      getCard' r   = EitherT (ioctlGetResources r fd)

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

         let res5 = Resources
                     (fmap FrameBufferID fbs)
                     (fmap ControllerID  ctrls)
                     (fmap ConnectorID   conns)
                     (fmap EncoderID     encs)
                     (csMinWidth res4)
                     (csMaxWidth res4)
                     (csMinHeight res4)
                     (csMaxHeight res4)

         right (res4, res5)

   -- we need to check that the number of resources is still the same (as
   -- resources may have appeared between the time we get the number of
   -- resources and the time we get them...)
   -- If not, we redo the whole process
   if   csCountFbs   res2 < csCountFbs   rawRes
     || csCountCrtcs res2 < csCountCrtcs rawRes
     || csCountConns res2 < csCountConns rawRes
     || csCountEncs  res2 < csCountEncs  rawRes
      then EitherT $ getResources fd
      else right retRes


-- | Internal function to retreive card entities from their identifiers
getEntities :: (Resources -> [a]) -> (Handle -> a -> IO (Either x b)) -> Handle -> IO [b]
getEntities getIDs getEntityFromID hdl = do
   res <- runSys $ sysCallAssert "Get resources" $ getResources hdl

   let 
      f (Left _)  xs = xs
      f (Right x) xs = x:xs
      ids            = getIDs res

   xs <- traverse (getEntityFromID hdl) ids
   return (foldr f [] xs)
