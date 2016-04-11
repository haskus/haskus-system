{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- | Property
module ViperVM.Arch.Linux.Graphics.Property
   ( PropertyMeta (..)
   , PropertyType (..)
   , RawProperty (..)
   , Property (..)
   , getPropertyMeta
   , InvalidProperty (..)
   )
where

import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Internals.Graphics
import ViperVM.System.Sys
import ViperVM.Utils.Flow
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Format.Binary.Vector as Vec

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Data.Word
import Data.Int
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import Control.Monad (forM)
import Foreign.C.String 

-- | Property meta-information
data PropertyMeta = PropertyMeta
   { propertyID        :: Word32       -- ^ ID of the property type
   , propertyImmutable :: Bool         -- ^ The value won't change
   , propertyPending   :: Bool         -- ^ The value is pending
   , propertyName      :: String       -- ^ Property name
   , propertyType      :: PropertyType -- ^ Type of the property
   } deriving (Show,Eq)

-- | The type of a property
data PropertyType
   = PropRange       [Word64]                   -- ^ A range
   | PropSignedRange [Int64]                    -- ^ A signed range
   | PropEnum        [(Word64,String)]          -- ^ Value-enum
   | PropBitmask     [(Word64,String)]          -- ^ Bit-enum (bitmask)
   | PropBlob        [(Word32,BS.ByteString)]   -- ^ Blob-enum
   | PropObject
   deriving (Show,Eq)

data RawProperty = RawProperty
   { rawPropertyMetaID  :: Word32   -- ^ Card-wise property meta-info ID
   , rawPropertyValue   :: Word64   -- ^ Property value
   } deriving (Show,Eq)

data Property = Property
   { propertyMeta       :: PropertyMeta -- ^ Meta-information about the property
   , propertyValue      :: Word64       -- ^ Value of the property
   } deriving (Show,Eq)


type PropertyMetaID = Word32

data InvalidProperty = InvalidProperty deriving (Show,Eq)

-- | Return meta-information from a property type ID
getPropertyMeta :: Handle -> PropertyMetaID -> Sys (Flow '[InvalidParam,InvalidProperty] PropertyMeta)
getPropertyMeta fd pid = runFlowT $ do
      -- get value size/number of elements/etc.
      g <- liftFlowT (getProperty' gp)
      -- get values
      ptype <- liftFlowT $ getValues (gpsCountValues g)
                  (gpsCountEnum g) (getPropertyTypeType g)
      return $ PropertyMeta pid
         (isImmutable g)
         (isPending g)
         (convertCString (gpsName g))
         ptype
   where
      getProperty' :: StructGetProperty -> Sys (Flow '[InvalidParam,InvalidProperty] StructGetProperty)
      getProperty' r = sysIO (ioctlGetProperty r fd) >>= \case
         Left EINVAL -> flowSet InvalidParam
         Left ENOENT -> flowSet InvalidProperty
         Right g     -> flowRet g
         Left e      -> unhdlErr "getPropertyMeta" e


      gp = StructGetProperty
            { gpsValuesPtr   = 0
            , gpsEnumBlobPtr = 0
            , gpsPropId      = pid
            , gpsFlags       = 0
            , gpsName        = Vec.replicate (castCharToCChar '\0')
            , gpsCountValues = 0
            , gpsCountEnum   = 0
            }
   
      allocaArray' 0 f = f nullPtr
      allocaArray' n f = allocaArray (fromIntegral n) f

      getBlobStruct :: StructGetBlob -> Sys (Flow '[InvalidParam,InvalidProperty] StructGetBlob)
      getBlobStruct r = sysIO (ioctlGetBlob r fd) >>= \case
         Left EINVAL -> flowSet InvalidParam
         Left ENOENT -> flowSet InvalidProperty
         Right g     -> flowRet g
         Left e      -> unhdlErr "getBlobStruct" e

      -- | Get a blob
      getBlob :: Word32 -> Sys (Flow '[InvalidParam,InvalidProperty] BS.ByteString)
      getBlob bid = runFlowT $ do
         let gb = StructGetBlob
                     { gbBlobId = bid
                     , gbLength = 0
                     , gbData   = 0
                     }
         gb' <- liftFlowT (getBlobStruct gb)
         ptr <- liftFlowM $ sysIO $ mallocBytes (fromIntegral (gbLength gb'))
         _   <- liftFlowT $ getBlobStruct (gb' { gbData = fromIntegral (ptrToWordPtr ptr) })
         liftFlowM $ sysIO $ BS.unsafePackMallocCStringLen (ptr, fromIntegral (gbLength gb'))


      withBuffers :: (Storable a, Storable b) => Word32 -> Word32 -> (Ptr a -> Ptr b ->  Sys (Flow '[InvalidParam,InvalidProperty] c)) -> Sys (Flow '[InvalidParam,InvalidProperty] c)
      withBuffers valueCount blobCount f =
         sysWith (allocaArray' valueCount) $ \valuePtr ->
            sysWith (allocaArray' blobCount) $ \blobPtr -> do
               let gp' = StructGetProperty
                           { gpsValuesPtr   = fromIntegral (ptrToWordPtr valuePtr)
                           , gpsEnumBlobPtr = fromIntegral (ptrToWordPtr blobPtr)
                           , gpsPropId      = pid
                           , gpsFlags       = 0
                           , gpsName        = Vec.replicate (castCharToCChar '\0')
                           , gpsCountValues = valueCount
                           , gpsCountEnum   = blobCount
                           }
               -- nothing changes, except for the two buffers
               _ <- getProperty' gp'
               f valuePtr blobPtr

      withValueBuffer n f = withBuffers n 0 $ \ptr (_ :: Ptr ()) ->
         f =<< sysIO (peekArray (fromIntegral n) ptr)
      withBlobBuffer  n f = withBuffers 0 n $ \(_ :: Ptr ()) ptr ->
         f =<< sysIO (peekArray (fromIntegral n) ptr)
      withBuffers' n m f = withBuffers n m $ \p1 p2 -> do
         vs <- sysIO (peekArray (fromIntegral n) p1)
         bs <- sysIO (peekArray (fromIntegral m) p2)
         f vs bs
         
      getValues :: Word32 -> Word32 -> PropertyTypeType -> Sys (Flow '[InvalidParam,InvalidProperty] PropertyType)
      getValues nval nblob ttype = case ttype of
         PropTypeObject      -> flowRet PropObject
         PropTypeRange       -> withValueBuffer nval (flowRet . PropRange)
         PropTypeSignedRange -> withValueBuffer nval (flowRet . PropSignedRange)
         PropTypeEnum        -> withBlobBuffer nblob $ \es ->
            flowRet (PropEnum [(peValue e, convertCString $ peName e) | e <- es])
         PropTypeBitmask     -> withBlobBuffer nblob $ \es ->
            flowRet (PropBitmask [(peValue e, convertCString $ peName e) | e <- es])

         PropTypeBlob        -> withBuffers' nblob nblob $ \ids bids -> runFlowT $ do
            bids' <- forM bids (liftFlowT . getBlob)
            return (PropBlob (ids `zip` bids'))
