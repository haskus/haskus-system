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
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.Buffer
import ViperVM.Format.String 

import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc


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
   = PropRange       [Word64]          -- ^ A range
   | PropSignedRange [Int64]           -- ^ A signed range
   | PropEnum        [(Word64,String)] -- ^ Value-enum
   | PropBitmask     [(Word64,String)] -- ^ Bit-enum (bitmask)
   | PropBlob        [(Word32,Buffer)] -- ^ Blob-enum
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
getPropertyMeta :: Handle -> PropertyMetaID -> Flow Sys '[PropertyMeta,InvalidParam,InvalidProperty]
getPropertyMeta fd pid = do
      -- get value size/number of elements/etc.
      getProperty' gp >.~^> \g -> do
         getValues (gpsCountValues g) (gpsCountEnum g) (getPropertyTypeType g)
            >.-.> PropertyMeta pid
               (isImmutable g)
               (isPending g)
               (fromCStringBuffer (gpsName g))
   where
      getProperty' :: StructGetProperty -> Flow Sys '[StructGetProperty,InvalidParam,InvalidProperty]
      getProperty' r = sysIO (ioctlGetProperty r fd) >%~^> \case
         EINVAL -> flowSet InvalidParam
         ENOENT -> flowSet InvalidProperty
         e      -> unhdlErr "getPropertyMeta" e


      gp = StructGetProperty
            { gpsValuesPtr   = 0
            , gpsEnumBlobPtr = 0
            , gpsPropId      = pid
            , gpsFlags       = 0
            , gpsName        = emptyCStringBuffer
            , gpsCountValues = 0
            , gpsCountEnum   = 0
            }
   
      allocaArray' 0 f = f nullPtr
      allocaArray' n f = allocaArray (fromIntegral n) f

      getBlobStruct :: StructGetBlob -> Flow Sys '[StructGetBlob,InvalidParam,InvalidProperty]
      getBlobStruct r = sysIO (ioctlGetBlob r fd) >%~^> \case
         EINVAL -> flowSet InvalidParam
         ENOENT -> flowSet InvalidProperty
         e      -> unhdlErr "getBlobStruct" e

      -- | Get a blob
      getBlob :: Word32 -> Flow Sys '[Buffer,InvalidParam,InvalidProperty]
      getBlob bid = do
         let gb = StructGetBlob
                     { gbBlobId = bid
                     , gbLength = 0
                     , gbData   = 0
                     }

         getBlobStruct gb >.~^> \gb' -> do
            ptr <- sysIO . mallocBytes . fromIntegral . gbLength $ gb'
            getBlobStruct (gb' { gbData = fromIntegral (ptrToWordPtr ptr) })
               -- free ptr on error
               >..~=> const (sysIO (free ptr))
               -- otherwise return a bytestring
               >.~.> const (sysIO (bufferPackPtr (fromIntegral (gbLength gb')) ptr))


      withBuffers :: (Storable a, Storable b) => Word32 -> Word32 -> (Ptr a -> Ptr b ->  Flow Sys '[c,InvalidParam,InvalidProperty]) -> Flow Sys '[c,InvalidParam,InvalidProperty]
      withBuffers valueCount blobCount f =
         sysWith (allocaArray' valueCount) $ \valuePtr ->
            sysWith (allocaArray' blobCount) $ \blobPtr -> do
               let gp' = StructGetProperty
                           { gpsValuesPtr   = fromIntegral (ptrToWordPtr valuePtr)
                           , gpsEnumBlobPtr = fromIntegral (ptrToWordPtr blobPtr)
                           , gpsPropId      = pid
                           , gpsFlags       = 0
                           , gpsName        = emptyCStringBuffer
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
         
      getValues :: Word32 -> Word32 -> PropertyTypeType -> Flow Sys '[PropertyType,InvalidParam,InvalidProperty]
      getValues nval nblob ttype = case ttype of
         PropTypeObject      -> flowRet0 PropObject
         PropTypeRange       -> withValueBuffer nval (flowRet0 . PropRange)
         PropTypeSignedRange -> withValueBuffer nval (flowRet0 . PropSignedRange)
         PropTypeEnum        -> withBlobBuffer nblob $ \es ->
            flowRet0 (PropEnum [(peValue e, fromCStringBuffer $ peName e) | e <- es])
         PropTypeBitmask     -> withBlobBuffer nblob $ \es ->
            flowRet0 (PropBitmask [(peValue e, fromCStringBuffer $ peName e) | e <- es])

         PropTypeBlob        -> withBuffers' nblob nblob $ \ids bids -> do
            flowTraverse getBlob bids
               >.-.> (PropBlob . (ids `zip`))
