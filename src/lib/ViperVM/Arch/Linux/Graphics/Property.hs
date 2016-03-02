{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Property
module ViperVM.Arch.Linux.Graphics.Property
   ( PropertyMeta (..)
   , PropertyType (..)
   , RawProperty (..)
   , Property (..)
   , getPropertyMeta
   , GetObjPropStruct (..)
   , SetObjPropStruct (..)
   , SetPropStruct(..)
   )
where

import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor

import Foreign.Storable
import Foreign.CStorable
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Data.Word
import Data.Int
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import GHC.Generics (Generic)

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Foreign.C.String 
   ( castCCharToChar
   , castCharToCChar
   )
import Foreign.C.Types (CChar)
import Data.Vector.Fixed.Cont (S,Z)
import Data.Vector.Fixed.Storable (Vec)
import qualified Data.Vector.Fixed as Vec

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

type N32 = -- 32 
   S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (
   S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z
   )))))))))))))))))))))))))))))))

type PropertyNameLength  = N32
type PropertyName = StorableWrap (Vec PropertyNameLength CChar)

emptyVec :: PropertyName
emptyVec = Storable (Vec.replicate (castCharToCChar '\0'))

data RawProperty = RawProperty
   { rawPropertyMetaID  :: Word32   -- ^ Card-wise property meta-info ID
   , rawPropertyValue   :: Word64   -- ^ Property value
   } deriving (Show,Eq)

data Property = Property
   { propertyMeta       :: PropertyMeta   -- ^ Meta-information about the property
   , propertyValue      :: Word64         -- ^ Value of the property
   } deriving (Show,Eq)

-- | Data matching the C structure drm_mode_obj_get_properties
data GetObjPropStruct = GetObjPropStruct
   { gopPropsPtr        :: Word64
   , gopValuesPtr       :: Word64
   , gopCountProps      :: Word32
   , gopObjId           :: Word32
   , gopObjType         :: Word32
   } deriving Generic

instance CStorable GetObjPropStruct
instance Storable GetObjPropStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- | Data matching the C structure drm_mode_obj_set_properties
data SetObjPropStruct = SetObjPropStruct
   { sopValue           :: Word64
   , sopPropId          :: Word32
   , sopObjId           :: Word32
   , sopObjType         :: Word32
   } deriving Generic

instance CStorable SetObjPropStruct
instance Storable SetObjPropStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke


-- | Type of the property
data PropertyTypeID
   = PropTypeRange
   | PropTypeEnum       -- ^ Enumerated type with text strings
   | PropTypeBlob
   | PropTypeBitmask    -- ^ Bitmask of enumerated types
   | PropTypeObject
   | PropTypeSignedRange
   deriving (Eq,Ord,Show)

getPropType :: GetPropStruct -> PropertyTypeID
getPropType s =
   -- type is interleaved with Pending and Immutable flags
   case gpsFlags s .&. 0xFA of
      2   -> PropTypeRange
      8   -> PropTypeEnum
      16  -> PropTypeBlob
      32  -> PropTypeBitmask
      64  -> PropTypeObject
      128 -> PropTypeSignedRange
      _   -> error "Unknown property type"

isPending :: GetPropStruct -> Bool
isPending s = (gpsFlags s .&. 0x01) /= 0

isImmutable :: GetPropStruct -> Bool
isImmutable s = (gpsFlags s .&. 0x04) /= 0


-- | Data matching the C structure drm_mode_property_enum
data PropEnumStruct = PropEnumStruct
   { peValue       :: Word64
   , peName        :: StorableWrap (Vec PropertyNameLength CChar)
   } deriving Generic

instance CStorable PropEnumStruct
instance Storable PropEnumStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- | Data matching the C structure drm_mode_get_property
data GetPropStruct = GetPropStruct
   { gpsValuesPtr      :: Word64
   , gpsEnumBlobPtr    :: Word64
   , gpsPropId         :: Word32
   , gpsFlags          :: Word32
   , gpsName           :: StorableWrap (Vec PropertyNameLength CChar)
   , gpsCountValues    :: Word32
   , gpsCountEnumBlobs :: Word32
   } deriving Generic

instance CStorable GetPropStruct
instance Storable GetPropStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- | Data matching the C structure drm_mode_set_property
data SetPropStruct = SetPropStruct
   { spsValue        :: Word64
   , spsPropId       :: Word32
   , spsConnId       :: Word32
   } deriving Generic

instance CStorable SetPropStruct
instance Storable SetPropStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke


-- | Data matching the C structure drm_mode_get_blob
data GetBlobStruct = GetBlobStruct
   { gbBlobId     :: Word32
   , gbLength     :: Word32
   , gbData       :: Word64
   } deriving Generic

instance CStorable GetBlobStruct
instance Storable GetBlobStruct where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

type PropertyMetaID = Word32

-- | Return meta-information from a property type ID
getPropertyMeta :: IOCTL -> FileDescriptor -> PropertyMetaID -> SysRet PropertyMeta
getPropertyMeta ioctl fd pid = runEitherT $ do
   let
      getProperty' = EitherT . ioctlReadWrite ioctl 0x64 0xAA defaultCheck fd

      gp = GetPropStruct
            { gpsValuesPtr      = 0
            , gpsEnumBlobPtr    = 0
            , gpsPropId         = pid
            , gpsFlags          = 0
            , gpsName           = emptyVec
            , gpsCountValues    = 0
            , gpsCountEnumBlobs = 0
            }
   
   -- get value size/number of elements/etc.
   g <- getProperty' gp

   let
      extractName :: PropertyName -> String
      extractName (Storable x) =
         takeWhile (/= '\0') (fmap castCCharToChar (Vec.toList x))

      nval  = gpsCountValues g
      nblob = gpsCountEnumBlobs g

      allocaArray' 0 f = f nullPtr
      allocaArray' n f = allocaArray (fromIntegral n) f

      -- corresponds to DRM_IOCTL_MODE_GETPROPBLOB
      getBlobStruct' = EitherT . ioctlReadWrite ioctl 0x64 0xAC defaultCheck fd
      getBlobStruct = runEitherT . getBlobStruct'

      withBuffers :: (Storable a, Storable b) => Word32 -> Word32 -> (Ptr a -> Ptr b -> IO c) -> IO c
      withBuffers valueCount blobCount f =
         allocaArray' valueCount $ \valuePtr ->
            allocaArray' blobCount $ \blobPtr -> do
               let gp' = GetPropStruct
                           { gpsValuesPtr      = fromIntegral (ptrToWordPtr valuePtr)
                           , gpsEnumBlobPtr    = fromIntegral (ptrToWordPtr blobPtr)
                           , gpsPropId         = pid
                           , gpsFlags          = 0
                           , gpsName           = emptyVec
                           , gpsCountValues    = valueCount
                           , gpsCountEnumBlobs = blobCount
                           }
               -- nothing changes, except for the two buffers
               _ <- runEitherT $ getProperty' gp'
               f valuePtr blobPtr

      withValueBuffer n f = withBuffers n 0 $ \ptr (_ :: Ptr ()) ->
         f =<< peekArray (fromIntegral n) ptr
      withBlobBuffer  n f = withBuffers 0 n $ \(_ :: Ptr ()) ptr ->
         f =<< peekArray (fromIntegral n) ptr
      withBuffers' n m f = withBuffers n m $ \p1 p2 -> do
         vs <- peekArray (fromIntegral n) p1
         bs <- peekArray (fromIntegral m) p2
         f vs bs
         

   ptype <- liftIO $ case getPropType g of

      PropTypeObject      -> return PropObject
      PropTypeRange       -> withValueBuffer nval (return . PropRange)
      PropTypeSignedRange -> withValueBuffer nval (return . PropSignedRange)
      PropTypeEnum        -> withBlobBuffer nblob $ \es ->
         return (PropEnum [(peValue e, extractName $ peName e) | e <- es])
      PropTypeBitmask     -> withBlobBuffer nblob $ \es ->
         return (PropBitmask [(peValue e, extractName $ peName e) | e <- es])

      PropTypeBlob        -> withBuffers' nblob nblob $ \ids bids -> do
         bids' <- forM bids $ \bid -> do
            let gb = GetBlobStruct
                        { gbBlobId = bid
                        , gbLength = 0
                        , gbData   = 0
                        }
            Right gb' <- getBlobStruct gb
            ptr <- mallocBytes (fromIntegral (gbLength gb'))
            _ <- getBlobStruct (gb' { gbData = fromIntegral (ptrToWordPtr ptr) })
            BS.unsafePackMallocCStringLen (ptr, fromIntegral (gbLength gb'))
         return (PropBlob (ids `zip` bids'))


   return $ PropertyMeta pid
      (isImmutable g)
      (isPending g)
      (extractName (gpsName g))
      ptype
   


