{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Property
module ViperVM.Arch.Linux.Graphics.Property
   ( PropertyMeta (..)
   , PropertyType (..)
   , getPropertyType
   , GetObjPropStruct (..)
   , SetObjPropStruct (..)
   , GetPropStruct(..)
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

-- | An object property
data PropertyMeta = PropertyMeta
   { propertyID      :: Word32         -- ^ ID of the property
   , propertyFlags   :: Word32         -- ^ TODO: extract Immutable and Pending
   , propertyName    :: String         -- ^ Property name
   , propertyType    :: PropertyType   -- ^ Type of the property
   } deriving (Show,Eq)

-- | The type of a property
data PropertyType
   = PropRange       [Word64]                   -- ^ A range
   | PropSignedRange [Int64]                    -- ^ A signed range
   | PropEnum        [(Word64,String)]          -- ^ Value-enum
   | PropBitmask     [(Word64,String)]          -- ^ Bit-enum (bitmask)
   | PropBlob        [(Word32,BS.ByteString)]   -- ^ Blob-enum
   deriving (Show,Eq)

type N32 = -- 32 
   S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (
   S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z
   )))))))))))))))))))))))))))))))

type PropertyNameLength  = N32
type PropertyName = StorableWrap (Vec PropertyNameLength CChar)

emptyVec :: PropertyName
emptyVec = Storable (Vec.replicate (castCharToCChar '\0'))


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
   = PropTypePending
   | PropTypeRange
   | PropTypeImmutable
   | PropTypeEnum       -- ^ Enumerated type with text strings
   | PropTypeBlob
   | PropTypeBitmask    -- ^ Bitmask of enumerated types
   | PropTypeObject
   | PropTypeSignedRange
   deriving (Eq,Ord,Show)

toPropType :: Word32 -> PropertyTypeID
toPropType typ =
   case typ of
      -- legacy types: 1 bit per type...
      1  -> PropTypePending
      2  -> PropTypeRange
      4  -> PropTypeImmutable
      8  -> PropTypeEnum
      16 -> PropTypeBlob
      32 -> PropTypeBitmask
      -- newer types, shifted int
      n -> case (n `shiftR` 6) of
         1 -> PropTypeObject
         2 -> PropTypeSignedRange
         _ -> error "Unknown type"

getPropertyTypeID :: GetPropStruct -> PropertyTypeID
getPropertyTypeID s = toPropType (gpsFlags s .&. 0xFA)

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
   { gpsValuesPtr    :: Word64
   , gpsEnumBlobPtr  :: Word64
   , gpsPropId       :: Word32
   , gpsFlags        :: Word32
   , gpsName         :: StorableWrap (Vec PropertyNameLength CChar)
   , gpsCountValues  :: Word32
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

type PropertyID = Word32

getPropertyType :: IOCTL -> FileDescriptor -> PropertyID -> SysRet PropertyMeta
getPropertyType ioctl fd pid = runEitherT $ do
   let
      extractName :: PropertyName -> String
      extractName (Storable x) = 
         takeWhile (/= '\0') (fmap castCCharToChar (Vec.toList x))
      getProperty' = EitherT . ioctlReadWrite ioctl 0x64 0xAA defaultCheck fd
      gp = GetPropStruct 0 0 pid 0 emptyVec 0 0

   
   -- get value size/number of elements/etc.
   g <- getProperty' gp

   let
      nval  = gpsCountValues g
      nblob = gpsCountEnumBlobs g

      allocaArray' 0 f = f nullPtr
      allocaArray' n f = allocaArray (fromIntegral n) f

      getBlobStruct' = EitherT . ioctlReadWrite ioctl 0x64 0xAC defaultCheck fd
      getBlobStruct = runEitherT . getBlobStruct'

      withBuffers :: (Storable a, Storable b) => Word32 -> Word32 -> (Ptr a -> Ptr b -> IO c) -> IO c
      withBuffers valueCount blobCount f = do
         allocaArray' valueCount $ \valuePtr -> do
            allocaArray' blobCount $ \blobPtr -> do
               let gp' = GetPropStruct 
                           (fromIntegral (ptrToWordPtr valuePtr))
                           (fromIntegral (ptrToWordPtr blobPtr))
                           pid 0 emptyVec valueCount blobCount
               -- nothing changes, except for the two buffers
               _ <- runEitherT $ getProperty' gp'
               f valuePtr blobPtr

      withValueBuffer n f = withBuffers n 0 $ \ptr (_ :: Ptr ()) -> do
         f =<< peekArray (fromIntegral n) ptr
      withBlobBuffer  n f = withBuffers 0 n $ \(_ :: Ptr ()) ptr -> do
         f =<< peekArray (fromIntegral n) ptr
      withBuffers' n m f = withBuffers n m $ \p1 p2 -> do
         vs <- peekArray (fromIntegral n) p1
         bs <- peekArray (fromIntegral m) p2
         f vs bs
         

   ptype <- liftIO $ case getPropertyTypeID g of

      PropTypeRange       -> withValueBuffer nval (return . PropRange)
      PropTypeSignedRange -> withValueBuffer nval (return . PropSignedRange)
      PropTypeEnum        -> withBlobBuffer nblob $ \es -> do
         return (PropEnum [(peValue e, extractName $ peName e) | e <- es])
      PropTypeBitmask     -> withBlobBuffer nblob $ \es -> do
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


   return $ PropertyMeta pid (gpsFlags g) (extractName (gpsName g)) ptype
   


