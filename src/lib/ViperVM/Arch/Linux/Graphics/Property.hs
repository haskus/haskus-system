{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

-- | Property
module ViperVM.Arch.Linux.Graphics.Property
   ( PropertyMeta (..)
   , PropertyType (..)
   , RawProperty (..)
   , Property (..)
   , getPropertyMeta
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Internals.Graphics

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Data.Word
import Data.Int
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Foreign.C.String 
   ( castCCharToChar
   , castCharToCChar
   )
import Foreign.C.Types (CChar)
import ViperVM.Format.Binary.Vector (Vector)
import qualified ViperVM.Format.Binary.Vector as Vec

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

type PropertyName = Vector 32 CChar

data RawProperty = RawProperty
   { rawPropertyMetaID  :: Word32   -- ^ Card-wise property meta-info ID
   , rawPropertyValue   :: Word64   -- ^ Property value
   } deriving (Show,Eq)

data Property = Property
   { propertyMeta       :: PropertyMeta -- ^ Meta-information about the property
   , propertyValue      :: Word64       -- ^ Value of the property
   } deriving (Show,Eq)


type PropertyMetaID = Word32

-- | Return meta-information from a property type ID
getPropertyMeta :: FileDescriptor -> PropertyMetaID -> SysRet PropertyMeta
getPropertyMeta fd pid = runEitherT $ do
   let
      getProperty' r = EitherT (ioctlGetProperty r fd)

      gp = StructGetProperty
            { gpsValuesPtr   = 0
            , gpsEnumBlobPtr = 0
            , gpsPropId      = pid
            , gpsFlags       = 0
            , gpsName        = Vec.replicate (castCharToCChar '\0')
            , gpsCountValues = 0
            , gpsCountEnum   = 0
            }
   
   -- get value size/number of elements/etc.
   g <- getProperty' gp

   let
      extractName :: PropertyName -> String
      extractName = takeWhile (/= '\0') . fmap castCCharToChar . Vec.toList

      nval  = gpsCountValues g
      nblob = gpsCountEnum   g

      allocaArray' 0 f = f nullPtr
      allocaArray' n f = allocaArray (fromIntegral n) f

      getBlobStruct' r = EitherT (ioctlGetBlob r fd)
      getBlobStruct    = runEitherT . getBlobStruct'

      withBuffers :: (Storable a, Storable b) => Word32 -> Word32 -> (Ptr a -> Ptr b -> IO c) -> IO c
      withBuffers valueCount blobCount f =
         allocaArray' valueCount $ \valuePtr ->
            allocaArray' blobCount $ \blobPtr -> do
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
         

   ptype <- liftIO $ case getPropertyTypeType g of

      PropTypeObject      -> return PropObject
      PropTypeRange       -> withValueBuffer nval (return . PropRange)
      PropTypeSignedRange -> withValueBuffer nval (return . PropSignedRange)
      PropTypeEnum        -> withBlobBuffer nblob $ \es ->
         return (PropEnum [(peValue e, extractName $ peName e) | e <- es])
      PropTypeBitmask     -> withBlobBuffer nblob $ \es ->
         return (PropBitmask [(peValue e, extractName $ peName e) | e <- es])

      PropTypeBlob        -> withBuffers' nblob nblob $ \ids bids -> do
         bids' <- forM bids $ \bid -> do
            let gb = StructGetBlob
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
   


