{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}

-- | Property
module Haskus.Arch.Linux.Graphics.Property
   ( PropertyMeta (..)
   , PropertyType (..)
   , RawProperty (..)
   , Property (..)
   , InvalidProperty (..)
   , getPropertyMeta
   , PropValue
   , ObjectID
   , PropID
   , PropertyMetaID
   -- * Atomic properties
   , setAtomic
   , AtomicErrors
   )
where

import Haskus.Utils.Flow
import Haskus.Arch.Linux.Handle
import Haskus.Arch.Linux.Internals.Graphics
import Haskus.Arch.Linux.Error
import Haskus.Arch.Linux.ErrorCode
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Ptr
import Haskus.Format.Binary.Buffer
import Haskus.Format.Binary.Storable
import Haskus.Format.String 

import Data.Map as Map

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


data InvalidProperty = InvalidProperty deriving (Show,Eq)

type PropertyMetaID = Word32
type ObjectID       = Word32
type PropID         = Word32
type PropValue      = Word64

type AtomicErrors = '[InvalidHandle,InvalidParam,MemoryError,InvalidRange,EntryNotFound]


-- | Return meta-information from a property type ID
getPropertyMeta :: forall m. MonadInIO m => Handle -> PropertyMetaID -> Flow m '[PropertyMeta,InvalidParam,InvalidProperty]
getPropertyMeta fd pid = do
      -- get value size/number of elements/etc.
      getProperty' gp >.~^> \g -> do
         getValues (gpsCountValues g) (gpsCountEnum g) (getPropertyTypeType g)
            >.-.> PropertyMeta pid
               (isImmutable g)
               (isPending g)
               (fromCStringBuffer (gpsName g))
   where
      getProperty' :: StructGetProperty -> Flow m '[StructGetProperty,InvalidParam,InvalidProperty]
      getProperty' r = liftIO (ioctlGetProperty r fd) >%~^> \case
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

      getBlobStruct :: StructGetBlob -> Flow m '[StructGetBlob,InvalidParam,InvalidProperty]
      getBlobStruct r = liftIO (ioctlGetBlob r fd) >%~^> \case
         EINVAL -> flowSet InvalidParam
         ENOENT -> flowSet InvalidProperty
         e      -> unhdlErr "getBlobStruct" e

      -- | Get a blob
      getBlob :: Word32 -> Flow m '[Buffer,InvalidParam,InvalidProperty]
      getBlob bid = do
         let gb = StructGetBlob
                     { gbBlobId = bid
                     , gbLength = 0
                     , gbData   = 0
                     }

         getBlobStruct gb >.~^> \gb' -> do
            ptr <- mallocBytes . fromIntegral . gbLength $ gb'
            getBlobStruct (gb' { gbData = fromIntegral (ptrToWordPtr ptr) })
               -- free ptr on error
               >..~=> const (free ptr)
               -- otherwise return a bytestring
               >.~.> const (bufferPackPtr (fromIntegral (gbLength gb')) ptr)


      withBuffers :: (Storable a, Storable b) => Word32 -> Word32 -> (Ptr a -> Ptr b ->  Flow m '[c,InvalidParam,InvalidProperty]) -> Flow m '[c,InvalidParam,InvalidProperty]
      withBuffers valueCount blobCount f =
         liftWith (allocaArray' valueCount) $ \valuePtr ->
            liftWith (allocaArray' blobCount) $ \blobPtr -> do
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

      withValueBuffer :: Storable a => Word32 -> ([a] -> Flow m '[c,InvalidParam,InvalidProperty]) -> Flow m '[c,InvalidParam,InvalidProperty]
      withValueBuffer n f = withBuffers n 0 $ \ptr (_ :: Ptr Word) ->
         f =<< peekArray (fromIntegral n) ptr
      withBlobBuffer  n f = withBuffers 0 n $ \(_ :: Ptr Word) ptr ->
         f =<< peekArray (fromIntegral n) ptr
      withBuffers' n m f = withBuffers n m $ \p1 p2 -> do
         vs <- peekArray (fromIntegral n) p1
         bs <- peekArray (fromIntegral m) p2
         f vs bs
         
      getValues :: Word32 -> Word32 -> PropertyTypeType -> Flow m '[PropertyType,InvalidParam,InvalidProperty]
      getValues nval nblob ttype = case ttype of
         PropTypeObject      -> flowSet PropObject
         PropTypeRange       -> withValueBuffer nval (flowSet . PropRange)
         PropTypeSignedRange -> withValueBuffer nval (flowSet . PropSignedRange)
         PropTypeEnum        -> withBlobBuffer nblob $ \es ->
            flowSet (PropEnum [(peValue e, fromCStringBuffer $ peName e) | e <- es])
         PropTypeBitmask     -> withBlobBuffer nblob $ \es ->
            flowSet (PropBitmask [(peValue e, fromCStringBuffer $ peName e) | e <- es])

         PropTypeBlob        -> withBuffers' nblob nblob $ \ids bids -> do
            flowTraverse getBlob bids
               >.-.> (PropBlob . (ids `zip`))


-- | Set object properties atomically
setAtomic :: MonadInIO m => Handle -> AtomicFlags -> Map ObjectID [(PropID,PropValue)] -> Flow m (() ': AtomicErrors)
setAtomic hdl flags objProps = do

   let
      kvs    = Map.assocs objProps -- [(Obj,[(Prop,Val)])]
      objs   = fmap fst    kvs     -- [Obj]
      pvs    = fmap snd    kvs     -- [[(Prop,Val)]]
      nprops = fmap length pvs
      props  = fmap fst (concat pvs) -- [Prop]
      vals   = fmap snd (concat pvs) -- [Val]


   withArray objs $ \pobjs ->
      withArray nprops $ \pnprops ->
         withArray props $ \pprops ->
            withArray vals $ \pvals -> do
               let
                  toPtr = fromIntegral . ptrToWordPtr
                  s = StructAtomic
                     { atomFlags         = flags
                     , atomCountObjects  = fromIntegral (length (Map.keys objProps))
                     , atomObjectsPtr    = toPtr pobjs
                     , atomCountPropsPtr = toPtr pnprops
                     , atomPropsPtr      = toPtr pprops
                     , atomPropValuesPtr = toPtr pvals
                     , atomReserved      = 0 -- must be zero
                     , atomUserData      = 0 -- used for event generation
                     }
               liftIO (ioctlAtomic s hdl)
                  >.-.> const ()
                  >..%~^> \case
                     EBADF  -> flowSet InvalidHandle
                     EINVAL -> flowSet InvalidParam
                     ENOMEM -> flowSet MemoryError
                     ENOENT -> flowSet EntryNotFound
                     ERANGE -> flowSet InvalidRange
                     ENOSPC -> flowSet InvalidRange
                     e      -> unhdlErr "setAtomic" e

