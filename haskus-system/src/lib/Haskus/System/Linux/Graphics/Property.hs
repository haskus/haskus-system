{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Property
module Haskus.System.Linux.Graphics.Property
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
   , showProperty
   -- * Atomic properties
   , setAtomic
   , AtomicErrors
   )
where

import Haskus.Utils.Flow
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Internals.Graphics
import Haskus.System.Linux.Error
import Haskus.System.Linux.ErrorCode
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Buffer
import Haskus.Format.Binary.Storable
import Haskus.Format.String 
import qualified Haskus.Utils.List as List

import Foreign.Ptr
import Foreign.Marshal.Alloc(free,mallocBytes)
import Data.Map as Map

-- | Property meta-information
data PropertyMeta = PropertyMeta
   { propertyID        :: Word32       -- ^ ID of the property type
   , propertyImmutable :: Bool         -- ^ The value won't change
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

-- | Display a property in a user readable way
showProperty :: Property -> String
showProperty (Property meta value) = mconcat
   [ if propertyImmutable meta then "val " else "var "
   , propertyName meta
   , " = "
   , case propertyType meta of
      PropRange [0,1]   -> if value == 0 then "False" else "True"
      PropSignedRange _ -> show (fromIntegral value :: Int64)
      PropEnum xs       -> Map.fromList xs Map.! value
      _                 -> show value
   , " :: "
   , case propertyType meta of
      PropObject         -> "Object"
      PropRange xs
         | xs == [0,1]            -> "Bool"
         | checkBounds @Word8  xs -> "Word8"
         | checkBounds @Word16 xs -> "Word16"
         | checkBounds @Word32 xs -> "Word32"
         | checkBounds @Word64 xs -> "Word64"
         | otherwise              -> "Range " ++ show xs
      PropSignedRange xs
         | checkBounds @Int8  xs -> "Int8"
         | checkBounds @Int16 xs -> "Int16"
         | checkBounds @Int32 xs -> "Int32"
         | checkBounds @Int64 xs -> "Int64"
         | otherwise             -> "Range " ++ show xs
      PropEnum xs        -> "Enum [" ++ mconcat (List.intersperse "," (fmap snd xs)) ++ "]"
      t                  -> show t
   ]

   where
      checkBounds :: forall b a. (Num a, Integral b, Bounded b, Eq a) => [a] -> Bool
      checkBounds [mi,ma] = mi == fromIntegral (minBound @b) && ma == fromIntegral (maxBound @b)
      checkBounds _ = False


data InvalidProperty = InvalidProperty deriving (Show,Eq)

type PropertyMetaID = Word32
type ObjectID       = Word32
type PropID         = Word32
type PropValue      = Word64

type AtomicErrors = '[InvalidHandle,InvalidParam,MemoryError,InvalidRange,EntryNotFound]


-- | Return meta-information from a property type ID
getPropertyMeta :: forall m. MonadInIO m => Handle -> PropertyMetaID -> Excepts '[InvalidParam,InvalidProperty] m PropertyMeta
getPropertyMeta fd pid = do
      -- get value size/number of elements/etc.
      g <- getProperty' gp
      getValues (gpsCountValues g) (gpsCountEnum g) (getPropertyTypeType g)
         ||> PropertyMeta pid (isImmutable g) (fromCStringBuffer (gpsName g))
   where
      getProperty' :: StructGetProperty -> Excepts '[InvalidParam,InvalidProperty] m StructGetProperty
      getProperty' r = ioctlGetProperty r fd
                        |> catchLiftLeft \case
                              EINVAL -> throwE InvalidParam
                              ENOENT -> throwE InvalidProperty
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

      getBlobStruct :: StructGetBlob -> Excepts '[InvalidParam,InvalidProperty] m StructGetBlob
      getBlobStruct r = ioctlGetBlob r fd
                           |> catchLiftLeft \case
                                 EINVAL -> throwE InvalidParam
                                 ENOENT -> throwE InvalidProperty
                                 e      -> unhdlErr "getBlobStruct" e

      -- | Get a blob
      getBlob :: Word32 -> Excepts '[InvalidParam,InvalidProperty] m Buffer
      getBlob bid = do
         let gb = StructGetBlob
                     { gbBlobId = bid
                     , gbLength = 0
                     , gbData   = 0
                     }

         gb' <- getBlobStruct gb
         ptr <- liftIO . mallocBytes . fromIntegral . gbLength $ gb'
         void (getBlobStruct (gb' { gbData = fromIntegral (ptrToWordPtr ptr) }))
            -- free ptr on error
            |> onE_ (liftIO (free ptr))
         -- otherwise return a bytestring
         bufferPackPtr (fromIntegral (gbLength gb')) ptr


      withBuffers :: (Storable a, Storable b) => Word32 -> Word32 -> (Ptr a -> Ptr b ->  Excepts '[InvalidParam,InvalidProperty] m c) -> Excepts '[InvalidParam,InvalidProperty] m c
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

      withValueBuffer :: Storable a => Word32 -> ([a] -> Excepts '[InvalidParam,InvalidProperty] m c) -> Excepts '[InvalidParam,InvalidProperty] m c
      withValueBuffer n f = withBuffers n 0 $ \ptr (_ :: Ptr Word) ->
         f =<< peekArray (fromIntegral n) ptr
      withBlobBuffer  n f = withBuffers 0 n $ \(_ :: Ptr Word) ptr ->
         f =<< peekArray (fromIntegral n) ptr
      withBuffers' n m f = withBuffers n m $ \p1 p2 -> do
         vs <- peekArray (fromIntegral n) p1
         bs <- peekArray (fromIntegral m) p2
         f vs bs
         
      getValues :: Word32 -> Word32 -> PropertyTypeType -> Excepts '[InvalidParam,InvalidProperty] m PropertyType
      getValues nval nblob ttype = case ttype of
         PropTypeObject      -> return PropObject
         PropTypeRange       -> withValueBuffer nval (return . PropRange)
         PropTypeSignedRange -> withValueBuffer nval (return . PropSignedRange)
         PropTypeEnum        -> withBlobBuffer nblob $ \es ->
            return (PropEnum [(peValue e, fromCStringBuffer $ peName e) | e <- es])
         PropTypeBitmask     -> withBlobBuffer nblob $ \es ->
            return (PropBitmask [(peValue e, fromCStringBuffer $ peName e) | e <- es])

         PropTypeBlob        -> withBuffers' nblob nblob $ \ids bids -> do
            traverse getBlob bids
               ||> (PropBlob . (ids `zip`))


-- | Set object properties atomically
setAtomic :: MonadInIO m => Handle -> AtomicFlags -> Map ObjectID [(PropID,PropValue)] -> Excepts AtomicErrors m ()
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
               void (ioctlAtomic s hdl)
                  |> catchLiftLeft \case
                        EBADF  -> throwE InvalidHandle
                        EINVAL -> throwE InvalidParam
                        ENOMEM -> throwE MemoryError
                        ENOENT -> throwE EntryNotFound
                        ERANGE -> throwE InvalidRange
                        ENOSPC -> throwE InvalidRange
                        e      -> unhdlErr "setAtomic" e
