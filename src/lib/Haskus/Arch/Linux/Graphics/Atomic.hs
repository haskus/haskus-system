{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- | The atomic interface is the new API to use DRM/KMS
module Haskus.Arch.Linux.Graphics.Atomic
   ( setAtomic
   )
where

import Haskus.Arch.Linux.Internals.Graphics
import Haskus.Arch.Linux.Handle
import Haskus.Arch.Linux.Error
import Haskus.Arch.Linux.ErrorCode
import Haskus.Utils.Flow
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Ptr
import Haskus.Format.Binary.Storable

import Data.Map as Map

-- DRM now has a single entry-point for changing the configuration: the atomic
-- ioctl. We can test and commit a whole configuration without going through
-- intermediate states. Legacy object properties are accessible through object
-- properties. An atomic modification is a list of (object, property, value)
-- tuples.

type ObjectID  = Word32
type PropID    = Word32
type PropValue = Word64

-- | Set object properties atomically
setAtomic :: MonadInIO m => Handle -> AtomicFlags -> Map ObjectID [(PropID,PropValue)] -> Flow m '[(),InvalidHandle,InvalidParam,MemoryError,InvalidRange,EntryNotFound]
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
