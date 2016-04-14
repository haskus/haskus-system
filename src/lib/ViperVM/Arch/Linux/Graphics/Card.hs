{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Graphic card resources
module ViperVM.Arch.Linux.Graphics.Card
   ( Resources(..)
   , FrameBufferID(..)
   , ControllerID(..)
   , ConnectorID(..)
   , EncoderID(..)
   , getResources
   , getEntities
   , pickEncoders
   , pickControllers
   )
where

import ViperVM.System.Sys
import ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Internals.Graphics
import ViperVM.Utils.Memory (allocaArrays,peekArrays)
import ViperVM.Utils.Flow

import Data.Word
import Foreign.Ptr
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

-- | Get graphic card resources
getResources :: Handle -> Flow Sys '[Resources,InvalidHandle]
getResources hdl = getValues [10,10,10,10] -- try with default values
   where 
      getRes :: StructCardRes -> Flow Sys '[StructCardRes,InvalidHandle]
      getRes r = sysIO (ioctlGetResources r hdl) >>= \case
         Left EINVAL -> flowSet (InvalidHandle hdl)
         Left e      -> unhdlErr "getResources" e
         Right g     -> flowRet g

      extractSize x = [csCountFbs, csCountCrtcs, csCountConns, csCountEncs] <*> [x]

      getValues :: [Word32] -> Flow Sys '[Resources,InvalidHandle]
      getValues arraySizes = sysWith (allocaArrays arraySizes) $ 
         \([fs,crs,cs,es] :: [Ptr Word32]) -> do
            let 
               -- we need to check that the number of resources is still
               -- lower than the size of our arrays (as resources may have
               -- appeared between the time we get the number of resources
               -- and the time we get them...) If not, we redo the whole
               -- process
               testSize r = 
                  if all (uncurry (>)) (arraySizes `zip` extractSize r)
                     then flowLift $ extractValues r
                     else getValues (extractSize r)

               [nfb,nct,nco,nen] = arraySizes
               cv = fromIntegral . ptrToWordPtr
               res3 = StructCardRes
                           { csFbIdPtr    = cv fs
                           , csCrtcIdPtr  = cv crs
                           , csEncIdPtr   = cv es
                           , csConnIdPtr  = cv cs
                           , csCountFbs   = nfb
                           , csCountCrtcs = nct
                           , csCountConns = nco
                           , csCountEncs  = nen
                           , csMinHeight  = 0
                           , csMaxHeight  = 0
                           , csMinWidth   = 0
                           , csMaxWidth   = 0
                           }
            getRes res3 >#~> testSize


      extractValues :: StructCardRes -> Flow Sys '[Resources]
      extractValues r = do
         let 
            as  = [csFbIdPtr, csCrtcIdPtr, csConnIdPtr, csEncIdPtr] <*> [r]
            as' = fmap (wordPtrToPtr . fromIntegral) as
            arraySizes = extractSize r
         [fbs,ctrls,conns,encs] <- sysIO (peekArrays arraySizes as')
         flowRet $ Resources
               (fmap FrameBufferID fbs)
               (fmap ControllerID  ctrls)
               (fmap ConnectorID   conns)
               (fmap EncoderID     encs)
               (csMinWidth  r)
               (csMaxWidth  r)
               (csMinHeight r)
               (csMaxHeight r)


-- | Internal function to retreive card entities from their identifiers
getEntities :: (Resources -> [a]) -> (Handle -> a -> Sys (Either x b)) -> Handle -> Sys [b]
getEntities getIDs getEntityFromID hdl = do
   res <- flowRes $
      getResources hdl >#!~>
         (\(InvalidHandle _) -> error "getEntities: invalid handle")
   let 
      f (Left _)  xs = xs
      f (Right x) xs = x:xs
      ids            = getIDs res

   xs <- traverse (getEntityFromID hdl) ids
   return (foldr f [] xs)


-- | Pick the elements in es whose indexes are in bs
pickResources :: [a] -> BitSet Word32 Int -> [a]
pickResources es bs = pick es 0 (BitSet.elems bs)
   where
      pick :: [a] -> Int -> [Int] -> [a]
      pick [] _ _ = []
      pick _ _ [] = []
      pick (x:xs) n (i:is)
         | n == i    = x : pick xs (n+1) is
         | otherwise = pick xs (n+1) (i:is)

-- | Pick the controllers whose indexes are in bs
pickControllers :: Resources -> BitSet Word32 Int -> [ControllerID]
pickControllers res = pickResources (resControllerIDs res)

-- | Pick the controllers whose indexes are in bs
pickEncoders :: Resources -> BitSet Word32 Int -> [EncoderID]
pickEncoders res = pickResources (resEncoderIDs res)
