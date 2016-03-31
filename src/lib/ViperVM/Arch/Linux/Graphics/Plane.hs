{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module ViperVM.Arch.Linux.Graphics.Plane
   ( getPlaneResources
   , PlaneId
   , InvalidPlane (..)
   , PlaneInfo (..)
   , getPlaneInfo
   )
where

import Data.Word
import Foreign.Marshal.Array
import Foreign.Ptr

import ViperVM.System.Sys
import ViperVM.Utils.Flow
import ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Format.Binary.BitField
import ViperVM.Arch.Linux.Internals.Graphics
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.PixelFormat

data InvalidPlane = InvalidPlane PlaneId deriving (Show)

newtype PlaneId = PlaneId Word32 deriving (Show,Eq)

getPlaneResources :: Handle -> Sys (Flow '[InvalidHandle] [PlaneId])
getPlaneResources hdl = runFlowT $ do
      cnt <- liftFlowT getCount
      liftFlowT (getIDs cnt)
   where
      gpr s = sysIO (ioctlGetPlaneResources s hdl)

      -- get the number of planes (invariant for a given device)
      getCount :: Sys (Flow '[InvalidHandle] Word32)
      getCount = gpr (StructGetPlaneRes 0 0) >>= \case
         Left EINVAL -> flowSet (InvalidHandle hdl)
         Left e      -> unhdlErr "getPlaneResources" e
         Right s     -> flowRet (gprsCountPlanes s)
   
      -- get the plane IDs (invariant for a given device)
      getIDs :: Word32 -> Sys (Flow '[InvalidHandle] [PlaneId])
      getIDs 0 = flowRet []
      getIDs n = sysWith (allocaArray (fromIntegral n)) $ \(p :: Ptr Word32) -> do
         let p' = fromIntegral (ptrToWordPtr p)
         gpr (StructGetPlaneRes p' n) >>= \case
            Left EINVAL -> flowSet (InvalidHandle hdl)
            Left e      -> unhdlErr "getPlaneResources" e
            Right _     -> do
               ps <- fmap PlaneId <$> sysIO (peekArray (fromIntegral n) p)
               flowRet ps

data PlaneInfo = PlaneInfo
   { planeId                  :: PlaneId
   , planeControllerId        :: Maybe ControllerID
   , planeFrameBufferId       :: Maybe FrameBufferID
   , planePossibleControllers :: [ControllerID]
   , planeGammaSize           :: Word32
   , planeFormats             :: [PixelFormat]
   }
   deriving (Show)

getPlaneInfo :: Handle -> PlaneId -> Sys (Flow '[InvalidHandle,InvalidPlane] PlaneInfo)
getPlaneInfo hdl pid = runFlowT $ do
      cnt <- liftFlowT getCount
      liftFlowT (getInfo cnt)
   where

      gpr s = sysIO (ioctlGetPlane s hdl)

      PlaneId pid' = pid

      toMaybe _ 0 = Nothing
      toMaybe f x = Just (f x)

      -- get the number of formats (invariant for a given plane)
      getCount :: Sys (Flow '[InvalidHandle,InvalidPlane] Word32)
      getCount = gpr (StructGetPlane pid' 0 0 BitSet.empty 0 0 0) >>= \case
         Left EINVAL -> flowSet (InvalidHandle hdl)
         Left ENOENT -> flowSet (InvalidPlane pid)
         Left e      -> unhdlErr "getPlaneInfo" e
         Right s     -> flowRet (gpCountFmtTypes s)

      -- get the plane info (invariant for a given plane)
      getInfo :: Word32 -> Sys (Flow '[InvalidHandle,InvalidPlane] PlaneInfo)
      getInfo n = sysWith (allocaArray (fromIntegral n)) $ \(p :: Ptr Word32) -> do
         let 
            p' = fromIntegral (ptrToWordPtr p)
            si = StructGetPlane pid' 0 0 BitSet.empty 0 n p'
         gpr si >>= \case
            Left EINVAL -> flowSet (InvalidHandle hdl)
            Left ENOENT -> flowSet (InvalidPlane pid)
            Left e      -> unhdlErr "getPlaneInfo" e
            Right StructGetPlane{..} -> do
               fmts <- fmap (PixelFormat . BitFields)
                  <$> sysIO (peekArray (fromIntegral n) p)
               flowRet PlaneInfo
                  { planeId                  = pid
                  , planeControllerId        = toMaybe ControllerID gpCrtcId
                  , planeFrameBufferId       = toMaybe FrameBufferID gpFbId
                  , planePossibleControllers = [] -- FIXME!!!!! need getResources
                  , planeGammaSize           = gpGammaSize
                  , planeFormats             = fmts
                  }
