{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module ViperVM.Arch.Linux.Graphics.Plane
   ( getPlaneResources
   , PlaneID
   , InvalidPlane (..)
   , Plane (..)
   , getPlane
   , setPlane
   , disablePlane
   , DestRect (..)
   , SrcRect (..)
   )
where

import Data.Word
import Data.Int
import Foreign.Marshal.Array
import Foreign.Ptr

import ViperVM.System.Sys
import ViperVM.Utils.Flow
import ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Format.Binary.BitField
import ViperVM.Format.Binary.FixedPoint
import ViperVM.Arch.Linux.Internals.Graphics
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.PixelFormat

-- | Error invalid plane ID
data InvalidPlane = InvalidPlane PlaneID deriving (Show)

-- | Plane identifier
newtype PlaneID = PlaneID Word32 deriving (Show,Eq)

-- | Get the IDs of the supported planes
getPlaneResources :: Handle -> Sys (Flow '[InvalidHandle] [PlaneID])
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
      getIDs :: Word32 -> Sys (Flow '[InvalidHandle] [PlaneID])
      getIDs 0 = flowRet []
      getIDs n = sysWith (allocaArray (fromIntegral n)) $ \(p :: Ptr Word32) -> do
         let p' = fromIntegral (ptrToWordPtr p)
         gpr (StructGetPlaneRes p' n) >>= \case
            Left EINVAL -> flowSet (InvalidHandle hdl)
            Left e      -> unhdlErr "getPlaneResources" e
            Right _     -> do
               ps <- fmap PlaneID <$> sysIO (peekArray (fromIntegral n) p)
               flowRet ps

-- | A plane
data Plane = Plane
   { planeId                  :: PlaneID              -- ^ Plane identifier
   , planeControllerId        :: Maybe ControllerID   -- ^ Connected controller
   , planeFrameBufferId       :: Maybe FrameBufferID  -- ^ Connected framebuffer
   , planePossibleControllers :: [ControllerID]       -- ^ Potential controllers
   , planeGammaSize           :: Word32               -- ^ Size of the gamma table
   , planeFormats             :: [PixelFormat]        -- ^ Supported pixel formats
   }
   deriving (Show)

-- | Get plane information
getPlane :: Handle -> PlaneID -> Sys (Flow '[InvalidHandle,InvalidPlane] Plane)
getPlane hdl pid = runFlowT $ do
      cnt <- liftFlowT getCount
      liftFlowT (getInfo cnt)
   where

      gpr s = sysIO (ioctlGetPlane s hdl)

      PlaneID pid' = pid

      toMaybe _ 0 = Nothing
      toMaybe f x = Just (f x)

      -- get the number of formats (invariant for a given plane)
      getCount :: Sys (Flow '[InvalidHandle,InvalidPlane] Word32)
      getCount = gpr (StructGetPlane pid' 0 0 BitSet.empty 0 0 0) >>= \case
         Left EINVAL -> flowSet (InvalidHandle hdl)
         Left ENOENT -> flowSet (InvalidPlane pid)
         Left e      -> unhdlErr "getPlane" e
         Right s     -> flowRet (gpCountFmtTypes s)

      -- get the plane info (invariant for a given plane)
      getInfo :: Word32 -> Sys (Flow '[InvalidHandle,InvalidPlane] Plane)
      getInfo n = sysWith (allocaArray (fromIntegral n)) $ \(p :: Ptr Word32) -> do
         let 
            p' = fromIntegral (ptrToWordPtr p)
            si = StructGetPlane pid' 0 0 BitSet.empty 0 n p'
         gpr si >>= \case
            Left EINVAL -> flowSet (InvalidHandle hdl)
            Left ENOENT -> flowSet (InvalidPlane pid)
            Left e      -> unhdlErr "getPlane" e
            Right StructGetPlane{..} -> runFlowT $ do
               -- TODO: controllers are invariant, we should store them
               -- somewhere to avoid this ioctl
               res <- liftFlowT $ getResources hdl
               fmts <- liftFlowM (fmap (PixelFormat . BitFields) <$> sysIO (peekArray (fromIntegral n) p))
               liftFlowM $ return Plane
                  { planeId                  = pid
                  , planeControllerId        = toMaybe ControllerID gpCrtcId
                  , planeFrameBufferId       = toMaybe FrameBufferID gpFbId
                  , planePossibleControllers = pickControllers res gpPossibleCrtcs
                  , planeGammaSize           = gpGammaSize
                  , planeFormats             = fmts
                  }

type FP16 = FixedPoint Word32 16 16

data DestRect = DestRect
   { destX      :: Int32
   , destY      :: Int32
   , destWidth  :: Word32
   , destHeight :: Word32
   }
   deriving (Show,Eq)

data SrcRect = SrcRect
   { srcX      :: FP16
   , srcY      :: FP16
   , srcWidth  :: FP16
   , srcHeight :: FP16
   }
   deriving (Show,Eq)


data InvalidDestRect = InvalidDestRect deriving (Show,Eq)
data InvalidSrcRect  = InvalidSrcRect deriving (Show,Eq)

-- | Set plane
--
-- If the source/destination rectangles are not the same, scaling support is
-- required. Devices not supporting scaling will fail with InvalidParam.
--
-- The fractional part in SrcRect is for devices supporting sub-pixel plane
-- coordinates.
setPlane :: Handle -> PlaneID -> Maybe (ControllerID, FrameBufferID, SrcRect, DestRect) -> Sys (Flow '[InvalidParam,EntryNotFound,InvalidDestRect,InvalidSrcRect] ())
setPlane hdl (PlaneID pid) opts = do

   let 
      makeS (ControllerID cid) (FrameBufferID fbid) =
         StructSetPlane pid cid fbid BitSet.empty

      e16 = toFixedPoint (0 :: Float)

      s = case opts of
            Nothing -> -- disable the plane
               makeS (ControllerID 0) (FrameBufferID 0)
                  0 0 0 0 e16 e16 e16 e16

            Just (cid,fbid,SrcRect{..},DestRect{..}) ->
               makeS cid fbid
                  destX destY destWidth destHeight
                  srcX srcY srcHeight srcWidth

   r <- sysIO (ioctlSetPlane s hdl)

   case r of
      Right _     -> flowRet ()
      Left EINVAL -> flowSet InvalidParam
      Left ENOENT -> flowSet EntryNotFound
      Left ERANGE -> flowSet InvalidDestRect
      Left ENOSPC -> flowSet InvalidSrcRect
      Left e      -> unhdlErr "setPlane" e

-- | Disable a plane
disablePlane :: Handle -> PlaneID -> Sys (Flow '[InvalidParam,EntryNotFound] ())
disablePlane hdl p = setPlane hdl p Nothing
   -- these errors should not be triggered when we disable a plane
   `flowMCatch` (\InvalidDestRect -> unhdlErr "disablePlane" InvalidDestRect)
   `flowMCatch` (\InvalidSrcRect  -> unhdlErr "disablePlane" InvalidSrcRect)
