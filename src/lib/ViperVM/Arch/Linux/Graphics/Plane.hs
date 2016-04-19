{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- | Planes
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
   , InvalidDestRect (..)
   , InvalidSrcRect (..)
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
getPlaneResources :: Handle -> Flow Sys '[[PlaneID], InvalidHandle]
getPlaneResources hdl = getCount >.~#> getIDs
   where
      gpr s = sysIO (ioctlGetPlaneResources s hdl)

      -- get the number of planes (invariant for a given device)
      getCount :: Flow Sys '[Word32,InvalidHandle]
      getCount = gpr (StructGetPlaneRes 0 0)
         >.-.> gprsCountPlanes
         >..%~#> \case
            EINVAL -> flowSet (InvalidHandle hdl)
            e      -> unhdlErr "getPlaneResources" e
   
      -- get the plane IDs (invariant for a given device)
      getIDs :: Word32 -> Flow Sys '[[PlaneID],InvalidHandle]
      getIDs 0 = flowRet []
      getIDs n = sysWith (allocaArray (fromIntegral n)) $ \(p :: Ptr Word32) -> do
         let p' = fromIntegral (ptrToWordPtr p)
         gpr (StructGetPlaneRes p' n)
            >..%~#> \case
               EINVAL -> flowSet (InvalidHandle hdl)
               e      -> unhdlErr "getPlaneResources" e
            >.~.> \_ -> fmap PlaneID <$> sysIO (peekArray (fromIntegral n) p)

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
getPlane :: Handle -> PlaneID -> Flow Sys '[Plane,InvalidHandle,InvalidPlane]
getPlane hdl pid = getCount >.~#> getInfo
   where

      gpr :: StructGetPlane -> Flow Sys '[StructGetPlane,InvalidHandle,InvalidPlane]
      gpr s = sysIO (ioctlGetPlane s hdl)
         >..%~#> \case
            EINVAL -> flowSet (InvalidHandle hdl)
            ENOENT -> flowSet (InvalidPlane (PlaneID (gpPlaneId s)))
            e      -> unhdlErr "getPlane" e

      PlaneID pid' = pid

      toMaybe _ 0 = Nothing
      toMaybe f x = Just (f x)

      -- get the number of formats (invariant for a given plane)
      getCount :: Flow Sys '[Word32,InvalidHandle,InvalidPlane]
      getCount = gpr (StructGetPlane pid' 0 0 BitSet.empty 0 0 0)
         >.-.> gpCountFmtTypes 

      -- get the plane info (invariant for a given plane)
      getInfo :: Word32 -> Flow Sys '[Plane,InvalidHandle,InvalidPlane]
      getInfo n = sysWith (allocaArray (fromIntegral n)) $ \(p :: Ptr Word32) -> do
         let 
            p' = fromIntegral (ptrToWordPtr p)
            si = StructGetPlane pid' 0 0 BitSet.empty 0 n p'
         gpr si
            >.~#> \StructGetPlane{..} -> getResources hdl >.~#> \res -> do
               -- TODO: controllers are invariant, we should store them
               -- somewhere to avoid getResources
               fmts <- fmap (PixelFormat . BitFields) <$> sysIO (peekArray (fromIntegral n) p)
               flowRet Plane
                  { planeId                  = pid
                  , planeControllerId        = toMaybe ControllerID gpCrtcId
                  , planeFrameBufferId       = toMaybe FrameBufferID gpFbId
                  , planePossibleControllers = pickControllers res gpPossibleCrtcs
                  , planeGammaSize           = gpGammaSize
                  , planeFormats             = fmts
                  }

type FP16 = FixedPoint Word32 16 16

-- | Destination rectangle
data DestRect = DestRect
   { destX      :: Int32
   , destY      :: Int32
   , destWidth  :: Word32
   , destHeight :: Word32
   }
   deriving (Show,Eq)

-- | Source rectangle
data SrcRect = SrcRect
   { srcX      :: FP16
   , srcY      :: FP16
   , srcWidth  :: FP16
   , srcHeight :: FP16
   }
   deriving (Show,Eq)

-- | Invalid destination rectangle
data InvalidDestRect = InvalidDestRect deriving (Show,Eq)

-- | Invalid source rectangle
data InvalidSrcRect  = InvalidSrcRect deriving (Show,Eq)

-- | Set plane
--
-- If the source/destination rectangles are not the same, scaling support is
-- required. Devices not supporting scaling will fail with InvalidParam.
--
-- The fractional part in SrcRect is for devices supporting sub-pixel plane
-- coordinates.
setPlane :: Handle -> PlaneID -> Maybe (ControllerID, FrameBufferID, SrcRect, DestRect) -> Flow Sys '[(),InvalidParam,EntryNotFound,InvalidDestRect,InvalidSrcRect]
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

   sysIO (ioctlSetPlane s hdl)
      >.-.>   const ()
      >..%~#> \case
         EINVAL -> flowSet InvalidParam
         ENOENT -> flowSet EntryNotFound
         ERANGE -> flowSet InvalidDestRect
         ENOSPC -> flowSet InvalidSrcRect
         e      -> unhdlErr "setPlane" e

-- | Disable a plane
disablePlane :: Handle -> PlaneID -> Flow Sys '[(),InvalidParam,EntryNotFound]
disablePlane hdl p = setPlane hdl p Nothing
   -- these errors should not be triggered when we disable a plane
   >..%~!!> (\InvalidDestRect -> unhdlErr "disablePlane" InvalidDestRect)
   >..%~!!> (\InvalidSrcRect  -> unhdlErr "disablePlane" InvalidSrcRect)
