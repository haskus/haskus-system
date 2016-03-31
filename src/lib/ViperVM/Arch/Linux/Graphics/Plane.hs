{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ViperVM.Arch.Linux.Graphics.Plane
   ( getPlaneResources
   , PlaneId
   )
where

import Data.Word
import Foreign.Marshal.Array
import Foreign.Ptr

import ViperVM.System.Sys
import ViperVM.Utils.Flow
import ViperVM.Arch.Linux.Internals.Graphics
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.ErrorCode

type PlaneId = Word32

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
            Right _     -> flowRet =<< sysIO (peekArray (fromIntegral n) p)


