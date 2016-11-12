{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

-- | Video controller management
--
-- Controllers are called CRTC in original terminology
module ViperVM.Arch.Linux.Graphics.Controller
   ( Controller (..)
   , FrameBufferPos (..)
   , setController'
   , switchFrameBuffer'
   , getControllers
   , getControllerGamma
   , setControllerGamma
   -- * Low level
   , getControllerFromID
   , fromStructController
   )
where

import ViperVM.System.Sys
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Internals.Graphics
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Utils.Memory (peekArrays,allocaArrays,withArrays)
import ViperVM.Utils.Flow
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.Storable

-- | Video controller
--
-- A controller is used to configure what is displayed on the screen
data Controller = Controller
   { controllerID             :: ControllerID
   , controllerMode           :: Maybe Mode
   , controllerFrameBuffer    :: Maybe FrameBufferPos -- ^ Associated frame buffer and its position (x,y)
   , controllerGammaTableSize :: Word32
   , controllerHandle         :: Handle
   } deriving (Show)

data FrameBufferPos = FrameBufferPos
   { frameBufferPosID :: FrameBufferID
   , frameBufferPosX  :: Word32
   , frameBufferPosY  :: Word32
   } deriving (Show)

emptyStructController :: StructController
emptyStructController = StructController 0 0 0 0 0 0 0 0 emptyStructMode

fromStructController :: Handle -> StructController -> Controller
fromStructController hdl StructController{..} =
   Controller
      (ControllerID contID)
      (if contModeValid /= 0
         then Just (fromStructMode contModeInfo)
         else Nothing)
      (if contFbID /= 0 
         then Just (FrameBufferPos (FrameBufferID contFbID) contFbX contFbY)
         else Nothing)
      contGammaSize
      hdl

      
-- | Get Controller
getControllerFromID :: Handle -> ControllerID -> Flow Sys '[Controller,EntryNotFound, InvalidHandle]
getControllerFromID hdl crtcid = liftIO (ioctlGetController crtc hdl)
      >.-.> fromStructController hdl
      >%~^> \case
         EINVAL -> flowSet InvalidHandle
         ENOENT -> flowSet EntryNotFound
         e      -> unhdlErr "getController" e
   where
      ControllerID cid = crtcid
      crtc             = emptyStructController { contID = cid }


setController' :: MonadInIO m => Handle -> ControllerID -> Maybe FrameBufferPos -> [ConnectorID] -> Maybe Mode -> Flow m '[(),ErrorCode]
setController' hdl crtcid fb conns mode = do
   let
      ControllerID cid = crtcid
      conns' = fmap (\(ConnectorID i) -> i) conns

      (fbid,fbx,fby) = case fb of
         Nothing -> (0,0,0)
         Just (FrameBufferPos (FrameBufferID z) x y) -> (z,x,y)

   withArray conns' $ \conArray -> do
      let
         crtc = StructController
            { contID = cid
            , contFbID = fbid
            , contFbX  = fbx
            , contFbY  = fby
            , contModeInfo = case mode of
               Nothing -> emptyStructMode
               Just x  -> toStructMode x
            , contModeValid  = case mode of
               Nothing -> 0
               Just _  -> 1
            , contConnCount  = fromIntegral (length conns)
            , contSetConnPtr = fromIntegral (ptrToWordPtr conArray)
            , contGammaSize = 0
            }

      liftIO (ioctlSetController crtc hdl) >.-.> const ()

-- | Switch to another framebuffer for the given controller
-- without doing a full mode change
--
-- Called "mode_page_flip" in the original terminology
switchFrameBuffer' :: MonadIO m => Handle -> ControllerID -> FrameBufferID -> PageFlipFlags -> Flow m '[(),ErrorCode]
switchFrameBuffer' hdl crtcid fb flags = do
   let
      ControllerID cid = crtcid
      FrameBufferID fid = fb
      s = StructPageFlip cid fid flags 0 0

   liftIO (ioctlPageFlip s hdl) >.-.> const ()

-- | Get controllers
getControllers :: Handle -> Flow Sys '[[Controller],EntryNotFound,InvalidHandle]
getControllers hdl = getResources hdl
   >.-.> resControllerIDs
   >.~^> flowTraverse (getControllerFromID hdl)

-- | Get controller gama look-up table
getControllerGamma :: Controller -> Sys ([Word16],[Word16],[Word16])
getControllerGamma c = do
   let 
      hdl                = controllerHandle c
      (ControllerID cid) = controllerID c
      sz                 = controllerGammaTableSize c
      s                  = StructControllerLut cid sz

   sysIO' $ \state ->
      allocaArrays [sz,sz,sz] $ \(as@[r,g,b] :: [Ptr Word16]) -> do
         let f = fromIntegral . ptrToWordPtr
         state2 <- sysExec state
            <| flowAssertQuiet "Get controller gamma look-up table"
            <| liftIO (ioctlGetGamma (s (f r) (f g) (f b)) hdl)
         [rs,gs,bs] <- peekArrays [sz,sz,sz] as
         return ((rs,gs,bs),state2)

-- | Set controller gama look-up table
setControllerGamma :: Controller -> ([Word16],[Word16],[Word16]) -> Sys ()
setControllerGamma c (rs,gs,bs) = do
   let 
      hdl                = controllerHandle c
      (ControllerID cid) = controllerID c
      sz'                = controllerGammaTableSize c
      sz                 = fromIntegral sz'
      s                  = StructControllerLut cid sz'
      ss                 = [take sz rs,take sz gs, take sz bs]

   sysIO' $ \state ->
      withArrays ss $ \[r,g,b] -> do
         let f = fromIntegral . ptrToWordPtr
         sysRun' state
            <| flowAssertQuiet "Set controller gamma look-up table"
            <| liftIO (ioctlSetGamma (s (f r) (f g) (f b)) hdl)
