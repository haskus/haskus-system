{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Video controller management
--
-- Controllers are called CRTC in original terminology
module ViperVM.Arch.Linux.Graphics.Controller
   ( Controller (..)
   , FrameBufferPos (..)
   , setController'
   , switchFrameBuffer'
   , cardControllers
   , getControllerGamma
   , setControllerGamma
   -- * Low level
   , cardControllerFromID
   , fromStructController
   )
where

import Foreign.Marshal.Array
import Foreign.Ptr
import Data.Word
import Control.Monad (void)

import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Internals
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Error
import ViperVM.Utils.Memory (peekArrays,allocaArrays,withArrays)

-- | Video controller
--
-- A controller is used to configure what is displayed on the screen
data Controller = Controller
   { controllerID             :: ControllerID
   , controllerMode           :: Maybe Mode
   , controllerFrameBuffer    :: Maybe FrameBufferPos -- ^ Associated frame buffer and its position (x,y)
   , controllerGammaTableSize :: Word32
   , controllerCard           :: Card
   } deriving (Show)

data FrameBufferPos = FrameBufferPos
   { frameBufferPosID :: FrameBufferID
   , frameBufferPosX  :: Word32
   , frameBufferPosY  :: Word32
   } deriving (Show)

emptyStructController :: StructController
emptyStructController = StructController 0 0 0 0 0 0 0 0 emptyStructMode

fromStructController :: Card -> StructController -> Controller
fromStructController card StructController{..} =
   Controller
      (ControllerID contID)
      (if contModeValid /= 0
         then Just (fromStructMode contModeInfo)
         else Nothing)
      (if contFbID /= 0 
         then Just (FrameBufferPos (FrameBufferID contFbID) contFbX contFbY)
         else Nothing)
      contGammaSize
      card

      
-- | Get Controller
cardControllerFromID :: Card -> ControllerID -> SysRet Controller
cardControllerFromID card crtcid = do
   let
      fd               = cardHandle card
      ControllerID cid = crtcid
      crtc             = emptyStructController { contID = cid }

   fmap (fromStructController card) <$> ioctlGetController fd crtc

setController' :: Card -> ControllerID -> Maybe FrameBufferPos -> [ConnectorID] -> Maybe Mode -> SysRet ()
setController' card crtcid fb conns mode = do
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

      void <$> ioctlSetController (cardHandle card) crtc

-- | Switch to another framebuffer for the given controller
-- without doing a full mode change
--
-- Called "mode_page_flip" in the original terminology
switchFrameBuffer' :: Card -> ControllerID -> FrameBufferID -> PageFlipFlags -> SysRet ()
switchFrameBuffer' card crtcid fb flags = do
   let
      ControllerID cid = crtcid
      FrameBufferID fid = fb
      s = StructPageFlip cid fid flags 0 0

   void <$> ioctlPageFlip (cardHandle card) s

-- | Get controllers (discard errors)
cardControllers :: Card -> IO [Controller]
cardControllers = cardEntities cardControllerIDs cardControllerFromID

-- | Get controller gama look-up table
getControllerGamma :: Controller -> Sys ([Word16],[Word16],[Word16])
getControllerGamma c = do
   let 
      card               = controllerCard c
      (ControllerID cid) = controllerID c
      sz                 = controllerGammaTableSize c
      s                  = StructControllerLut cid sz

   sysIO' $ \state ->
      allocaArrays [sz,sz,sz] $ \(as@[r,g,b] :: [Ptr Word16]) -> do
         let f = fromIntegral . ptrToWordPtr
         state2 <- sysExec state $
            sysCallAssert "Get controller gamma look-up table" $
               ioctlGetGamma (cardHandle card) (s (f r) (f g) (f b))
         [rs,gs,bs] <- peekArrays [sz,sz,sz] as
         return ((rs,gs,bs),state2)

-- | Set controller gama look-up table
setControllerGamma :: Controller -> ([Word16],[Word16],[Word16]) -> Sys ()
setControllerGamma c (rs,gs,bs) = do
   let 
      card               = controllerCard c
      (ControllerID cid) = controllerID c
      sz'                = controllerGammaTableSize c
      sz                 = fromIntegral sz'
      s                  = StructControllerLut cid sz'
      ss                 = [take sz rs,take sz gs, take sz bs]

   sysIO' $ \state ->
      withArrays ss $ \[r,g,b] -> do
         let f = fromIntegral . ptrToWordPtr
         sysRun' state $
            sysCallAssert "Set controller gamma look-up table" $
               ioctlSetGamma (cardHandle card) (s (f r) (f g) (f b))
