module ViperVM.Arch.Linux.Graphics.Helper
   ( cardSetController
   , cardSwitchFrameBuffer
   )
where

import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Graphics.Connector
import ViperVM.Arch.Linux.Graphics.FrameBuffer
import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor

-- | Configure a controller
cardSetController :: Card -> Controller -> Maybe FrameBuffer -> [Connector] -> Maybe Mode -> SysRet ()
cardSetController card = withCard card setController 
   where
      setController :: IOCTL -> FileDescriptor -> Controller -> Maybe FrameBuffer -> [Connector] -> Maybe Mode -> SysRet ()
      setController ioctl fd crtc fb conns mode = do
         let fbpos = case fb of
               Nothing -> Nothing
               Just z  -> Just $ FrameBufferPos (FrameBufferID (fbID z)) 0 0
         setController' ioctl fd (controllerID crtc) fbpos (fmap connectorID conns) mode

-- | Switch to another framebuffer for the given controller
-- without doing a full mode change
cardSwitchFrameBuffer :: Card -> Controller -> FrameBuffer -> PageFlipFlags -> SysRet ()
cardSwitchFrameBuffer card = withCard card switchFrameBuffer
   where
      switchFrameBuffer :: IOCTL -> FileDescriptor -> Controller -> FrameBuffer -> PageFlipFlags -> SysRet ()
      switchFrameBuffer ioctl fd crtc fb flags =
         switchFrameBuffer' ioctl fd (controllerID crtc) (FrameBufferID (fbID fb)) flags

