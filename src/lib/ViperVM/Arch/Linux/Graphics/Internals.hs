module ViperVM.Arch.Linux.Graphics.Internals
   ( ioctlGetCapabilities
   , ioctlModeGetResources
   , ioctlModeGetController
   , ioctlModeSetController
   , ioctlModeGetEncoder
   , ioctlModeGetConnector
   , ioctlModeGetProperty
   , ioctlModeSetProperty
   , ioctlModeGetPropertyBlob
   , ioctlModePageFlip
   , ioctlModeCreateGenericBuffer
   , ioctlModeMapGenericBuffer
   , ioctlModeDestroyGenericBuffer
   , ioctlModeAddFrameBuffer
   , ioctlModeRemoveFrameBuffer
   )
where

import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileSystem
import ViperVM.Arch.Linux.FileDescriptor

import Foreign.Storable (Storable)
import Data.Word (Word8)

----------------------------------
-- IOCTLs
----------------------------------

-- | IOCTL for DRM is restarted on interruption
drmIoctl :: Storable a => Word8 -> FileDescriptor -> a -> SysRet a
drmIoctl n = ioctlReadWrite (repeatIoctl sysIoctl) 0x64 n defaultCheck


ioctlGetCapabilities :: Storable a => FileDescriptor -> a -> SysRet a
ioctlGetCapabilities = drmIoctl 0x0C
 
ioctlModeGetResources :: Storable a => FileDescriptor -> a -> SysRet a
ioctlModeGetResources = drmIoctl 0xA0

ioctlModeGetController :: Storable a => FileDescriptor -> a -> SysRet a
ioctlModeGetController = drmIoctl 0xA1

ioctlModeSetController :: Storable a => FileDescriptor -> a -> SysRet a
ioctlModeSetController = drmIoctl 0xA2

ioctlModeGetEncoder :: Storable a => FileDescriptor -> a -> SysRet a
ioctlModeGetEncoder = drmIoctl 0xA6

ioctlModeGetConnector :: Storable a => FileDescriptor -> a -> SysRet a
ioctlModeGetConnector = drmIoctl 0xA7

ioctlModeGetProperty :: Storable a => FileDescriptor -> a -> SysRet a
ioctlModeGetProperty = drmIoctl 0xAA

ioctlModeSetProperty :: Storable a => FileDescriptor -> a -> SysRet a
ioctlModeSetProperty = drmIoctl 0xAB

ioctlModeGetPropertyBlob :: Storable a => FileDescriptor -> a -> SysRet a
ioctlModeGetPropertyBlob = drmIoctl 0xAC

ioctlModeRemoveFrameBuffer :: Storable a => FileDescriptor -> a -> SysRet a
ioctlModeRemoveFrameBuffer = drmIoctl 0xAF

ioctlModePageFlip :: Storable a => FileDescriptor -> a -> SysRet a
ioctlModePageFlip = drmIoctl 0xB0

ioctlModeCreateGenericBuffer :: Storable a => FileDescriptor -> a -> SysRet a
ioctlModeCreateGenericBuffer = drmIoctl 0xB2

ioctlModeMapGenericBuffer :: Storable a => FileDescriptor -> a -> SysRet a
ioctlModeMapGenericBuffer = drmIoctl 0xB3

ioctlModeDestroyGenericBuffer :: Storable a => FileDescriptor -> a -> SysRet a
ioctlModeDestroyGenericBuffer = drmIoctl 0xB4

ioctlModeAddFrameBuffer :: Storable a => FileDescriptor -> a -> SysRet a
ioctlModeAddFrameBuffer = drmIoctl 0xB8
