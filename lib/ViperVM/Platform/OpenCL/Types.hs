{-# LANGUAGE ForeignFunctionInterface,
             DeriveDataTypeable, 
             GeneralizedNewtypeDeriving #-}

-- | OpenCL basic types
module ViperVM.Platform.OpenCL.Types where

import Foreign.Storable (Storable(..))
import Data.Word (Word32,Word64)
import Data.Int (Int32)
import Foreign.Ptr (Ptr,FunPtr,IntPtr)
import Foreign.C.Types (CSize(..))
import Foreign.C.String (CString)

import ViperVM.Platform.OpenCL.ImageFormat

type Platform_ = Ptr ()
type Device_ = Ptr ()
type Context_ = Ptr ()
type CommandQueue_ = Ptr ()
type Mem_ = Ptr ()
type Program_ = Ptr ()
type Event_ = Ptr ()
type Kernel_ = Ptr ()
type Sampler_ = Ptr ()

type CLint = Int32
type CLuint = Word32
type CLulong = Word64
newtype CLbool = CLbool CLuint deriving (Eq,Storable)
type CLbitfield = CLulong

fromBool :: Bool -> CLbool
fromBool False = CLbool 0
fromBool True = CLbool 1

fromCLBool :: CLbool -> Bool
fromCLBool (CLbool x) = x /= 0

type PlatformInfo_ = CLuint
type DeviceType_ = CLbitfield
type DeviceInfo_ = CLuint
type CLDeviceFPConfig_ = CLbitfield
type CLDeviceMemCacheType_ = CLuint
type CLDeviceLocalMemType_ = CLuint
type CLDeviceExecCapability_ = CLbitfield
type CLContextInfo_ = CLuint
type CLContextProperty_ = IntPtr
type CLCommandQueueInfo_ = CLuint
type CLCommandQueueProperty_ = CLbitfield
type CLEventInfo_ = CLuint
type CLProfilingInfo_ = CLuint
type CLCommandType_ = CLuint
type CLMemFlags_ = CLbitfield
type CLMemObjectType_ = CLuint
type CLMemInfo_ = CLuint
type CLImageInfo_ = CLuint
type CLMapFlags_ = CLbitfield
type CLProgramInfo_ = CLuint
type CLProgramBuildInfo_ = CLuint
type CLBuildStatus_ = CLint
type CLKernelInfo_ = CLuint
type CLKernelWorkGroupInfo_ = CLuint
type CLFilterMode_ = CLuint
type CLSamplerInfo_ = CLuint
type CLAddressingMode_ = CLuint
type CLImageFormat_p = Ptr ImageFormat

type BuildCallback = Program_ -> Ptr () -> IO ()
type ContextCallback = CString -> Ptr () -> CSize -> Ptr () -> IO ()

foreign import ccall "wrapper" wrapContextCallback :: 
  ContextCallback -> IO (FunPtr ContextCallback)

type NativeKernelCallback = Ptr () -> IO ()

foreign import ccall "wrapper" wrapNativeKernelCallback :: 
  NativeKernelCallback -> IO (FunPtr NativeKernelCallback)
