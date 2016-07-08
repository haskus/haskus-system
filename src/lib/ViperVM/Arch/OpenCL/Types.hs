{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | OpenCL basic types
module ViperVM.Arch.OpenCL.Types where

import Foreign.Storable (Storable(..))
import Foreign.C.Types (CSize(..))

import ViperVM.Format.Binary.Ptr (Ptr,FunPtr,IntPtr)
import ViperVM.Format.Binary.Word
import ViperVM.Format.String (CString)
import ViperVM.Arch.OpenCL.ImageFormat

-- | OpenCL platform
type Platform_ = Ptr ()

-- | OpenCL device
type Device_ = Ptr ()

-- | OpenCL context
type Context_ = Ptr ()

-- | OpenCL command queue
type CommandQueue_ = Ptr ()

-- | OpenCL memory object (buffer, texture)
type Mem_ = Ptr ()

-- | OpenCL program
type Program_ = Ptr ()

-- | OpenCL event
type Event_ = Ptr ()

-- | OpenCL kernel
type Kernel_ = Ptr ()

-- | OpenCL sampler
type Sampler_ = Ptr ()

-- | OpenCL bool
newtype CLbool = CLbool Word32 deriving (Eq,Storable)

-- | OpenCL bitfield
type CLbitfield = Word64

-- | Create an OpenCL bool
fromBool :: Bool -> CLbool
fromBool False = CLbool 0
fromBool True = CLbool 1

-- | Convert from an OpenCL bool
fromCLBool :: CLbool -> Bool
fromCLBool (CLbool x) = x /= 0

-- | OpenCL platform info
type PlatformInfo_ = Word32

-- | OpenCL device type
type DeviceType_ = CLbitfield

-- | OpenCL device info
type DeviceInfo_ = Word32

-- | OpenCL FP config
type CLDeviceFPConfig_ = CLbitfield

-- | OpenCL memory cache type
type CLDeviceMemCacheType_ = Word32

-- | OpenCL local memory type
type CLDeviceLocalMemType_ = Word32

-- | OpenCL execution capabilities
type CLDeviceExecCapability_ = CLbitfield

-- | OpenCL context info
type CLContextInfo_ = Word32

-- | OpenCL context property
type CLContextProperty_ = IntPtr

-- | OpenCL command queue info
type CLCommandQueueInfo_ = Word32

-- | OpenCL command queue property
type CLCommandQueueProperty_ = CLbitfield

-- | OpenCL event info
type CLEventInfo_ = Word32

-- | OpenCL profiling info
type CLProfilingInfo_ = Word32

-- | OpenCL command type
type CLCommandType_ = Word32

-- | OpenCL memory flags
type CLMemFlags_ = CLbitfield

-- | OpenCL memory object type
type CLMemObjectType_ = Word32

-- | OpenCL memory info
type CLMemInfo_ = Word32

-- | OpenCL image info
type CLImageInfo_ = Word32

-- | OpenCL mapping flags
type CLMapFlags_ = CLbitfield

-- | OpenCL program info
type CLProgramInfo_ = Word32

-- | OpenCL program build info
type CLProgramBuildInfo_ = Word32

-- | OpenCL build status
type CLBuildStatus_ = Int32

-- | OpenCL kernel info
type CLKernelInfo_ = Word32

-- | OpenCL kernel work group info
type CLKernelWorkGroupInfo_ = Word32

-- | OpenCL filter mode
type CLFilterMode_ = Word32

-- | OpenCL sampler info
type CLSamplerInfo_ = Word32

-- | OpenCL addressing mode
type CLAddressingMode_ = Word32

-- | OpenCL image format pointer
type CLImageFormat_p = Ptr ImageFormat


-- | OpenCL build callback
type BuildCallback = Program_ -> Ptr () -> IO ()

-- | OpenCL context callback
type ContextCallback = CString -> Ptr () -> CSize -> Ptr () -> IO ()

-- | Wrap a context callback
foreign import ccall "wrapper" wrapContextCallback :: 
  ContextCallback -> IO (FunPtr ContextCallback)

-- | Native kernel callback
type NativeKernelCallback = Ptr () -> IO ()

-- | Wrap a native kernel
foreign import ccall "wrapper" wrapNativeKernelCallback :: 
  NativeKernelCallback -> IO (FunPtr NativeKernelCallback)
