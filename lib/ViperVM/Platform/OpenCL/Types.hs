{-# LANGUAGE ForeignFunctionInterface,
             DeriveDataTypeable, 
             GeneralizedNewtypeDeriving #-}
module ViperVM.Platform.OpenCL.Types where

import Foreign.Storable (Storable(..))
import Data.Word (Word32,Word64)
import Data.Int (Int32)
import Foreign.Ptr (Ptr,FunPtr,IntPtr)
import Foreign.C.Types (CSize(..))
import Foreign.C.String (CString)

import ViperVM.Platform.OpenCL.ImageFormat
import ViperVM.Platform.OpenCL.Bindings

newtype Platform = Platform (Ptr ()) deriving (Eq,Storable)
newtype Context = Context (Ptr ()) deriving (Eq,Storable)
newtype Device = Device (Ptr ()) deriving (Eq,Storable)
newtype CommandQueue = CommandQueue (Ptr ()) deriving (Eq,Storable)
newtype Mem = Mem (Ptr ()) deriving (Eq,Storable)
newtype Event = Event (Ptr ()) deriving (Eq,Storable)
newtype Program = Program (Ptr ()) deriving (Eq,Storable)
newtype Kernel = Kernel (Ptr ()) deriving (Eq,Storable)
newtype Sampler = Sampler (Ptr ()) deriving (Eq,Storable)

class Entity e where unwrap :: e -> Ptr ()
instance Entity Platform where unwrap (Platform x) = x
instance Entity Device where unwrap (Device x) = x
instance Entity Context where unwrap (Context x) = x
instance Entity CommandQueue where unwrap (CommandQueue x) = x
instance Entity Mem where unwrap (Mem x) = x
instance Entity Event where unwrap (Event x) = x
instance Entity Program where unwrap (Program x) = x
instance Entity Kernel where unwrap (Kernel x) = x
instance Entity Sampler where unwrap (Sampler x) = x


type CLint = Int32
type CLuint = Word32
type CLulong = Word64
newtype CLbool = CLbool CLuint deriving (Eq,Storable)
type CLbitfield = CLulong

fromBool :: Bool -> CLbool
fromBool False = CLbool 0
fromBool True = CLbool 1

fromCLBool :: CLbool -> Bool
fromCLBool (CLbool x) = not (x == 0)


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
type CLImageFormat_p = Ptr CLImageFormat

type ContextCallback = CString -> Ptr () -> CSize -> Ptr () -> IO ()

foreign import ccall "wrapper" wrapContextCallback :: 
  ContextCallback -> IO (FunPtr ContextCallback)

type NativeKernelCallback = Ptr () -> IO ()

foreign import ccall "wrapper" wrapNativeKernelCallback :: 
  NativeKernelCallback -> IO (FunPtr NativeKernelCallback)


type BuildCallback = Program -> Ptr () -> IO ()

data CLContextInfo = 
     CL_CONTEXT_REFERENCE_COUNT
   | CL_CONTEXT_DEVICES
   | CL_CONTEXT_PROPERTIES
   | CL_CONTEXT_NUM_DEVICES
   | CL_CONTEXT_PLATFORM
   deriving (Eq,Enum)

instance CLConstant CLContextInfo where
   toCL x = fromIntegral (0x1080 + fromEnum x)
   fromCL x = toEnum (fromIntegral x - 0x1080)

data PlatformInfoTag =
     CL_PLATFORM_PROFILE
   | CL_PLATFORM_VERSION
   | CL_PLATFORM_NAME
   | CL_PLATFORM_VENDOR
   | CL_PLATFORM_EXTENSIONS
   deriving (Show,Enum)

instance CLConstant PlatformInfoTag where
   toCL x = fromIntegral (0x0900 + fromEnum x)
   fromCL x = toEnum (fromIntegral x - 0x0900)

data DeviceInfoTag =
     CL_DEVICE_TYPE
   | CL_DEVICE_VENDOR_ID
   | CL_DEVICE_MAX_COMPUTE_UNITS
   | CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS
   | CL_DEVICE_MAX_WORK_GROUP_SIZE
   | CL_DEVICE_MAX_WORK_ITEM_SIZES
   | CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR
   | CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT
   | CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT
   | CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG
   | CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT
   | CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE
   | CL_DEVICE_MAX_CLOCK_FREQUENCY
   | CL_DEVICE_ADDRESS_BITS
   | CL_DEVICE_MAX_READ_IMAGE_ARGS
   | CL_DEVICE_MAX_WRITE_IMAGE_ARGS
   | CL_DEVICE_MAX_MEM_ALLOC_SIZE
   | CL_DEVICE_IMAGE2D_MAX_WIDTH
   | CL_DEVICE_IMAGE2D_MAX_HEIGHT
   | CL_DEVICE_IMAGE3D_MAX_WIDTH
   | CL_DEVICE_IMAGE3D_MAX_HEIGHT
   | CL_DEVICE_IMAGE3D_MAX_DEPTH
   | CL_DEVICE_IMAGE_SUPPORT
   | CL_DEVICE_MAX_PARAMETER_SIZE
   | CL_DEVICE_MAX_SAMPLERS
   | CL_DEVICE_MEM_BASE_ADDR_ALIGN
   | CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE
   | CL_DEVICE_SINGLE_FP_CONFIG
   | CL_DEVICE_GLOBAL_MEM_CACHE_TYPE
   | CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE
   | CL_DEVICE_GLOBAL_MEM_CACHE_SIZE
   | CL_DEVICE_GLOBAL_MEM_SIZE
   | CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE
   | CL_DEVICE_MAX_CONSTANT_ARGS
   | CL_DEVICE_LOCAL_MEM_TYPE
   | CL_DEVICE_LOCAL_MEM_SIZE
   | CL_DEVICE_ERROR_CORRECTION_SUPPORT
   | CL_DEVICE_PROFILING_TIMER_RESOLUTION
   | CL_DEVICE_ENDIAN_LITTLE
   | CL_DEVICE_AVAILABLE
   | CL_DEVICE_COMPILER_AVAILABLE
   | CL_DEVICE_EXECUTION_CAPABILITIES
   | CL_DEVICE_QUEUE_PROPERTIES
   | CL_DEVICE_NAME
   | CL_DEVICE_VENDOR
   | CL_DRIVER_VERSION
   | CL_DEVICE_PROFILE
   | CL_DEVICE_VERSION
   | CL_DEVICE_EXTENSIONS
   | CL_DEVICE_PLATFORM
   | CL_DEVICE_DOUBLE_FP_CONFIG
   | CL_DEVICE_HALF_FP_CONFIG
   | CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF
   | CL_DEVICE_HOST_UNIFIED_MEMORY
   | CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR
   | CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT
   | CL_DEVICE_NATIVE_VECTOR_WIDTH_INT
   | CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG
   | CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT
   | CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE
   | CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF
   | CL_DEVICE_OPENCL_C_VERSION
   | CL_DEVICE_LINKER_AVAILABLE
   | CL_DEVICE_BUILT_IN_KERNELS
   | CL_DEVICE_IMAGE_MAX_BUFFER_SIZE
   | CL_DEVICE_IMAGE_MAX_ARRAY_SIZE
   | CL_DEVICE_PARENT_DEVICE
   | CL_DEVICE_PARTITION_MAX_SUB_DEVICES
   | CL_DEVICE_PARTITION_PROPERTIES
   | CL_DEVICE_PARTITION_AFFINITY_DOMAIN
   | CL_DEVICE_PARTITION_TYPE
   | CL_DEVICE_REFERENCE_COUNT
   | CL_DEVICE_PREFERRED_INTEROP_USER_SYNC
   | CL_DEVICE_PRINTF_BUFFER_SIZE
   | CL_DEVICE_IMAGE_PITCH_ALIGNMENT
   | CL_DEVICE_IMAGE_BASE_ADDRESS_ALIGNMENT
   deriving (Enum)

instance CLConstant DeviceInfoTag where
   toCL x = fromIntegral (0x1000 + fromEnum x)
   fromCL x = toEnum (fromIntegral x - 0x1000)

data DeviceType = 
     CL_DEVICE_TYPE_DEFAULT
   | CL_DEVICE_TYPE_CPU
   | CL_DEVICE_TYPE_GPU
   | CL_DEVICE_TYPE_ACCELERATOR
   | CL_DEVICE_TYPE_CUSTOM
   deriving (Eq,Show,Bounded,Enum)

instance CLSet DeviceType

clDeviceTypeAll :: [DeviceType]
clDeviceTypeAll = [CL_DEVICE_TYPE_CPU,
                   CL_DEVICE_TYPE_GPU,
                   CL_DEVICE_TYPE_ACCELERATOR,
                   CL_DEVICE_TYPE_CUSTOM]

data CommandQueueProperty =
     CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE
   | CL_QUEUE_PROFILING_ENABLE
   deriving (Show, Bounded, Eq, Ord, Enum)

instance CLSet CommandQueueProperty

data CLDeviceFPConfig =
     CL_FP_DENORM
   | CL_FP_INF_NAN
   | CL_FP_ROUND_TO_NEAREST
   | CL_FP_ROUND_TO_ZERO
   | CL_FP_ROUND_TO_INF
   | CL_FP_FMA
   | CL_FP_SOFT_FLOAT
   | CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT
   deriving (Show, Bounded, Eq, Ord, Enum)

instance CLSet CLDeviceFPConfig

data CLDeviceExecCapability =
     CL_EXEC_KERNEL
   | CL_EXEC_NATIVE_KERNEL
   deriving (Show, Bounded, Eq, Ord, Enum)

instance CLSet CLDeviceExecCapability

data CLDeviceMemCacheType =
     CL_NONE
   | CL_READ_ONLY_CACHE
   | CL_READ_WRITE_CACHE
   deriving (Show,Enum)

instance CLConstant CLDeviceMemCacheType where
   toCL x = fromIntegral (fromEnum x)
   fromCL x = toEnum (fromIntegral x)

data CLDeviceLocalMemType =
     CL_LOCAL
   | CL_GLOBAL
   deriving (Show,Enum)

instance CLConstant CLDeviceLocalMemType where
   toCL x = fromIntegral (fromEnum x + 1)
   fromCL x = toEnum (fromIntegral x - 1)


data CLCommandType =
     CL_COMMAND_NDRANGE_KERNEL      
   | CL_COMMAND_TASK                
   | CL_COMMAND_NATIVE_KERNEL       
   | CL_COMMAND_READ_BUFFER         
   | CL_COMMAND_WRITE_BUFFER        
   | CL_COMMAND_COPY_BUFFER         
   | CL_COMMAND_READ_IMAGE          
   | CL_COMMAND_WRITE_IMAGE         
   | CL_COMMAND_COPY_IMAGE          
   | CL_COMMAND_COPY_IMAGE_TO_BUFFER
   | CL_COMMAND_COPY_BUFFER_TO_IMAGE
   | CL_COMMAND_MAP_BUFFER          
   | CL_COMMAND_MAP_IMAGE           
   | CL_COMMAND_UNMAP_MEM_OBJECT    
   | CL_COMMAND_MARKER              
   | CL_COMMAND_ACQUIRE_GL_OBJECTS  
   | CL_COMMAND_RELEASE_GL_OBJECTS  
   | CL_COMMAND_READ_BUFFER_RECT    
   | CL_COMMAND_WRITE_BUFFER_RECT   
   | CL_COMMAND_COPY_BUFFER_RECT    
   | CL_COMMAND_USER                
   | CL_COMMAND_BARRIER             
   | CL_COMMAND_MIGRATE_MEM_OBJECTS 
   | CL_COMMAND_FILL_BUFFER         
   | CL_COMMAND_FILL_IMAGE          
   deriving (Show,Enum)

instance CLConstant CLCommandType where
   toCL x = fromIntegral (fromEnum x + 0x11F0)
   fromCL x = toEnum (fromIntegral x - 0x11F0)

data CLCommandExecutionStatus =
     CL_EXEC_ERROR   -- -1
   | CL_COMPLETE     -- 0
   | CL_RUNNING      -- 1
   | CL_SUBMITTED    -- 2
   | CL_QUEUED       -- 3
   deriving (Show,Enum)

instance CLConstant CLCommandExecutionStatus where
   toCL x = fromIntegral (fromEnum x - 1)
   fromCL x = toEnum (fromIntegral x + 1)

data CLProfilingInfo =
     CL_PROFILING_COMMAND_QUEUED
   | CL_PROFILING_COMMAND_SUBMIT
   | CL_PROFILING_COMMAND_START
   | CL_PROFILING_COMMAND_END
   deriving (Show,Enum)

instance CLConstant CLProfilingInfo where
   toCL x = fromIntegral (fromEnum x + 0x1280)
   fromCL x = toEnum (fromIntegral x - 0x1280)

data CLMemFlag =
     CL_MEM_READ_WRITE        -- 1
   | CL_MEM_WRITE_ONLY        -- 2
   | CL_MEM_READ_ONLY         -- 4
   | CL_MEM_USE_HOST_PTR      -- 8
   | CL_MEM_ALLOC_HOST_PTR    -- 16
   | CL_MEM_COPY_HOST_PTR     -- 32
   | CL_MEM_RESERVED          -- 64
   | CL_MEM_HOST_WRITE_ONLY   -- 128
   | CL_MEM_HOST_READ_ONLY    -- 256
   | CL_MEM_HOST_NO_ACCESS    -- 512
   deriving (Show, Bounded, Eq, Ord, Enum)

instance CLSet CLMemFlag

data CLMapFlag =
     CL_MAP_READ
   | CL_MAP_WRITE
   | CL_MAP_WRITE_INVALIDATE_REGION
   deriving (Show, Bounded, Eq, Ord, Enum)

instance CLSet CLMapFlag

data CLMemObjectType =
     CL_MEM_OBJECT_BUFFER
   | CL_MEM_OBJECT_IMAGE2D
   | CL_MEM_OBJECT_IMAGE3D
   | CL_MEM_OBJECT_IMAGE2D_ARRAY
   | CL_MEM_OBJECT_IMAGE1D
   | CL_MEM_OBJECT_IMAGE1D_ARRAY
   | CL_MEM_OBJECT_IMAGE1D_BUFFER
   deriving (Show, Enum)

instance CLConstant CLMemObjectType where
   toCL x = fromIntegral (fromEnum x + 0x10F0)
   fromCL x = toEnum (fromIntegral x - 0x10F0)

data CLBuildStatus = 
     CL_BUILD_SUCCESS
   | CL_BUILD_NONE
   | CL_BUILD_ERROR
   | CL_BUILD_IN_PROGRESS
   deriving (Show,Enum)

instance CLConstant CLBuildStatus where
   toCL x = fromIntegral (fromEnum x * (-1))
   fromCL x = toEnum (fromIntegral x * (-1))

data CLAddressingMode =
     CL_ADDRESS_NONE
   | CL_ADDRESS_CLAMP_TO_EDGE
   | CL_ADDRESS_CLAMP
   | CL_ADDRESS_REPEAT
   | CL_ADDRESS_MIRRORED_REPEAT
   deriving (Show,Enum)

instance CLConstant CLAddressingMode where
   toCL x = fromIntegral (fromEnum x + 0x1130)
   fromCL x = toEnum (fromIntegral x - 0x1130)

data CLFilterMode =
     CL_FILTER_NEAREST
   | CL_FILTER_LINEAR
   deriving (Show,Enum)

instance CLConstant CLFilterMode where
   toCL x = fromIntegral (fromEnum x + 0x1140)
   fromCL x = toEnum (fromIntegral x - 0x1140)

data CLCommandQueueInfo = 
     CL_QUEUE_CONTEXT 
   | CL_QUEUE_DEVICE
   | CL_QUEUE_REFERENCE_COUNT
   | CL_QUEUE_PROPERTIES
   deriving (Enum)

instance CLConstant CLCommandQueueInfo where
   toCL x = fromIntegral (fromEnum x + 0x1090)
   fromCL x = toEnum (fromIntegral x - 0x1090)

