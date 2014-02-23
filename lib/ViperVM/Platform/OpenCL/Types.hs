{-# LANGUAGE ForeignFunctionInterface,
             DeriveDataTypeable, 
             GeneralizedNewtypeDeriving #-}
module ViperVM.Platform.OpenCL.Types where

import Foreign.Storable (Storable(..))
import Data.Word (Word8,Word32,Word64)
import Data.Int (Int32)
import Foreign.Ptr (Ptr,FunPtr,IntPtr)
import Foreign.C.Types (CSize(..))
import Foreign.C.String (CString)
import System.Posix.DynamicLinker

import ViperVM.Platform.OpenCL.ImageFormat
import ViperVM.Platform.OpenCL.Bindings

-- | An OpenCL library
data Library = Library {
   libHandle :: DL,
   rawClGetPlatformIDs    :: CLuint -> Ptr Platform_ -> Ptr CLuint -> IO CLint,
   rawClGetPlatformInfo   :: Platform_ -> PlatformInfo_ -> CSize -> Ptr () -> 
                              Ptr CSize -> IO CLint,
   rawClGetDeviceIDs      :: Platform_ -> DeviceType_ -> CLuint -> Ptr Device_ -> 
                              Ptr CLuint -> IO CLint,
   rawClGetDeviceInfo     :: Device_ -> DeviceInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,

   -- Memory
   rawClCreateBuffer      :: Context_ -> CLMemFlags_ -> CSize -> Ptr () -> Ptr CLint -> IO Mem_,
   rawClCreateImage2D     :: Context_ -> CLMemFlags_ -> CLImageFormat_p -> CSize -> 
                              CSize -> CSize -> Ptr () -> Ptr CLint -> IO Mem_,
   rawClCreateImage3D     :: Context_ -> CLMemFlags_-> CLImageFormat_p -> CSize -> 
                              CSize -> CSize -> CSize -> CSize -> Ptr () -> Ptr CLint -> IO Mem_,
   rawClCreateFromGLTexture2D :: Context_ -> CLMemFlags_ -> CLuint -> CLint -> CLuint -> 
                              Ptr CLint -> IO Mem_,
   rawClCreateFromGLBuffer :: Context_ -> CLMemFlags_ -> CLuint -> Ptr CLint -> IO Mem_,
   rawClRetainMemObject   :: Mem_ -> IO CLint,
   rawClReleaseMemObject  :: Mem_ -> IO CLint,
   rawClGetSupportedImageFormats :: Context_ -> CLMemFlags_ -> CLMemObjectType_ -> CLuint -> 
                              CLImageFormat_p -> Ptr CLuint -> IO CLint,
   rawClGetMemObjectInfo  :: Mem_ -> CLMemInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
   rawClGetImageInfo      :: Mem_ -> CLImageInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
   rawClCreateSampler     :: Context_ -> CLbool -> CLAddressingMode_ -> CLFilterMode_ -> 
                           Ptr CLint -> IO Sampler_,
   rawClRetainSampler     :: Sampler_ -> IO CLint,
   rawClReleaseSampler    :: Sampler_ -> IO CLint,
   rawClGetSamplerInfo    :: Sampler_ -> CLSamplerInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
 
   -- Context
   rawClCreateContext     :: Ptr CLContextProperty_ -> CLuint -> Ptr Device_ ->
                           FunPtr ContextCallback -> Ptr () -> Ptr CLint -> IO Context_,
   rawClCreateContextFromType :: Ptr CLContextProperty_ -> DeviceType_ -> FunPtr ContextCallback -> 
                           Ptr () -> Ptr CLint -> IO Context_,
   rawClRetainContext     :: Context_ -> IO CLint,
   rawClReleaseContext    :: Context_ -> IO CLint,
   rawClGetContextInfo    :: Context_ -> CLContextInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
 
   -- CommandQueue
   rawClCreateCommandQueue    :: Context_ -> Device_ -> CLCommandQueueProperty_ -> 
                                 Ptr CLint -> IO CommandQueue_,
   rawClRetainCommandQueue    :: CommandQueue_ -> IO CLint,
   rawClReleaseCommandQueue   :: CommandQueue_ -> IO CLint,
   rawClGetCommandQueueInfo   :: CommandQueue_ -> CLCommandQueueInfo_ -> CSize -> Ptr () -> 
                                 Ptr CSize -> IO CLint,
   rawClSetCommandQueueProperty :: CommandQueue_ -> CLCommandQueueProperty_ -> CLbool -> 
                                 Ptr CLCommandQueueProperty_ -> IO CLint,
   rawClEnqueueReadBuffer     :: CommandQueue_ -> Mem_ -> CLbool -> CSize -> CSize -> 
                                 Ptr () -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint,
   rawClEnqueueReadBufferRect :: Maybe(CommandQueue_ -> Mem_ -> CLbool -> Ptr CSize -> 
                                 Ptr CSize -> Ptr CSize -> CSize -> CSize -> CSize -> 
                                 CSize -> Ptr () -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint),
   rawClEnqueueWriteBuffer    :: CommandQueue_ -> Mem_ -> CLbool -> CSize -> CSize -> 
                                 Ptr () -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint,
   rawClEnqueueWriteBufferRect :: Maybe(CommandQueue_ -> Mem_ -> CLbool -> Ptr CSize -> 
                                 Ptr CSize -> Ptr CSize -> CSize -> CSize -> CSize -> CSize -> 
                                 Ptr () -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint),
   rawClEnqueueCopyBuffer    :: CommandQueue_ -> Mem_ -> Mem_ -> CSize -> CSize ->  
                                 CSize -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint,
   rawClEnqueueCopyBufferRect :: Maybe(CommandQueue_ -> Mem_ -> Mem_ -> Ptr CSize -> 
                                 Ptr CSize -> Ptr CSize -> CSize -> CSize -> CSize -> CSize -> 
                                 CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint),
   rawClEnqueueReadImage      :: CommandQueue_ -> Mem_ -> CLbool -> Ptr CSize -> Ptr CSize -> 
                                 CSize -> CSize -> Ptr () -> CLuint -> Ptr Event_ -> 
                                 Ptr Event_ -> IO CLint,
   rawClEnqueueWriteImage     :: CommandQueue_ -> Mem_ -> CLbool -> Ptr CSize -> Ptr CSize -> 
                                 CSize -> CSize -> Ptr () -> CLuint -> Ptr Event_ -> 
                                 Ptr Event_ -> IO CLint,
   rawClEnqueueCopyImage      :: CommandQueue_ -> Mem_ -> Mem_ -> Ptr CSize -> Ptr CSize -> 
                                 Ptr CSize -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint,
   rawClEnqueueCopyImageToBuffer :: CommandQueue_ -> Mem_ -> Mem_ -> Ptr CSize -> Ptr CSize -> 
                                 CSize -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint,
   rawClEnqueueCopyBufferToImage :: CommandQueue_ -> Mem_ -> Mem_ -> CSize -> Ptr CSize -> 
                                 Ptr CSize -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint,
   rawClEnqueueMapBuffer      :: CommandQueue_ -> Mem_ -> CLbool -> CLMapFlags_ -> CSize -> 
                                 CSize -> CLuint -> Ptr Event_ -> Ptr Event_ -> Ptr CLint -> IO (Ptr ()),
   rawClEnqueueMapImage       :: CommandQueue_ -> Mem_ -> CLbool -> CLMapFlags_ -> Ptr CSize -> 
                                 Ptr CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr Event_ -> 
                                 Ptr Event -> Ptr CLint -> IO (Ptr ()),
   rawClEnqueueUnmapMemObject :: CommandQueue_ -> Mem_ -> Ptr () -> CLuint -> Ptr Event_ -> 
                                 Ptr Event_ -> IO CLint,
   rawClEnqueueNDRangeKernel  :: CommandQueue_ -> Kernel_ -> CLuint -> Ptr CSize -> Ptr CSize -> 
                                 Ptr CSize -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint,
   rawClEnqueueNativeKernel   :: CommandQueue_ ->  FunPtr NativeKernelCallback -> Ptr () -> 
                                 CSize -> CLuint -> Ptr Mem_ -> Ptr (Ptr ()) -> CLuint -> 
                                 Ptr Event_ -> Ptr Event_ -> IO CLint,
   rawClEnqueueTask           :: CommandQueue_ -> Kernel_ -> CLuint -> Ptr Event_ -> 
                                 Ptr Event_ -> IO CLint,
   rawClEnqueueMarker         :: CommandQueue_ -> Ptr Event_ -> IO CLint ,
   rawClEnqueueWaitForEvents  :: CommandQueue_ -> CLuint -> Ptr Event_ -> IO CLint,
   rawClEnqueueBarrier        :: CommandQueue_ -> IO CLint ,
   rawClFlush                 :: CommandQueue_ -> IO CLint,
   rawClFinish                :: CommandQueue_ -> IO CLint,
   -- Event
   rawClWaitForEvents         :: CLuint -> Ptr Event_ -> IO CLint,
   rawClGetEventInfo          :: Event_ -> CLEventInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
   rawClRetainEvent           :: Event_ -> IO CLint ,
   rawClReleaseEvent          :: Event_ -> IO CLint ,
   rawClGetEventProfilingInfo :: Event_ -> CLProfilingInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
   -- Program_
   rawClCreateProgramWithSource :: Context_ -> CLuint -> Ptr CString -> Ptr CSize -> Ptr CLint -> IO Program_,
   rawClCreateProgramWithBinary :: Context_ -> CLuint -> Ptr Device_ -> Ptr CSize -> 
                                 Ptr (Ptr Word8) -> Ptr CLint -> Ptr CLint -> IO Program_,
   rawClRetainProgram         :: Program_ -> IO CLint,
   rawClReleaseProgram        :: Program_ -> IO CLint,
   rawClBuildProgram          :: Program_ -> CLuint -> Ptr Device_ -> CString -> 
                                 FunPtr BuildCallback -> Ptr () -> IO CLint,
   rawClUnloadCompiler        :: IO CLint,
   rawClGetProgramInfo        :: Program_ -> CLProgramInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
   rawClGetProgramBuildInfo   :: Program_ -> Device_ -> CLProgramBuildInfo_ -> CSize -> 
                                 Ptr () -> Ptr CSize -> IO CLint,
   rawClCreateKernel          :: Program_ -> CString -> Ptr CLint -> IO Kernel_ ,
   rawClCreateKernelsInProgram :: Program_ -> CLuint -> Ptr Kernel_ -> Ptr CLuint -> IO CLint ,
   rawClRetainKernel          :: Kernel_ -> IO CLint ,
   rawClReleaseKernel         :: Kernel_ -> IO CLint ,
   rawClSetKernelArg          :: Kernel_ -> CLuint -> CSize -> Ptr () -> IO CLint,
   rawClGetKernelInfo         :: Kernel_ -> CLKernelInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
   rawClGetKernelWorkGroupInfo :: Kernel_ -> Device_ -> CLKernelWorkGroupInfo_ -> CSize -> 
                                 Ptr () -> Ptr CSize -> IO CLint
}

instance Eq Library where
  (==) a b = (==) (packDL $ libHandle a) (packDL $ libHandle b)

instance Ord Library where
  compare a b = compare (packDL $ libHandle a) (packDL $ libHandle b)


-- Entities
data Platform = Platform Library Platform_ deriving (Eq)
data Device = Device Library Device_ deriving (Eq)
data Context = Context Library Context_ deriving (Eq)
data CommandQueue = CommandQueue Library CommandQueue_ deriving (Eq)
data Mem = Mem Library Mem_ deriving (Eq)
data Program = Program Library Program_ deriving (Eq)
data Event = Event Library Event_ deriving (Eq)
data Kernel = Kernel Library Kernel_ deriving (Eq)
data Sampler = Sampler Library Sampler_ deriving (Eq)

class Entity e where 
   unwrap :: e -> Ptr ()
   cllib :: e -> Library

instance Entity Platform where 
   unwrap (Platform _ x) = x
   cllib (Platform l _) = l

instance Entity Device where 
   unwrap (Device _ x) = x
   cllib (Device l _) = l

instance Entity Context where 
   unwrap (Context _ x) = x
   cllib (Context l _) = l

instance Entity CommandQueue where 
   unwrap (CommandQueue _ x) = x
   cllib (CommandQueue l _) = l

instance Entity Mem where 
   unwrap (Mem _ x) = x
   cllib (Mem l _) = l

instance Entity Event where 
   unwrap (Event _ x) = x
   cllib (Event l _) = l

instance Entity Program where 
   unwrap (Program _ x) = x
   cllib (Program l _) = l

instance Entity Kernel where 
   unwrap (Kernel _ x) = x
   cllib (Kernel l _) = l

instance Entity Sampler where 
   unwrap (Sampler _ x) = x
   cllib (Sampler l _) = l


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

