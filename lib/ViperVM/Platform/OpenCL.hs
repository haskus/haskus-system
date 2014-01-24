{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module ViperVM.Platform.OpenCL (
   Platform, Device, Context, CommandQueue, Mem,
   Event, Program, Kernel, Sampler, Library,
   PlatformInfo(..),
   loadOpenCL,
   -- Platforms
   getNumPlatforms, getPlatforms, 
   getPlatformNumDevices, getPlatformDevices, 
   getPlatformName, getPlatformName', 
   getPlatformVendor, getPlatformVendor',
   getPlatformProfile, getPlatformProfile',
   getPlatformVersion, getPlatformVersion',
   getPlatformExtensions, getPlatformExtensions',
   getPlatformInfos',
   -- Contexts
   createContext, releaseContext,
   -- Buffers
   createBuffer, releaseBuffer,
   -- Commands
   createCommandQueue, releaseCommandQueue,
   flush, finish, enqueueBarrier,
   enqueueReadBuffer, enqueueWriteBuffer,
   -- Events
   waitForEvents
) where

import Control.Applicative
import Control.Monad (void)
import Control.Exception
import Data.Typeable
import Data.Bits
import Data.Int
import Data.List (stripPrefix)
import Data.Word
import Data.Maybe
import Foreign.C.String
import Foreign.C.Types
import Foreign (allocaArray,peekArray, pokeArray)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr
import Foreign.Storable
import System.Posix.DynamicLinker
import System.Posix.DynamicLinker.Template

-----------------------------------------------------
-- OpenCL types
-----------------------------------------------------

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
newtype CLbool = CLbool CLuint
type CLbitfield = CLulong

fromBool :: Bool -> CLbool
fromBool False = CLbool 0
fromBool True = CLbool 1

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

data CLImageFormat = CLImageFormat {
   image_channel_order :: !CLChannelOrder,
   image_channel_data_type :: !CLChannelType
} deriving (Show)

instance Storable CLImageFormat where
   alignment _ = alignment (undefined :: CDouble)
   sizeOf _ = 64
   peek p = do
      a <- fmap fromCL (peekByteOff p 0 :: IO Word32)
      b <- fmap fromCL (peekByteOff p 4 :: IO Word32)
      return $ CLImageFormat a b
   poke p (CLImageFormat a b) = do
      pokeByteOff p 0 (toCL a :: Word32)
      pokeByteOff p 4 (toCL b :: Word32)

type ContextCallback = CString -> Ptr () -> CSize -> Ptr () -> IO ()

foreign import ccall "wrapper" wrapContextCallback :: 
  ContextCallback -> IO (FunPtr ContextCallback)

type NativeKernelCallback = Ptr () -> IO ()

foreign import ccall "wrapper" wrapNativeKernelCallback :: 
  NativeKernelCallback -> IO (FunPtr NativeKernelCallback)


type BuildCallback = Program -> Ptr () -> IO ()

class Enum a => CLConstant a where
   toCL :: Integral b => a -> b
   fromCL :: Integral b => b -> a

class (Bounded a, Enum a) => CLSet a where
   toCLSet :: (Bits b, Integral b) => [a] -> b
   toCLSet = sum . map f
      where f = shiftL 1 . fromIntegral . fromEnum

   fromCLSet :: (Bits b, Integral b) => b -> [a]
   fromCLSet x = mapMaybe f [0..maxBound]
      where f idx = if testBit x idx
                        then Just (toEnum (idx+1)) 
                        else Nothing

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

data CLChannelOrder =
     CL_R                         
   | CL_A                         
   | CL_RG                        
   | CL_RA                        
   | CL_RGB                       
   | CL_RGBA                      
   | CL_BGRA                      
   | CL_ARGB                      
   | CL_INTENSITY                 
   | CL_LUMINANCE                 
   | CL_Rx                        
   | CL_RGx                       
   | CL_RGBx                      
   | CL_DEPTH                     
   | CL_DEPTH_STENCIL             
   deriving (Show,Enum)

instance CLConstant CLChannelOrder where
   toCL x = fromIntegral (0x10B0 + fromEnum x)
   fromCL x = toEnum (fromIntegral x - 0x10B0)

data CLChannelType = 
     CL_SNORM_INT8                 
   | CL_SNORM_INT16                
   | CL_UNORM_INT8                 
   | CL_UNORM_INT16                
   | CL_UNORM_SHORT_565            
   | CL_UNORM_SHORT_555            
   | CL_UNORM_INT_101010           
   | CL_SIGNED_INT8                
   | CL_SIGNED_INT16               
   | CL_SIGNED_INT32               
   | CL_UNSIGNED_INT8              
   | CL_UNSIGNED_INT16             
   | CL_UNSIGNED_INT32             
   | CL_HALF_FLOAT                 
   | CL_FLOAT                      
   | CL_UNORM_INT24                
   deriving (Show,Enum)

instance CLConstant CLChannelType where
   toCL x = fromIntegral (0x10D0 + fromEnum x)
   fromCL x = toEnum (fromIntegral x - 0x10D0)

data CLError =
     CL_SUCCESS                                  -- 0
   | CL_DEVICE_NOT_FOUND                         -- -1
   | CL_DEVICE_NOT_AVAILABLE                     -- -2
   | CL_COMPILER_NOT_AVAILABLE                   -- -3
   | CL_MEM_OBJECT_ALLOCATION_FAILURE            -- -4
   | CL_OUT_OF_RESOURCES                         -- -5
   | CL_OUT_OF_HOST_MEMORY                       -- -6
   | CL_PROFILING_INFO_NOT_AVAILABLE             -- -7
   | CL_MEM_COPY_OVERLAP                         -- -8
   | CL_IMAGE_FORMAT_MISMATCH                    -- -9
   | CL_IMAGE_FORMAT_NOT_SUPPORTED               -- -10
   | CL_BUILD_PROGRAM_FAILURE                    -- -11
   | CL_MAP_FAILURE                              -- -12
   | CL_MISALIGNED_SUB_BUFFER_OFFSET             -- -13
   | CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST-- -14
   | CL_COMPILE_PROGRAM_FAILURE                  -- -15
   | CL_LINKER_NOT_AVAILABLE                     -- -16
   | CL_LINK_PROGRAM_FAILURE                     -- -17
   | CL_DEVICE_PARTITION_FAILED                  -- -18
   | CL_KERNEL_ARG_INFO_NOT_AVAILABLE            -- -19

   | CL_ERROR_RESERVED_0                         -- -20
   | CL_ERROR_RESERVED_1                         -- -21
   | CL_ERROR_RESERVED_2                         -- -22
   | CL_ERROR_RESERVED_3                         -- -23
   | CL_ERROR_RESERVED_4                         -- -24
   | CL_ERROR_RESERVED_5                         -- -25
   | CL_ERROR_RESERVED_6                         -- -26
   | CL_ERROR_RESERVED_7                         -- -27
   | CL_ERROR_RESERVED_8                         -- -28
   | CL_ERROR_RESERVED_9                         -- -29

   | CL_INVALID_VALUE                            -- -30
   | CL_INVALID_DEVICE_TYPE                      -- -31
   | CL_INVALID_PLATFORM                         -- -32
   | CL_INVALID_DEVICE                           -- -33
   | CL_INVALID_CONTEXT                          -- -34
   | CL_INVALID_QUEUE_PROPERTIES                 -- -35
   | CL_INVALID_COMMAND_QUEUE                    -- -36
   | CL_INVALID_HOST_PTR                         -- -37
   | CL_INVALID_MEM_OBJECT                       -- -38
   | CL_INVALID_IMAGE_FORMAT_DESCRIPTOR          -- -39
   | CL_INVALID_IMAGE_SIZE                       -- -40
   | CL_INVALID_SAMPLER                          -- -41
   | CL_INVALID_BINARY                           -- -42
   | CL_INVALID_BUILD_OPTIONS                    -- -43
   | CL_INVALID_PROGRAM                          -- -44
   | CL_INVALID_PROGRAM_EXECUTABLE               -- -45
   | CL_INVALID_KERNEL_NAME                      -- -46
   | CL_INVALID_KERNEL_DEFINITION                -- -47
   | CL_INVALID_KERNEL                           -- -48
   | CL_INVALID_ARG_INDEX                        -- -49
   | CL_INVALID_ARG_VALUE                        -- -50
   | CL_INVALID_ARG_SIZE                         -- -51
   | CL_INVALID_KERNEL_ARGS                      -- -52
   | CL_INVALID_WORK_DIMENSION                   -- -53
   | CL_INVALID_WORK_GROUP_SIZE                  -- -54
   | CL_INVALID_WORK_ITEM_SIZE                   -- -55
   | CL_INVALID_GLOBAL_OFFSET                    -- -56
   | CL_INVALID_EVENT_WAIT_LIST                  -- -57
   | CL_INVALID_EVENT                            -- -58
   | CL_INVALID_OPERATION                        -- -59
   | CL_INVALID_GL_OBJECT                        -- -60
   | CL_INVALID_BUFFER_SIZE                      -- -61
   | CL_INVALID_MIP_LEVEL                        -- -62
   | CL_INVALID_GLOBAL_WORK_SIZE                 -- -63
   | CL_INVALID_PROPERTY                         -- -64
   | CL_INVALID_IMAGE_DESCRIPTOR                 -- -65
   | CL_INVALID_COMPILER_OPTIONS                 -- -66
   | CL_INVALID_LINKER_OPTIONS                   -- -67
   | CL_INVALID_DEVICE_PARTITION_COUNT           -- -68

   | CL_PLATFORM_NOT_FOUND_KHR                   -- -1001
   deriving (Show, Eq, Enum, Typeable)

instance CLConstant CLError where
   toCL CL_PLATFORM_NOT_FOUND_KHR = -1001
   toCL x = fromIntegral (-1 * fromEnum x)

   fromCL (-1001) = CL_PLATFORM_NOT_FOUND_KHR
   fromCL x = toEnum (-1 * fromIntegral x)

instance Exception CLError where

-- | Convert an explicit error into a exception
toException :: Either CLError a -> a
toException (Right a) = a
toException (Left a) = throw a

wrapPError :: (Ptr CLint -> IO a) -> IO (Either CLError a)
wrapPError f = alloca $ \perr -> do
  v <- f perr
  errcode <- fromCL <$> peek perr
  return $ if errcode == CL_SUCCESS
    then Right v
    else Left errcode
  
wrapCheckSuccess :: IO CLint -> IO Bool
wrapCheckSuccess f = f >>= return . (==CL_SUCCESS) . fromCL

wrapGetInfo :: Storable a => (Ptr a -> Ptr CSize -> IO CLint) -> (a -> b) -> IO (Either CLError b)
wrapGetInfo fget fconvert= alloca $ \dat -> do
  errcode <- fget dat nullPtr
  if errcode == toCL CL_SUCCESS
    then Right . fconvert <$> peek dat
    else return (Left (fromCL errcode))

whenSuccess :: IO CLint -> IO a -> IO (Either CLError a)
whenSuccess fcheck fval = do
  errcode <- fcheck
  if errcode == toCL CL_SUCCESS
    then Right <$> fval
    else return $ Left (fromCL errcode)

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


data DeviceType = 
     CL_DEVICE_TYPE_DEFAULT
   | CL_DEVICE_TYPE_CPU
   | CL_DEVICE_TYPE_GPU
   | CL_DEVICE_TYPE_ACCELERATOR
   | CL_DEVICE_TYPE_CUSTOM
   deriving (Show,Bounded,Enum)

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

withMaybeArray :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withMaybeArray [] = ($ nullPtr)
withMaybeArray xs = withArray xs

-----------------------------------------------------
-- OpenCL library
-----------------------------------------------------

data Library = Library {
   libHandle :: DL,
   rawClGetPlatformIDs    :: CLuint -> Ptr Platform -> Ptr CLuint -> IO CLint,
   rawClGetPlatformInfo   :: Platform -> PlatformInfo_ -> CSize -> Ptr () -> 
                              Ptr CSize -> IO CLint,
   rawClGetDeviceIDs      :: Platform -> DeviceType_ -> CLuint -> Ptr Device -> 
                              Ptr CLuint -> IO CLint,
   rawClGetDeviceInfo     :: Device -> DeviceInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,

   -- Memory
   rawClCreateBuffer      :: Context -> CLMemFlags_ -> CSize -> Ptr () -> Ptr CLint -> IO Mem,
   rawClCreateImage2D     :: Context -> CLMemFlags_ -> CLImageFormat_p -> CSize -> 
                              CSize -> CSize -> Ptr () -> Ptr CLint -> IO Mem,
   rawClCreateImage3D     :: Context -> CLMemFlags_-> CLImageFormat_p -> CSize -> 
                              CSize -> CSize -> CSize -> CSize -> Ptr () -> Ptr CLint -> IO Mem,
   rawClCreateFromGLTexture2D :: Context -> CLMemFlags_ -> CLuint -> CLint -> CLuint -> 
                              Ptr CLint -> IO Mem,
   rawClCreateFromGLBuffer :: Context -> CLMemFlags_ -> CLuint -> Ptr CLint -> IO Mem,
   rawClRetainMemObject   :: Mem -> IO CLint,
   rawClReleaseMemObject  :: Mem -> IO CLint,
   rawClGetSupportedImageFormats :: Context -> CLMemFlags_ -> CLMemObjectType_ -> CLuint -> 
                              CLImageFormat_p -> Ptr CLuint -> IO CLint,
   rawClGetMemObjectInfo  :: Mem -> CLMemInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
   rawClGetImageInfo      :: Mem -> CLImageInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
   rawClCreateSampler     :: Context -> CLbool -> CLAddressingMode_ -> CLFilterMode_ -> 
                           Ptr CLint -> IO Sampler,
   rawClRetainSampler     :: Sampler -> IO CLint,
   rawClReleaseSampler    :: Sampler -> IO CLint,
   rawClGetSamplerInfo    :: Sampler -> CLSamplerInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
 
   -- Context
   rawClCreateContext     :: Ptr CLContextProperty_ -> CLuint -> Ptr Device ->
                           FunPtr ContextCallback -> Ptr () -> Ptr CLint -> IO Context,
   rawClCreateContextFromType :: Ptr CLContextProperty_ -> DeviceType_ -> FunPtr ContextCallback -> 
                           Ptr () -> Ptr CLint -> IO Context,
   rawClRetainContext     :: Context -> IO CLint,
   rawClReleaseContext    :: Context -> IO CLint,
   rawClGetContextInfo    :: Context -> CLContextInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
 
   -- CommandQueue
   rawClCreateCommandQueue    :: Context -> Device -> CLCommandQueueProperty_ -> 
                                 Ptr CLint -> IO CommandQueue,
   rawClRetainCommandQueue    :: CommandQueue -> IO CLint,
   rawClReleaseCommandQueue   :: CommandQueue -> IO CLint,
   rawClGetCommandQueueInfo   :: CommandQueue -> CLCommandQueueInfo_ -> CSize -> Ptr () -> 
                                 Ptr CSize -> IO CLint,
   rawClSetCommandQueueProperty :: CommandQueue -> CLCommandQueueProperty_ -> CLbool -> 
                                 Ptr CLCommandQueueProperty_ -> IO CLint,
   rawClEnqueueReadBuffer     :: CommandQueue -> Mem -> CLbool -> CSize -> CSize -> 
                                 Ptr () -> CLuint -> Ptr Event -> Ptr Event -> IO CLint,
   rawClEnqueueReadBufferRect :: Maybe(CommandQueue -> Mem -> CLbool -> Ptr CSize -> 
                                 Ptr CSize -> Ptr CSize -> CSize -> CSize -> CSize -> 
                                 CSize -> Ptr () -> CLuint -> Ptr Event -> Ptr Event -> IO CLint),
   rawClEnqueueWriteBuffer    :: CommandQueue -> Mem -> CLbool -> CSize -> CSize -> 
                                 Ptr () -> CLuint -> Ptr Event -> Ptr Event -> IO CLint,
   rawClEnqueueWriteBufferRect :: Maybe(CommandQueue -> Mem -> CLbool -> Ptr CSize -> 
                                 Ptr CSize -> Ptr CSize -> CSize -> CSize -> CSize -> CSize -> 
                                 Ptr () -> CLuint -> Ptr Event -> Ptr Event -> IO CLint),
   rawClEnqueueCopyBuffer    :: CommandQueue -> Mem -> Mem -> CSize -> CSize ->  
                                 CSize -> CLuint -> Ptr Event -> Ptr Event -> IO CLint,
   rawClEnqueueCopyBufferRect :: Maybe(CommandQueue -> Mem -> Mem -> Ptr CSize -> 
                                 Ptr CSize -> Ptr CSize -> CSize -> CSize -> CSize -> CSize -> 
                                 CLuint -> Ptr Event -> Ptr Event -> IO CLint),
   rawClEnqueueReadImage      :: CommandQueue -> Mem -> CLbool -> Ptr CSize -> Ptr CSize -> 
                                 CSize -> CSize -> Ptr () -> CLuint -> Ptr Event -> 
                                 Ptr Event -> IO CLint,
   rawClEnqueueWriteImage     :: CommandQueue -> Mem -> CLbool -> Ptr CSize -> Ptr CSize -> 
                                 CSize -> CSize -> Ptr () -> CLuint -> Ptr Event -> 
                                 Ptr Event -> IO CLint,
   rawClEnqueueCopyImage      :: CommandQueue -> Mem -> Mem -> Ptr CSize -> Ptr CSize -> 
                                 Ptr CSize -> CLuint -> Ptr Event -> Ptr Event -> IO CLint,
   rawClEnqueueCopyImageToBuffer :: CommandQueue -> Mem -> Mem -> Ptr CSize -> Ptr CSize -> 
                                 CSize -> CLuint -> Ptr Event -> Ptr Event -> IO CLint,
   rawClEnqueueCopyBufferToImage :: CommandQueue -> Mem -> Mem -> CSize -> Ptr CSize -> 
                                 Ptr CSize -> CLuint -> Ptr Event -> Ptr Event -> IO CLint,
   rawClEnqueueMapBuffer      :: CommandQueue -> Mem -> CLbool -> CLMapFlags_ -> CSize -> 
                                 CSize -> CLuint -> Ptr Event -> Ptr Event -> Ptr CLint -> IO (Ptr ()),
   rawClEnqueueMapImage       :: CommandQueue -> Mem -> CLbool -> CLMapFlags_ -> Ptr CSize -> 
                                 Ptr CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr Event -> 
                                 Ptr Event -> Ptr CLint -> IO (Ptr ()),
   rawClEnqueueUnmapMemObject :: CommandQueue -> Mem -> Ptr () -> CLuint -> Ptr Event -> 
                                 Ptr Event -> IO CLint,
   rawClEnqueueNDRangeKernel  :: CommandQueue -> Kernel -> CLuint -> Ptr CSize -> Ptr CSize -> 
                                 Ptr CSize -> CLuint -> Ptr Event -> Ptr Event -> IO CLint,
   rawClEnqueueNativeKernel   :: CommandQueue ->  FunPtr NativeKernelCallback -> Ptr () -> 
                                 CSize -> CLuint -> Ptr Mem -> Ptr (Ptr ()) -> CLuint -> 
                                 Ptr Event -> Ptr Event -> IO CLint,
   rawClEnqueueTask           :: CommandQueue -> Kernel -> CLuint -> Ptr Event -> 
                                 Ptr Event -> IO CLint,
   rawClEnqueueMarker         :: CommandQueue -> Ptr Event -> IO CLint ,
   rawClEnqueueWaitForEvents  :: CommandQueue -> CLuint -> Ptr Event -> IO CLint,
   rawClEnqueueBarrier        :: CommandQueue -> IO CLint ,
   rawClFlush                 :: CommandQueue -> IO CLint,
   rawClFinish                :: CommandQueue -> IO CLint,
   -- Event
   rawClWaitForEvents         :: CLuint -> Ptr Event -> IO CLint,
   rawClGetEventInfo          :: Event -> CLEventInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
   rawClRetainEvent           :: Event -> IO CLint ,
   rawClReleaseEvent          :: Event -> IO CLint ,
   rawClGetEventProfilingInfo :: Event -> CLProfilingInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
   -- Program
   rawClCreateProgramWithSource :: Context -> CLuint -> Ptr CString -> Ptr CSize -> Ptr CLint -> IO Program,
   rawClCreateProgramWithBinary :: Context -> CLuint -> Ptr Device -> Ptr CSize -> 
                                 Ptr (Ptr Word8) -> Ptr CLint -> Ptr CLint -> IO Program,
   rawClRetainProgram         :: Program -> IO CLint,
   rawClReleaseProgram        :: Program -> IO CLint,
   rawClBuildProgram          :: Program -> CLuint -> Ptr Device -> CString -> 
                                 FunPtr BuildCallback -> Ptr () -> IO CLint,
   rawClUnloadCompiler        :: IO CLint,
   rawClGetProgramInfo        :: Program -> CLProgramInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
   rawClGetProgramBuildInfo   :: Program -> Device -> CLProgramBuildInfo_ -> CSize -> 
                                 Ptr () -> Ptr CSize -> IO CLint,
   rawClCreateKernel          :: Program -> CString -> Ptr CLint -> IO Kernel ,
   rawClCreateKernelsInProgram :: Program -> CLuint -> Ptr Kernel -> Ptr CLuint -> IO CLint ,
   rawClRetainKernel          :: Kernel -> IO CLint ,
   rawClReleaseKernel         :: Kernel -> IO CLint ,
   rawClSetKernelArg          :: Kernel -> CLuint -> CSize -> Ptr () -> IO CLint,
   rawClGetKernelInfo         :: Kernel -> CLKernelInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
   rawClGetKernelWorkGroupInfo :: Kernel -> Device -> CLKernelWorkGroupInfo_ -> CSize -> 
                                 Ptr () -> Ptr CSize -> IO CLint
}


instance Eq Library where
  (==) a b = (==) (packDL $ libHandle a) (packDL $ libHandle b)

instance Ord Library where
  compare a b = compare (packDL $ libHandle a) (packDL $ libHandle b)

myMod :: String -> String
myMod x = fromJust $ ("c" ++) <$> stripPrefix "rawC" x

$(makeDynamicLinker ''Library CCall 'myMod)

loadOpenCL :: String -> IO Library
loadOpenCL lib = loadLibrary lib [RTLD_NOW,RTLD_LOCAL]


-----------------------------------------------------
-- OpenCL functions
-----------------------------------------------------

-- | Return the number of available platforms
getNumPlatforms :: Library -> IO Word32
getNumPlatforms lib = alloca $ \numPlatforms -> do 
   err <- rawClGetPlatformIDs lib 0 nullPtr numPlatforms
   case fromCL err of
      CL_SUCCESS -> peek numPlatforms
      _ -> return 0 -- the ICD may return an error CL_PLATFORM_NOT_FOUND_KHR...

-- | Get available platforms
getPlatforms :: Library -> IO [Platform]
getPlatforms lib = do
   nplats <- getNumPlatforms lib
   if nplats == 0
      then return []
      else allocaArray (fromIntegral nplats) $ \plats -> do
         err <- rawClGetPlatformIDs lib nplats plats nullPtr
         case fromCL err of
            CL_SUCCESS -> peekArray (fromIntegral nplats) plats
            _ -> return []

-- | Return the number of available devices
getPlatformNumDevices :: Library -> Platform -> IO Word32
getPlatformNumDevices lib pf = alloca $ \numDevices -> do 
   err <- rawClGetDeviceIDs lib pf (toCLSet clDeviceTypeAll) 0 nullPtr numDevices
   case fromCL err of
      CL_SUCCESS -> peek numDevices
      _ -> return 0

-- | Get available platform devices
getPlatformDevices :: Library -> Platform -> IO [Device]
getPlatformDevices lib pf = do
   nbDevices <- getPlatformNumDevices lib pf
   if nbDevices == 0
      then return []
      else allocaArray (fromIntegral nbDevices) $ \devs -> do
         err <- rawClGetDeviceIDs lib pf (toCLSet clDeviceTypeAll) nbDevices devs nullPtr
         case fromCL err of
            CL_SUCCESS -> peekArray (fromIntegral nbDevices) devs
            _ -> return []

-- | Get platform info
getPlatformInfo :: Library -> PlatformInfoTag -> Platform -> IO (Either CLError String)
getPlatformInfo lib infoid platform = getSize >>>= getInfo
   where
      (>>>=) f g = do
         t <- f
         case t of
            Left a -> return (Left a)
            Right a -> g a

      -- Get output size
      getSize :: IO (Either CLError CSize)
      getSize = alloca $ \sz -> whenSuccess 
         (rawClGetPlatformInfo lib platform (toCL infoid) 0 nullPtr sz) 
         (peek sz)

      -- Get info
      getInfo :: CSize -> IO (Either CLError String)
      getInfo size = allocaArray (fromIntegral size) $ \buff -> whenSuccess 
         (rawClGetPlatformInfo lib platform (toCL infoid) size (castPtr buff) nullPtr)
         (peekCString buff)

-- | Get platform info (throw an exception if an error occurs)
getPlatformInfo' :: Library -> PlatformInfoTag -> Platform -> IO String
getPlatformInfo' lib infoid platform = toException <$> getPlatformInfo lib infoid platform

-- | Get platform name
getPlatformName :: Library -> Platform -> IO (Either CLError String)
getPlatformName = flip getPlatformInfo CL_PLATFORM_NAME

-- | Get platform name (throw an exception if an error occurs)
getPlatformName' :: Library -> Platform -> IO String
getPlatformName' = flip getPlatformInfo' CL_PLATFORM_NAME

-- | Get platform vendor
getPlatformVendor :: Library -> Platform -> IO (Either CLError String)
getPlatformVendor = flip getPlatformInfo CL_PLATFORM_VENDOR

-- | Get platform vendor (throw an exception if an error occurs)
getPlatformVendor' :: Library -> Platform -> IO String
getPlatformVendor' = flip getPlatformInfo' CL_PLATFORM_VENDOR

-- | Get platform profile
getPlatformProfile :: Library -> Platform -> IO (Either CLError String)
getPlatformProfile = flip getPlatformInfo CL_PLATFORM_PROFILE

-- | Get platform profile (throw an exception if an error occurs)
getPlatformProfile' :: Library -> Platform -> IO String
getPlatformProfile' = flip getPlatformInfo' CL_PLATFORM_PROFILE

-- | Get platform version
getPlatformVersion :: Library -> Platform -> IO (Either CLError String)
getPlatformVersion = flip getPlatformInfo CL_PLATFORM_VERSION

-- | Get platform version (throw an exception if an error occurs)
getPlatformVersion' :: Library -> Platform -> IO String
getPlatformVersion' = flip getPlatformInfo' CL_PLATFORM_VERSION

-- | Get platform extensions
getPlatformExtensions :: Library -> Platform -> IO (Either CLError [String])
getPlatformExtensions lib pf = fmap words <$> getPlatformInfo lib CL_PLATFORM_EXTENSIONS pf

-- | Get platform extensions (throw an exception if an error occurs)
getPlatformExtensions' :: Library -> Platform -> IO [String]
getPlatformExtensions' lib pf = words <$> getPlatformInfo' lib CL_PLATFORM_EXTENSIONS pf

data PlatformInfo = PlatformInfo {
   platformName :: String,
   platformVendor :: String,
   platformProfile :: String,
   platformVersion :: String,
   platformExtensions :: [String]
} deriving (Show)

-- | Get platform informations (throw an exception if an error occurs)
getPlatformInfos' :: Library -> Platform -> IO PlatformInfo
getPlatformInfos' lib pf = PlatformInfo
   <$> getPlatformName' lib pf
   <*> getPlatformVendor' lib pf
   <*> getPlatformProfile' lib pf
   <*> getPlatformVersion' lib pf
   <*> getPlatformExtensions' lib pf

-- | Create a context
createContext :: Library -> Platform -> [Device] -> IO (Either CLError Context)
createContext lib pf devs = do
   let props = [toCL CL_CONTEXT_PLATFORM, ptrToIntPtr (unwrap pf), 0]
       ndevs = fromIntegral (length devs)
   withArray devs $ \devs' ->
      withArray props $ \props' ->
         wrapPError (rawClCreateContext lib props' ndevs devs' nullFunPtr nullPtr)

-- | Release a context
releaseContext :: Library -> Context -> IO ()
releaseContext lib ctx = void (rawClReleaseContext lib ctx)

-- | Create a buffer
createBuffer :: Library -> Device -> Context -> [CLMemFlag] -> CSize -> IO (Either CLError Mem)
createBuffer lib _ ctx flags size = do
   mem <- wrapPError (rawClCreateBuffer lib ctx (toCLSet flags) size nullPtr)
   --FIXME: ensure buffer is allocated 
   --  use clEnqueueMigrateMemObjects if available (OpenCL 1.1 or 1.2?)
   --  perform a dummy operation on the buffer (OpenCL 1.0)
   return mem

-- | Release a buffer
releaseBuffer :: Library -> Mem -> IO ()
releaseBuffer lib mem = void (rawClReleaseMemObject lib mem)

-- | Create a command queue
createCommandQueue :: Library -> Context -> Device -> [CommandQueueProperty] -> IO (Either CLError CommandQueue)
createCommandQueue lib ctx dev props =
   wrapPError (rawClCreateCommandQueue lib ctx dev (toCLSet props))

-- | Release a command queue
releaseCommandQueue :: Library -> CommandQueue -> IO ()
releaseCommandQueue lib cq = void (rawClReleaseCommandQueue lib cq)

-- | Helper function to enqueue commands
enqueue :: (CLuint -> Ptr Event -> Ptr Event -> IO CLint) -> [Event] -> IO (Either CLError Event)
enqueue f [] = alloca $ \event -> whenSuccess (f 0 nullPtr event) (peek event)
enqueue f events = allocaArray nevents $ \pevents -> do
  pokeArray pevents events
  alloca $ \event -> whenSuccess (f cnevents pevents event) (peek event)
    where
      nevents = length events
      cnevents = fromIntegral nevents

-- | Transfer from device to host
enqueueReadBuffer :: Library -> CommandQueue -> Mem -> Bool -> CSize -> CSize -> Ptr () -> [Event] -> IO (Either CLError Event)
enqueueReadBuffer lib cq mem blocking off size ptr = 
   enqueue (rawClEnqueueReadBuffer lib cq mem (fromBool blocking) off size ptr)

-- | Transfer from host to device
enqueueWriteBuffer :: Library -> CommandQueue -> Mem -> Bool -> CSize -> CSize -> Ptr () -> [Event] -> IO (Either CLError Event)
enqueueWriteBuffer lib cq mem blocking off size ptr = 
   enqueue (rawClEnqueueWriteBuffer lib cq mem (fromBool blocking) off size ptr)

-- | Flush commands
flush :: Library -> CommandQueue -> IO CLError
flush lib cq = fromCL <$> rawClFlush lib cq

-- | Finish commands
finish :: Library -> CommandQueue -> IO CLError
finish lib cq = fromCL <$> rawClFinish lib cq

-- | Enqueue barrier
enqueueBarrier :: Library -> CommandQueue -> IO CLError
enqueueBarrier lib cq = fromCL <$> rawClEnqueueBarrier lib cq

-- | Wait for events
waitForEvents :: Library -> [Event] -> IO CLError
waitForEvents lib evs = withArray evs $ \events -> do
   fromCL <$> rawClWaitForEvents lib (fromIntegral $ length evs) events
