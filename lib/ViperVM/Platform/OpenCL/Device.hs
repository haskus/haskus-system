{-# LANGUAGE ScopedTypeVariables #-}
module ViperVM.Platform.OpenCL.Device (
   Device(..), DeviceInfoTag(..), DeviceType(..),
   DeviceFPConfig(..), DeviceExecCapability(..),
   DeviceMemCacheType(..), DeviceLocalMemType(..),
   isDeviceLittleEndian, isDeviceLittleEndian',
   getDeviceGlobalMemSize, getDeviceGlobalMemSize',
   getDeviceType, getDeviceType',
   allDeviceTypes
) where

import ViperVM.Platform.OpenCL.Types
import ViperVM.Platform.OpenCL.Entity
import ViperVM.Platform.OpenCL.Library
import ViperVM.Platform.OpenCL.Error
import ViperVM.Platform.OpenCL.Bindings

import Data.Word (Word64)
import Control.Applicative ((<$>))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek, sizeOf)

data Device = Device Library Device_ deriving (Eq)

instance Entity Device where 
   unwrap (Device _ x) = x
   cllib (Device l _) = l

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

allDeviceTypes :: [DeviceType]
allDeviceTypes = [
      CL_DEVICE_TYPE_CPU, 
      CL_DEVICE_TYPE_GPU,
      CL_DEVICE_TYPE_ACCELERATOR,
      CL_DEVICE_TYPE_CUSTOM
   ]

data DeviceFPConfig =
     CL_FP_DENORM
   | CL_FP_INF_NAN
   | CL_FP_ROUND_TO_NEAREST
   | CL_FP_ROUND_TO_ZERO
   | CL_FP_ROUND_TO_INF
   | CL_FP_FMA
   | CL_FP_SOFT_FLOAT
   | CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT
   deriving (Show, Bounded, Eq, Ord, Enum)

instance CLSet DeviceFPConfig

data DeviceExecCapability =
     CL_EXEC_KERNEL
   | CL_EXEC_NATIVE_KERNEL
   deriving (Show, Bounded, Eq, Ord, Enum)

instance CLSet DeviceExecCapability

data DeviceMemCacheType =
     CL_NONE
   | CL_READ_ONLY_CACHE
   | CL_READ_WRITE_CACHE
   deriving (Show,Enum)

instance CLConstant DeviceMemCacheType where
   toCL x = fromIntegral (fromEnum x)
   fromCL x = toEnum (fromIntegral x)

data DeviceLocalMemType =
     CL_LOCAL
   | CL_GLOBAL
   deriving (Show,Enum)

instance CLConstant DeviceLocalMemType where
   toCL x = fromIntegral (fromEnum x + 1)
   fromCL x = toEnum (fromIntegral x - 1)


-- | Return a boolean device info
getDeviceInfoBool :: DeviceInfoTag -> Device -> IO (Either CLError Bool)
getDeviceInfoBool infoid dev = do
   let size = fromIntegral $ sizeOf (fromBool False)
   alloca $ \(dat :: Ptr CLbool) -> whenSuccess 
      (rawClGetDeviceInfo (cllib dev) (unwrap dev) (toCL infoid) size (castPtr dat) nullPtr)
      (fromCLBool <$> peek dat)

-- | Return a unsigned long device info
getDeviceInfoWord64 :: DeviceInfoTag -> Device -> IO (Either CLError Word64)
getDeviceInfoWord64 infoid dev = do
   let size = fromIntegral $ sizeOf (0 :: Word64)
   alloca $ \(dat :: Ptr Word64) -> whenSuccess 
      (rawClGetDeviceInfo (cllib dev) (unwrap dev) (toCL infoid) size (castPtr dat) nullPtr)
      (peek dat)

-- | Return OpenCL device type
getDeviceType :: Device -> IO (Either CLError [DeviceType])
getDeviceType dev = fmap fromCLSet <$> getDeviceInfoWord64 CL_DEVICE_TYPE dev

-- | Return OpenCL device type (throw an exception on error)
getDeviceType' :: Device -> IO [DeviceType]
getDeviceType' = fmap toException . getDeviceType

-- | Indicate if the device is little endian
isDeviceLittleEndian :: Device -> IO (Either CLError Bool)
isDeviceLittleEndian = getDeviceInfoBool CL_DEVICE_ENDIAN_LITTLE

-- | Indicate if the device is little endian (throw an exception on error)
isDeviceLittleEndian' :: Device -> IO Bool
isDeviceLittleEndian' = fmap toException . isDeviceLittleEndian

-- | Size of global device memory in bytes
getDeviceGlobalMemSize :: Device -> IO (Either CLError Word64)
getDeviceGlobalMemSize = getDeviceInfoWord64 CL_DEVICE_GLOBAL_MEM_SIZE

-- | Size of global device memory in bytes (throw an exception on error)
getDeviceGlobalMemSize' :: Device -> IO Word64
getDeviceGlobalMemSize' = fmap toException . getDeviceGlobalMemSize

