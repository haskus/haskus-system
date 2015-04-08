{-# LANGUAGE DeriveDataTypeable #-}

-- | OpenCL error management module
module ViperVM.Arch.OpenCL.Error
   ( CLError(..)
   , CLRet
   , toException
   , wrapPError
   , wrapCheckSuccess
   , whenSuccess
   , wrapGetInfo
   )
where

import Control.Exception (Exception, throw)
import Data.Typeable (Typeable)
import Foreign.Ptr (Ptr,nullPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable, peek)
import Foreign.C.Types (CSize)

import ViperVM.Arch.OpenCL.Bindings
import ViperVM.Arch.OpenCL.Types

type CLRet a = IO (Either CLError a)

-- | An OpenCL error code
data CLError
   = CL_SUCCESS                                  -- 0
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

-- | Wrap an OpenCL call taking an CLint error pointer as last parameter
wrapPError :: (Ptr CLint -> IO a) -> CLRet a
wrapPError f = alloca $ \perr -> do
  v <- f perr
  errcode <- fromCL <$> peek perr
  return $ if errcode == CL_SUCCESS
    then Right v
    else Left errcode
  
-- | Return True if the given OpenCL call returns CL_SUCCESS
wrapCheckSuccess :: IO CLint -> IO Bool
wrapCheckSuccess f = (== CL_SUCCESS) . fromCL <$> f

-- | Wrap an OpenCL call to get entity info
wrapGetInfo :: Storable a => (Ptr a -> Ptr CSize -> IO CLint) -> (a -> b) -> CLRet b
wrapGetInfo fget fconvert= alloca $ \dat -> do
  errcode <- fget dat nullPtr
  if errcode == toCL CL_SUCCESS
    then Right . fconvert <$> peek dat
    else return (Left (fromCL errcode))

-- | If the OpenCL call (first parameter) is successful, evaluate and return the second parameter, otherwise return the error
whenSuccess :: IO CLint -> IO a -> IO (Either CLError a)
whenSuccess fcheck fval = do
  errcode <- fcheck
  if errcode == toCL CL_SUCCESS
    then Right <$> fval
    else return $ Left (fromCL errcode)
