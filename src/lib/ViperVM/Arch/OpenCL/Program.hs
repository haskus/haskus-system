{-# LANGUAGE ScopedTypeVariables #-}

-- | OpenCL program module
module ViperVM.Arch.OpenCL.Program
   ( Program(..)
   , ProgramBuildStatus(..)
   , ProgramInfo(..)
   , buildProgram
   , getProgramDeviceCount
   , getProgramDeviceCount'
   )
where

import ViperVM.Arch.OpenCL.Types
import ViperVM.Arch.OpenCL.Entity
import ViperVM.Arch.OpenCL.Library
import ViperVM.Arch.OpenCL.Bindings
import ViperVM.Arch.OpenCL.Device
import ViperVM.Arch.OpenCL.Error

import Control.Monad (void)
import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Alloc (alloca,allocaBytes)
import Foreign.Storable (peek, sizeOf)
import Data.Word

-- | Program
data Program = Program Library Program_ deriving (Eq)

instance Entity Program where 
   unwrap (Program _ x) = x
   cllib (Program l _) = l
   retain = retainProgram
   release = releaseProgram

-- | Program build status
data ProgramBuildStatus
   = CL_BUILD_SUCCESS
   | CL_BUILD_NONE
   | CL_BUILD_ERROR
   | CL_BUILD_IN_PROGRESS
   deriving (Show,Enum)

instance CLConstant ProgramBuildStatus where
   toCL x = fromIntegral (fromEnum x * (-1))
   fromCL x = toEnum (fromIntegral x * (-1))

data ProgramInfo
   = CL_PROGRAM_REFERENCE_COUNT
   | CL_PROGRAM_CONTEXT
   | CL_PROGRAM_NUM_DEVICES
   | CL_PROGRAM_DEVICES
   | CL_PROGRAM_SOURCE
   | CL_PROGRAM_BINARY_SIZES
   | CL_PROGRAM_BINARIES
   deriving (Enum)

instance CLConstant ProgramInfo where
   toCL x = fromIntegral (0x1160 + fromEnum x)
   fromCL x = toEnum (fromIntegral x - 0x1160)

-- | Release a program
releaseProgram :: Program -> IO ()
releaseProgram prog = void (rawClReleaseProgram (cllib prog) (unwrap prog))

-- | Retain a program
retainProgram :: Program -> IO ()
retainProgram prog = void (rawClRetainProgram (cllib prog) (unwrap prog))

-- | Build a program for the specified device
buildProgram :: Program -> Device -> String -> IO (Either CLError ())
buildProgram prog dev options = do
   withCString options $ \options' ->
      withArray [(unwrap dev)] $ \dev' ->
         whenSuccess
            (rawClBuildProgram (cllib prog) (unwrap prog) 1 dev' options' nullFunPtr nullPtr)
            (return ())

-- | Return a unsigned int program info
getProgramInfoWord32 :: ProgramInfo -> Program -> IO (Either CLError Word32)
getProgramInfoWord32 infoid prog = do
   let size = fromIntegral $ sizeOf (0 :: Word32)
   alloca $ \(dat :: Ptr Word32) -> whenSuccess 
      (rawClGetProgramInfo (cllib prog) (unwrap prog) (toCL infoid) size (castPtr dat) nullPtr)
      (peek dat)

-- | Return the number of associated devices
getProgramDeviceCount :: Program -> IO (Either CLError Word32)
getProgramDeviceCount = getProgramInfoWord32 CL_PROGRAM_NUM_DEVICES

-- | Return the number of associated devices
-- throw an exception on error
getProgramDeviceCount' :: Program -> IO Word32
getProgramDeviceCount' = fmap toException . getProgramDeviceCount
