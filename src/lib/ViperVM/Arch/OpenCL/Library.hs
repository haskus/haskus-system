{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}
module ViperVM.Arch.OpenCL.Library
   ( Library(..)
   , loadOpenCL
   )
where

import Data.List (stripPrefix)
import System.Posix.DynamicLinker
import System.Posix.DynamicLinker.Template
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (Ptr)
import Foreign.C.String (CString)
import Data.Maybe (fromJust)
import Data.Word (Word8)

import ViperVM.Arch.OpenCL.Types

-- | An OpenCL library
data Library = Library
   { libHandle :: DL

   , rawClGetPlatformIDs
      :: CLuint -> Ptr Platform_ -> Ptr CLuint -> IO CLint

   , rawClGetPlatformInfo
      :: Platform_ -> PlatformInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

   , rawClGetDeviceIDs
      :: Platform_ -> DeviceType_ -> CLuint -> Ptr Device_ -> Ptr CLuint -> IO CLint

   , rawClGetDeviceInfo
      :: Device_ -> DeviceInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

   -- Memory
   , rawClCreateBuffer
      :: Context_ -> CLMemFlags_ -> CSize -> Ptr () -> Ptr CLint -> IO Mem_

   , rawClCreateImage2D
      :: Context_ -> CLMemFlags_ -> CLImageFormat_p -> CSize -> CSize -> CSize -> Ptr () -> Ptr CLint -> IO Mem_

   , rawClCreateImage3D
      :: Context_ -> CLMemFlags_-> CLImageFormat_p -> CSize -> CSize -> CSize -> CSize -> CSize -> Ptr () -> Ptr CLint -> IO Mem_

   , rawClCreateFromGLTexture2D 
      :: Context_ -> CLMemFlags_ -> CLuint -> CLint -> CLuint -> Ptr CLint -> IO Mem_

   , rawClCreateFromGLBuffer
      :: Context_ -> CLMemFlags_ -> CLuint -> Ptr CLint -> IO Mem_

   , rawClRetainMemObject 
      :: Mem_ -> IO CLint

   , rawClReleaseMemObject
      :: Mem_ -> IO CLint

   , rawClGetSupportedImageFormats 
      :: Context_ -> CLMemFlags_ -> CLMemObjectType_ -> CLuint -> CLImageFormat_p -> Ptr CLuint -> IO CLint

   , rawClGetMemObjectInfo
      :: Mem_ -> CLMemInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

   , rawClGetImageInfo
      :: Mem_ -> CLImageInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

   , rawClCreateSampler
      :: Context_ -> CLbool -> CLAddressingMode_ -> CLFilterMode_ -> Ptr CLint -> IO Sampler_

   , rawClRetainSampler
      :: Sampler_ -> IO CLint

   , rawClReleaseSampler
      :: Sampler_ -> IO CLint

   , rawClGetSamplerInfo
      :: Sampler_ -> CLSamplerInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
 
   -- Context
   , rawClCreateContext
      :: Ptr CLContextProperty_ -> CLuint -> Ptr Device_ -> FunPtr ContextCallback -> Ptr () -> Ptr CLint -> IO Context_

   , rawClCreateContextFromType
      :: Ptr CLContextProperty_ -> DeviceType_ -> FunPtr ContextCallback -> Ptr () -> Ptr CLint -> IO Context_

   , rawClRetainContext
      :: Context_ -> IO CLint

   , rawClReleaseContext
      :: Context_ -> IO CLint

   , rawClGetContextInfo
      :: Context_ -> CLContextInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

   -- CommandQueue
   , rawClCreateCommandQueue
      :: Context_ -> Device_ -> CLCommandQueueProperty_ -> Ptr CLint -> IO CommandQueue_

   , rawClRetainCommandQueue
      :: CommandQueue_ -> IO CLint

   , rawClReleaseCommandQueue
      :: CommandQueue_ -> IO CLint

   , rawClGetCommandQueueInfo
      :: CommandQueue_ -> CLCommandQueueInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

   , rawClSetCommandQueueProperty
      :: CommandQueue_ -> CLCommandQueueProperty_ -> CLbool -> Ptr CLCommandQueueProperty_ -> IO CLint

   , rawClEnqueueReadBuffer
      :: CommandQueue_ -> Mem_ -> CLbool -> CSize -> CSize -> Ptr () -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint

   , rawClEnqueueReadBufferRect
      :: Maybe(CommandQueue_ -> Mem_ -> CLbool -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> CSize -> 
         CSize -> Ptr () -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint)

   , rawClEnqueueWriteBuffer
      :: CommandQueue_ -> Mem_ -> CLbool -> CSize -> CSize -> Ptr () -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint

   , rawClEnqueueWriteBufferRect
      :: Maybe(CommandQueue_ -> Mem_ -> CLbool -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> CSize -> 
         CSize -> Ptr () -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint)

   , rawClEnqueueCopyBuffer
      :: CommandQueue_ -> Mem_ -> Mem_ -> CSize -> CSize ->  CSize -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint

   , rawClEnqueueCopyBufferRect 
      :: Maybe(CommandQueue_ -> Mem_ -> Mem_ -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> CSize -> CSize -> 
         CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint)

   , rawClEnqueueReadImage
      :: CommandQueue_ -> Mem_ -> CLbool -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> Ptr () -> CLuint -> Ptr Event_ -> 
         Ptr Event_ -> IO CLint

   , rawClEnqueueWriteImage
      :: CommandQueue_ -> Mem_ -> CLbool -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> Ptr () -> CLuint -> Ptr Event_ -> 
         Ptr Event_ -> IO CLint

   , rawClEnqueueCopyImage
      :: CommandQueue_ -> Mem_ -> Mem_ -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint

   , rawClEnqueueCopyImageToBuffer
      :: CommandQueue_ -> Mem_ -> Mem_ -> Ptr CSize -> Ptr CSize -> CSize -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint

   , rawClEnqueueCopyBufferToImage
      :: CommandQueue_ -> Mem_ -> Mem_ -> CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint

   , rawClEnqueueMapBuffer
      :: CommandQueue_ -> Mem_ -> CLbool -> CLMapFlags_ -> CSize -> CSize -> CLuint -> Ptr Event_ -> Ptr Event_ -> Ptr CLint -> IO (Ptr ())

   , rawClEnqueueMapImage
      :: CommandQueue_ -> Mem_ -> CLbool -> CLMapFlags_ -> Ptr CSize -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr Event_ -> 
         Ptr Event_ -> Ptr CLint -> IO (Ptr ())

   , rawClEnqueueUnmapMemObject
      :: CommandQueue_ -> Mem_ -> Ptr () -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint

   , rawClEnqueueNDRangeKernel
      :: CommandQueue_ -> Kernel_ -> CLuint -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint

   , rawClEnqueueNativeKernel
      :: CommandQueue_ ->  FunPtr NativeKernelCallback -> Ptr () -> CSize -> CLuint -> Ptr Mem_ -> Ptr (Ptr ()) -> CLuint -> 
         Ptr Event_ -> Ptr Event_ -> IO CLint

   , rawClEnqueueTask
      :: CommandQueue_ -> Kernel_ -> CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint

   , rawClEnqueueMarker
      :: CommandQueue_ -> Ptr Event_ -> IO CLint

   , rawClEnqueueWaitForEvents
      :: CommandQueue_ -> CLuint -> Ptr Event_ -> IO CLint

   , rawClEnqueueBarrier
      :: CommandQueue_ -> IO CLint

   , rawClFlush
      :: CommandQueue_ -> IO CLint

   , rawClFinish
      :: CommandQueue_ -> IO CLint

   -- Event
   , rawClWaitForEvents
      :: CLuint -> Ptr Event_ -> IO CLint

   , rawClGetEventInfo
      :: Event_ -> CLEventInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

   , rawClRetainEvent
      :: Event_ -> IO CLint

   , rawClReleaseEvent
      :: Event_ -> IO CLint

   , rawClGetEventProfilingInfo 
      :: Event_ -> CLProfilingInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

   -- Program_
   , rawClCreateProgramWithSource
      :: Context_ -> CLuint -> Ptr CString -> Ptr CSize -> Ptr CLint -> IO Program_

   , rawClCreateProgramWithBinary
      :: Context_ -> CLuint -> Ptr Device_ -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CLint -> Ptr CLint -> IO Program_

   , rawClRetainProgram
      :: Program_ -> IO CLint

   , rawClReleaseProgram
      :: Program_ -> IO CLint

   , rawClBuildProgram
      :: Program_ -> CLuint -> Ptr Device_ -> CString -> FunPtr BuildCallback -> Ptr () -> IO CLint

   , rawClUnloadCompiler
      :: IO CLint

   , rawClGetProgramInfo
      :: Program_ -> CLProgramInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

   , rawClGetProgramBuildInfo
      :: Program_ -> Device_ -> CLProgramBuildInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

   , rawClCreateKernel
      :: Program_ -> CString -> Ptr CLint -> IO Kernel_

   , rawClCreateKernelsInProgram 
      :: Program_ -> CLuint -> Ptr Kernel_ -> Ptr CLuint -> IO CLint

   , rawClRetainKernel
      :: Kernel_ -> IO CLint

   , rawClReleaseKernel
      :: Kernel_ -> IO CLint

   , rawClSetKernelArg
      :: Kernel_ -> CLuint -> CSize -> Ptr () -> IO CLint

   , rawClGetKernelInfo
      :: Kernel_ -> CLKernelInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

   , rawClGetKernelWorkGroupInfo
      :: Kernel_ -> Device_ -> CLKernelWorkGroupInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

   }

instance Eq Library where
  (==) a b = (==) (packDL $ libHandle a) (packDL $ libHandle b)

instance Ord Library where
  compare a b = compare (packDL $ libHandle a) (packDL $ libHandle b)



myMod :: String -> String
myMod x = fromJust $ ("c" ++) <$> stripPrefix "rawC" x

$(makeDynamicLinker ''Library CCall 'myMod)

-- | Load OpenCL library from path
loadOpenCL :: String -> IO Library
loadOpenCL lib = loadLibrary lib [RTLD_NOW,RTLD_LOCAL]
