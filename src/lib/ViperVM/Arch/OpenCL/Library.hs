{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}

-- | OpenCL library dynamic loading
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

import ViperVM.Format.Binary.Word
import ViperVM.Arch.OpenCL.Types

-- | An OpenCL library
data Library = Library
   { libHandle :: DL

   , rawClGetPlatformIDs
      :: Word32 -> Ptr Platform_ -> Ptr Word32 -> IO Int32

   , rawClGetPlatformInfo
      :: Platform_ -> PlatformInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO Int32

   , rawClGetDeviceIDs
      :: Platform_ -> DeviceType_ -> Word32 -> Ptr Device_ -> Ptr Word32 -> IO Int32

   , rawClGetDeviceInfo
      :: Device_ -> DeviceInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO Int32

   -- Memory
   , rawClCreateBuffer
      :: Context_ -> CLMemFlags_ -> CSize -> Ptr () -> Ptr Int32 -> IO Mem_

   , rawClCreateImage2D
      :: Context_ -> CLMemFlags_ -> CLImageFormat_p -> CSize -> CSize -> CSize -> Ptr () -> Ptr Int32 -> IO Mem_

   , rawClCreateImage3D
      :: Context_ -> CLMemFlags_-> CLImageFormat_p -> CSize -> CSize -> CSize -> CSize -> CSize -> Ptr () -> Ptr Int32 -> IO Mem_

   , rawClCreateFromGLTexture2D 
      :: Context_ -> CLMemFlags_ -> Word32 -> Int32 -> Word32 -> Ptr Int32 -> IO Mem_

   , rawClCreateFromGLBuffer
      :: Context_ -> CLMemFlags_ -> Word32 -> Ptr Int32 -> IO Mem_

   , rawClRetainMemObject 
      :: Mem_ -> IO Int32

   , rawClReleaseMemObject
      :: Mem_ -> IO Int32

   , rawClGetSupportedImageFormats 
      :: Context_ -> CLMemFlags_ -> CLMemObjectType_ -> Word32 -> CLImageFormat_p -> Ptr Word32 -> IO Int32

   , rawClGetMemObjectInfo
      :: Mem_ -> CLMemInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO Int32

   , rawClGetImageInfo
      :: Mem_ -> CLImageInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO Int32

   , rawClCreateSampler
      :: Context_ -> CLbool -> CLAddressingMode_ -> CLFilterMode_ -> Ptr Int32 -> IO Sampler_

   , rawClRetainSampler
      :: Sampler_ -> IO Int32

   , rawClReleaseSampler
      :: Sampler_ -> IO Int32

   , rawClGetSamplerInfo
      :: Sampler_ -> CLSamplerInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO Int32
 
   -- Context
   , rawClCreateContext
      :: Ptr CLContextProperty_ -> Word32 -> Ptr Device_ -> FunPtr ContextCallback -> Ptr () -> Ptr Int32 -> IO Context_

   , rawClCreateContextFromType
      :: Ptr CLContextProperty_ -> DeviceType_ -> FunPtr ContextCallback -> Ptr () -> Ptr Int32 -> IO Context_

   , rawClRetainContext
      :: Context_ -> IO Int32

   , rawClReleaseContext
      :: Context_ -> IO Int32

   , rawClGetContextInfo
      :: Context_ -> CLContextInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO Int32

   -- CommandQueue
   , rawClCreateCommandQueue
      :: Context_ -> Device_ -> CLCommandQueueProperty_ -> Ptr Int32 -> IO CommandQueue_

   , rawClRetainCommandQueue
      :: CommandQueue_ -> IO Int32

   , rawClReleaseCommandQueue
      :: CommandQueue_ -> IO Int32

   , rawClGetCommandQueueInfo
      :: CommandQueue_ -> CLCommandQueueInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO Int32

   , rawClSetCommandQueueProperty
      :: CommandQueue_ -> CLCommandQueueProperty_ -> CLbool -> Ptr CLCommandQueueProperty_ -> IO Int32

   , rawClEnqueueReadBuffer
      :: CommandQueue_ -> Mem_ -> CLbool -> CSize -> CSize -> Ptr () -> Word32 -> Ptr Event_ -> Ptr Event_ -> IO Int32

   , rawClEnqueueReadBufferRect
      :: Maybe(CommandQueue_ -> Mem_ -> CLbool -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> CSize -> 
         CSize -> Ptr () -> Word32 -> Ptr Event_ -> Ptr Event_ -> IO Int32)

   , rawClEnqueueWriteBuffer
      :: CommandQueue_ -> Mem_ -> CLbool -> CSize -> CSize -> Ptr () -> Word32 -> Ptr Event_ -> Ptr Event_ -> IO Int32

   , rawClEnqueueWriteBufferRect
      :: Maybe(CommandQueue_ -> Mem_ -> CLbool -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> CSize -> 
         CSize -> Ptr () -> Word32 -> Ptr Event_ -> Ptr Event_ -> IO Int32)

   , rawClEnqueueCopyBuffer
      :: CommandQueue_ -> Mem_ -> Mem_ -> CSize -> CSize ->  CSize -> Word32 -> Ptr Event_ -> Ptr Event_ -> IO Int32

   , rawClEnqueueCopyBufferRect 
      :: Maybe(CommandQueue_ -> Mem_ -> Mem_ -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> CSize -> CSize -> 
         Word32 -> Ptr Event_ -> Ptr Event_ -> IO Int32)

   , rawClEnqueueReadImage
      :: CommandQueue_ -> Mem_ -> CLbool -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> Ptr () -> Word32 -> Ptr Event_ -> 
         Ptr Event_ -> IO Int32

   , rawClEnqueueWriteImage
      :: CommandQueue_ -> Mem_ -> CLbool -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> Ptr () -> Word32 -> Ptr Event_ -> 
         Ptr Event_ -> IO Int32

   , rawClEnqueueCopyImage
      :: CommandQueue_ -> Mem_ -> Mem_ -> Ptr CSize -> Ptr CSize -> Ptr CSize -> Word32 -> Ptr Event_ -> Ptr Event_ -> IO Int32

   , rawClEnqueueCopyImageToBuffer
      :: CommandQueue_ -> Mem_ -> Mem_ -> Ptr CSize -> Ptr CSize -> CSize -> Word32 -> Ptr Event_ -> Ptr Event_ -> IO Int32

   , rawClEnqueueCopyBufferToImage
      :: CommandQueue_ -> Mem_ -> Mem_ -> CSize -> Ptr CSize -> Ptr CSize -> Word32 -> Ptr Event_ -> Ptr Event_ -> IO Int32

   , rawClEnqueueMapBuffer
      :: CommandQueue_ -> Mem_ -> CLbool -> CLMapFlags_ -> CSize -> CSize -> Word32 -> Ptr Event_ -> Ptr Event_ -> Ptr Int32 -> IO (Ptr ())

   , rawClEnqueueMapImage
      :: CommandQueue_ -> Mem_ -> CLbool -> CLMapFlags_ -> Ptr CSize -> Ptr CSize -> Ptr CSize -> Ptr CSize -> Word32 -> Ptr Event_ -> 
         Ptr Event_ -> Ptr Int32 -> IO (Ptr ())

   , rawClEnqueueUnmapMemObject
      :: CommandQueue_ -> Mem_ -> Ptr () -> Word32 -> Ptr Event_ -> Ptr Event_ -> IO Int32

   , rawClEnqueueNDRangeKernel
      :: CommandQueue_ -> Kernel_ -> Word32 -> Ptr CSize -> Ptr CSize -> Ptr CSize -> Word32 -> Ptr Event_ -> Ptr Event_ -> IO Int32

   , rawClEnqueueNativeKernel
      :: CommandQueue_ ->  FunPtr NativeKernelCallback -> Ptr () -> CSize -> Word32 -> Ptr Mem_ -> Ptr (Ptr ()) -> Word32 -> 
         Ptr Event_ -> Ptr Event_ -> IO Int32

   , rawClEnqueueTask
      :: CommandQueue_ -> Kernel_ -> Word32 -> Ptr Event_ -> Ptr Event_ -> IO Int32

   , rawClEnqueueMarker
      :: CommandQueue_ -> Ptr Event_ -> IO Int32

   , rawClEnqueueWaitForEvents
      :: CommandQueue_ -> Word32 -> Ptr Event_ -> IO Int32

   , rawClEnqueueBarrier
      :: CommandQueue_ -> IO Int32

   , rawClFlush
      :: CommandQueue_ -> IO Int32

   , rawClFinish
      :: CommandQueue_ -> IO Int32

   -- Event
   , rawClWaitForEvents
      :: Word32 -> Ptr Event_ -> IO Int32

   , rawClGetEventInfo
      :: Event_ -> CLEventInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO Int32

   , rawClRetainEvent
      :: Event_ -> IO Int32

   , rawClReleaseEvent
      :: Event_ -> IO Int32

   , rawClGetEventProfilingInfo 
      :: Event_ -> CLProfilingInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO Int32

   -- Program_
   , rawClCreateProgramWithSource
      :: Context_ -> Word32 -> Ptr CString -> Ptr CSize -> Ptr Int32 -> IO Program_

   , rawClCreateProgramWithBinary
      :: Context_ -> Word32 -> Ptr Device_ -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr Int32 -> Ptr Int32 -> IO Program_

   , rawClRetainProgram
      :: Program_ -> IO Int32

   , rawClReleaseProgram
      :: Program_ -> IO Int32

   , rawClBuildProgram
      :: Program_ -> Word32 -> Ptr Device_ -> CString -> FunPtr BuildCallback -> Ptr () -> IO Int32

   , rawClUnloadCompiler
      :: IO Int32

   , rawClGetProgramInfo
      :: Program_ -> CLProgramInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO Int32

   , rawClGetProgramBuildInfo
      :: Program_ -> Device_ -> CLProgramBuildInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO Int32

   , rawClCreateKernel
      :: Program_ -> CString -> Ptr Int32 -> IO Kernel_

   , rawClCreateKernelsInProgram 
      :: Program_ -> Word32 -> Ptr Kernel_ -> Ptr Word32 -> IO Int32

   , rawClRetainKernel
      :: Kernel_ -> IO Int32

   , rawClReleaseKernel
      :: Kernel_ -> IO Int32

   , rawClSetKernelArg
      :: Kernel_ -> Word32 -> CSize -> Ptr () -> IO Int32

   , rawClGetKernelInfo
      :: Kernel_ -> CLKernelInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO Int32

   , rawClGetKernelWorkGroupInfo
      :: Kernel_ -> Device_ -> CLKernelWorkGroupInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO Int32

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
