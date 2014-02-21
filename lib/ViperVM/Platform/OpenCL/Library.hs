{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}
module ViperVM.Platform.OpenCL.Library (
   Library(..), loadOpenCL
) where

import Data.List (stripPrefix)
import System.Posix.DynamicLinker
import System.Posix.DynamicLinker.Template
import Foreign.Ptr (Ptr)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize(..))
import Control.Applicative ((<$>))
import Data.Word (Word8)
import Data.Maybe (fromJust)

import ViperVM.Platform.OpenCL.Types

-- | An OpenCL library
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
