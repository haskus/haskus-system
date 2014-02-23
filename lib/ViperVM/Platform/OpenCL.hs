{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module ViperVM.Platform.OpenCL (
   PlatformInfo(..),
   -- Platform
   getNumPlatforms, getPlatforms, 
   getPlatformNumDevices, getPlatformDevices, 
   getPlatformName, getPlatformName', 
   getPlatformVendor, getPlatformVendor',
   getPlatformProfile, getPlatformProfile',
   getPlatformVersion, getPlatformVersion',
   getPlatformExtensions, getPlatformExtensions',
   getPlatformInfos',
   -- Device
   isDeviceLittleEndian, isDeviceLittleEndian',
   getDeviceGlobalMemSize, getDeviceGlobalMemSize',
   getDeviceType, getDeviceType',
   -- Contexts
   createContext, releaseContext,
   -- Buffers
   createBuffer, releaseBuffer, retainBuffer,
   -- Images
   createImage2D,createImage3D,
   -- Transfers
   enqueueReadBuffer, enqueueWriteBuffer, enqueueCopyBuffer,
   -- Command queue
   createCommandQueue, releaseCommandQueue, retainCommandQueue,
   flush, finish, enqueueBarrier,
   waitForEvents,
   module X
) where

import Control.Applicative
import Control.Monad (void)
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign (allocaArray,peekArray, pokeArray)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr
import Foreign.Storable

import ViperVM.Platform.OpenCL.Types as X
import ViperVM.Platform.OpenCL.Error as X
import ViperVM.Platform.OpenCL.Library as X
import ViperVM.Platform.OpenCL.Bindings


withMaybeArray :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withMaybeArray [] = ($ nullPtr)
withMaybeArray xs = withArray xs

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
            CL_SUCCESS -> fmap (Platform lib) <$> peekArray (fromIntegral nplats) plats
            _ -> return []

-- | Return the number of available devices
getPlatformNumDevices :: Platform -> IO Word32
getPlatformNumDevices pf = alloca $ \numDevices -> do 
   err <- rawClGetDeviceIDs (cllib pf) (unwrap pf) (toCLSet clDeviceTypeAll) 0 nullPtr numDevices
   case fromCL err of
      CL_SUCCESS -> peek numDevices
      _ -> return 0

-- | Get available platform devices
getPlatformDevices :: Platform -> IO [Device]
getPlatformDevices pf = do
   let lib = cllib pf
   nbDevices <- getPlatformNumDevices pf
   if nbDevices == 0
      then return []
      else allocaArray (fromIntegral nbDevices) $ \devs -> do
         err <- rawClGetDeviceIDs lib (unwrap pf) (toCLSet clDeviceTypeAll) nbDevices devs nullPtr
         case fromCL err of
            CL_SUCCESS -> fmap (Device lib) <$> peekArray (fromIntegral nbDevices) devs
            _ -> return []

-- | Get platform info
getPlatformInfo :: PlatformInfoTag -> Platform -> IO (Either CLError String)
getPlatformInfo infoid pf = getSize >>>= getInfo
   where
      lib = cllib pf
      (>>>=) f g = do
         t <- f
         case t of
            Left a -> return (Left a)
            Right a -> g a

      -- Get output size
      getSize :: IO (Either CLError CSize)
      getSize = alloca $ \sz -> whenSuccess 
         (rawClGetPlatformInfo lib (unwrap pf) (toCL infoid) 0 nullPtr sz) 
         (peek sz)

      -- Get info
      getInfo :: CSize -> IO (Either CLError String)
      getInfo size = allocaArray (fromIntegral size) $ \buff -> whenSuccess 
         (rawClGetPlatformInfo lib (unwrap pf) (toCL infoid) size (castPtr buff) nullPtr)
         (peekCString buff)

-- | Get platform info (throw an exception if an error occurs)
getPlatformInfo' :: PlatformInfoTag -> Platform -> IO String
getPlatformInfo' infoid platform = toException <$> getPlatformInfo infoid platform

-- | Get platform name
getPlatformName :: Platform -> IO (Either CLError String)
getPlatformName = getPlatformInfo CL_PLATFORM_NAME

-- | Get platform name (throw an exception if an error occurs)
getPlatformName' :: Platform -> IO String
getPlatformName' = getPlatformInfo' CL_PLATFORM_NAME

-- | Get platform vendor
getPlatformVendor :: Platform -> IO (Either CLError String)
getPlatformVendor = getPlatformInfo CL_PLATFORM_VENDOR

-- | Get platform vendor (throw an exception if an error occurs)
getPlatformVendor' :: Platform -> IO String
getPlatformVendor' = getPlatformInfo' CL_PLATFORM_VENDOR

-- | Get platform profile
getPlatformProfile :: Platform -> IO (Either CLError String)
getPlatformProfile = getPlatformInfo CL_PLATFORM_PROFILE

-- | Get platform profile (throw an exception if an error occurs)
getPlatformProfile' :: Platform -> IO String
getPlatformProfile' = getPlatformInfo' CL_PLATFORM_PROFILE

-- | Get platform version
getPlatformVersion :: Platform -> IO (Either CLError String)
getPlatformVersion = getPlatformInfo CL_PLATFORM_VERSION

-- | Get platform version (throw an exception if an error occurs)
getPlatformVersion' :: Platform -> IO String
getPlatformVersion' = getPlatformInfo' CL_PLATFORM_VERSION

-- | Get platform extensions
getPlatformExtensions :: Platform -> IO (Either CLError [String])
getPlatformExtensions pf = fmap words <$> getPlatformInfo CL_PLATFORM_EXTENSIONS pf

-- | Get platform extensions (throw an exception if an error occurs)
getPlatformExtensions' :: Platform -> IO [String]
getPlatformExtensions' pf = words <$> getPlatformInfo' CL_PLATFORM_EXTENSIONS pf

data PlatformInfo = PlatformInfo {
   platformName :: String,
   platformVendor :: String,
   platformProfile :: String,
   platformVersion :: String,
   platformExtensions :: [String]
} deriving (Show)

-- | Get platform informations (throw an exception if an error occurs)
getPlatformInfos' :: Platform -> IO PlatformInfo
getPlatformInfos' pf = PlatformInfo
   <$> getPlatformName' pf
   <*> getPlatformVendor' pf
   <*> getPlatformProfile' pf
   <*> getPlatformVersion' pf
   <*> getPlatformExtensions' pf
 
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

-- | Create a context
createContext :: Platform -> [Device] -> IO (Either CLError Context)
createContext pf devs = do
   let props = [toCL CL_CONTEXT_PLATFORM, ptrToIntPtr (unwrap pf), 0]
       ndevs = fromIntegral (length devs)
       lib = cllib pf
   withArray (fmap unwrap devs) $ \devs' ->
      withArray props $ \props' ->
         fmap (Context lib) <$> wrapPError (rawClCreateContext lib props' ndevs devs' nullFunPtr nullPtr)

-- | Release a context
releaseContext :: Context -> IO ()
releaseContext ctx = void (rawClReleaseContext (cllib ctx) (unwrap ctx))

-- | Create a buffer
createBuffer :: Device -> Context -> [CLMemFlag] -> CSize -> IO (Either CLError Mem)
createBuffer _ ctx flags size = do
   let lib = cllib ctx
   mem <- fmap (Mem lib) <$> wrapPError (rawClCreateBuffer lib (unwrap ctx) (toCLSet flags) size nullPtr)
   --FIXME: ensure buffer is allocated 
   --  use clEnqueueMigrateMemObjects if available (OpenCL 1.1 or 1.2?)
   --  perform a dummy operation on the buffer (OpenCL 1.0)
   return mem

-- | Create 2D image
createImage2D :: Context -> [CLMemFlag] -> CLImageFormat_p -> CSize -> CSize -> CSize -> Ptr () -> IO (Either CLError Mem)
createImage2D ctx flags imgFormat width height rowPitch hostPtr =
   fmap (Mem lib) <$> wrapPError (rawClCreateImage2D lib (unwrap ctx) (toCLSet flags) imgFormat width height rowPitch hostPtr)
   where lib = cllib ctx

-- | Create 3D image
createImage3D :: Context -> [CLMemFlag] -> CLImageFormat_p -> CSize -> CSize -> CSize -> CSize -> CSize -> Ptr () -> IO (Either CLError Mem)
createImage3D ctx flags imgFormat width height depth rowPitch slicePitch hostPtr =
   fmap (Mem lib) <$> wrapPError (rawClCreateImage3D lib (unwrap ctx) (toCLSet flags) imgFormat width height depth rowPitch slicePitch hostPtr)
   where lib = cllib ctx

-- | Release a buffer
releaseBuffer :: Mem -> IO ()
releaseBuffer mem = void (rawClReleaseMemObject (cllib mem) (unwrap mem))

-- | Retain a buffer
retainBuffer :: Mem -> IO ()
retainBuffer mem = void (rawClRetainMemObject (cllib mem) (unwrap mem))

-- | Create a command queue
createCommandQueue :: Context -> Device -> [CommandQueueProperty] -> IO (Either CLError CommandQueue)
createCommandQueue ctx dev props =
   fmap (CommandQueue lib) <$> wrapPError (rawClCreateCommandQueue lib (unwrap ctx) (unwrap dev) (toCLSet props))
   where lib = cllib ctx

-- | Release a command queue
releaseCommandQueue :: CommandQueue -> IO ()
releaseCommandQueue cq = void (rawClReleaseCommandQueue (cllib cq) (unwrap cq))

-- | Retain a command queue
retainCommandQueue :: CommandQueue -> IO ()
retainCommandQueue cq = void (rawClRetainCommandQueue (cllib cq) (unwrap cq))

-- | Helper function to enqueue commands
enqueue :: Library -> (CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint) -> [Event] -> IO (Either CLError Event)
enqueue lib f [] = alloca $ \event -> whenSuccess (f 0 nullPtr event) (Event lib <$> peek event)
enqueue lib f events = allocaArray nevents $ \pevents -> do
  pokeArray pevents (fmap unwrap events)
  alloca $ \event -> whenSuccess (f cnevents pevents event) (Event lib <$> peek event)
    where
      nevents = length events
      cnevents = fromIntegral nevents

-- | Transfer from device to host
enqueueReadBuffer :: CommandQueue -> Mem -> Bool -> CSize -> CSize -> Ptr () -> [Event] -> IO (Either CLError Event)
enqueueReadBuffer cq mem blocking off size ptr = 
   enqueue lib (rawClEnqueueReadBuffer lib (unwrap cq) (unwrap mem) (fromBool blocking) off size ptr)
   where lib = cllib cq

-- | Transfer from host to device
enqueueWriteBuffer :: CommandQueue -> Mem -> Bool -> CSize -> CSize -> Ptr () -> [Event] -> IO (Either CLError Event)
enqueueWriteBuffer cq mem blocking off size ptr = 
   enqueue lib (rawClEnqueueWriteBuffer lib (unwrap cq) (unwrap mem) (fromBool blocking) off size ptr)
   where lib = cllib cq

-- | Flush commands
flush :: CommandQueue -> IO CLError
flush cq = fromCL <$> rawClFlush (cllib cq) (unwrap cq)

-- | Finish commands
finish :: CommandQueue -> IO CLError
finish cq = fromCL <$> rawClFinish (cllib cq) (unwrap cq)

-- | Enqueue barrier
enqueueBarrier :: CommandQueue -> IO CLError
enqueueBarrier cq = fromCL <$> rawClEnqueueBarrier (cllib cq) (unwrap cq)

-- | Copy from one buffer to another
enqueueCopyBuffer :: CommandQueue -> Mem -> Mem -> CSize -> CSize -> CSize -> [Event] -> IO (Either CLError Event)
enqueueCopyBuffer cq src dst srcOffset dstOffset sz =
   enqueue lib (rawClEnqueueCopyBuffer lib (unwrap cq) (unwrap src) (unwrap dst) srcOffset dstOffset sz)
   where lib = cllib cq

-- | Wait for events
waitForEvents :: [Event] -> IO CLError
waitForEvents [] = return CL_SUCCESS
waitForEvents evs@(e:_) = let lib = cllib e in
   withArray (fmap unwrap evs) $ \events ->
      fromCL <$> rawClWaitForEvents lib (fromIntegral $ length evs) events
