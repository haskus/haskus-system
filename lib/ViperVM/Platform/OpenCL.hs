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
   -- Contexts
   createContext, releaseContext,
   -- Buffers
   createBuffer, releaseBuffer,
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
 
-- | Return a boolean device info
getDeviceInfoBool :: Library -> DeviceInfoTag -> Device -> IO (Either CLError Bool)
getDeviceInfoBool lib infoid dev = do
   let size = fromIntegral $ sizeOf (fromBool False)
   alloca $ \(dat :: Ptr CLbool) -> whenSuccess 
      (rawClGetDeviceInfo lib dev (toCL infoid) size (castPtr dat) nullPtr)
      (fromCLBool <$> peek dat)

-- | Indicate if the device is little endian
isDeviceLittleEndian :: Library -> Device -> IO (Either CLError Bool)
isDeviceLittleEndian lib dev = getDeviceInfoBool lib CL_DEVICE_ENDIAN_LITTLE dev

-- | Indicate if the device is little endian (throw an exception on error)
isDeviceLittleEndian' :: Library -> Device -> IO Bool
isDeviceLittleEndian' lib dev = toException <$> isDeviceLittleEndian lib dev

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

-- | Retain a command queue
retainCommandQueue :: Library -> CommandQueue -> IO ()
retainCommandQueue lib cq = void (rawClRetainCommandQueue lib cq)

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

-- | Copy from one buffer to another
enqueueCopyBuffer :: Library -> CommandQueue -> Mem -> Mem -> CSize -> CSize -> CSize -> [Event] -> IO (Either CLError Event)
enqueueCopyBuffer lib cq src dst srcOffset dstOffset sz =
   enqueue (rawClEnqueueCopyBuffer lib cq src dst srcOffset dstOffset sz)

-- | Wait for events
waitForEvents :: Library -> [Event] -> IO CLError
waitForEvents lib evs = withArray evs $ \events -> do
   fromCL <$> rawClWaitForEvents lib (fromIntegral $ length evs) events
