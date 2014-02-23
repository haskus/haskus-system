module ViperVM.Platform.OpenCL.Platform (
   Platform, PlatformInfo(..),
   getNumPlatforms, getPlatforms, 
   getPlatformNumDevices, getPlatformDevices,
   getPlatformInfo, getPlatformInfo',
   getPlatformName, getPlatformName',
   getPlatformVendor, getPlatformVendor',
   getPlatformProfile, getPlatformProfile',
   getPlatformVersion, getPlatformVersion',
   getPlatformExtensions, getPlatformExtensions',
   getPlatformInfos',
) where

import ViperVM.Platform.OpenCL.Types
import ViperVM.Platform.OpenCL.Entity
import ViperVM.Platform.OpenCL.Library
import ViperVM.Platform.OpenCL.Error
import ViperVM.Platform.OpenCL.Device
import ViperVM.Platform.OpenCL.Bindings

import Foreign.C.Types (CSize)
import Foreign.C.String (peekCString)
import Data.Word (Word32)
import Control.Applicative ((<$>), (<*>))
import Foreign.Marshal.Alloc (alloca)
import Foreign (allocaArray,peekArray)
import Foreign.Storable (peek)
import Foreign.Ptr (nullPtr,castPtr)

data Platform = Platform Library Platform_ deriving (Eq)

instance Entity Platform where 
   unwrap (Platform _ x) = x
   cllib (Platform l _) = l

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
   err <- rawClGetDeviceIDs (cllib pf) (unwrap pf) (toCLSet allDeviceTypes) 0 nullPtr numDevices
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
         err <- rawClGetDeviceIDs lib (unwrap pf) (toCLSet allDeviceTypes) nbDevices devs nullPtr
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
