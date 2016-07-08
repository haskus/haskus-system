{-# LANGUAGE ScopedTypeVariables #-}
-- | OpenCL platform module
module ViperVM.Arch.OpenCL.Platform
   ( Platform
   , PlatformInfo(..)
   , PlatformInfos(..)
   , getNumPlatforms
   , getPlatforms
   , getPlatformNumDevices
   , getPlatformDevices
   , getPlatformInfo
   , getPlatformInfo'
   , getPlatformName
   , getPlatformName'
   , getPlatformVendor
   , getPlatformVendor'
   , getPlatformProfile
   , getPlatformProfile'
   , getPlatformVersion
   , getPlatformVersion'
   , getPlatformExtensions
   , getPlatformExtensions'
   , getPlatformInfos'
   , getDevicePlatform
   )
where

import qualified ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Format.Binary.Enum
import ViperVM.Format.Binary.Word
import ViperVM.Format.String (peekCString)
import ViperVM.Format.Binary.Ptr (Ptr,nullPtr,castPtr)

import ViperVM.Arch.OpenCL.Types
import ViperVM.Arch.OpenCL.Entity
import ViperVM.Arch.OpenCL.Library
import ViperVM.Arch.OpenCL.Error
import ViperVM.Arch.OpenCL.Device

import Control.Monad.Trans.Either
import Foreign.Marshal.Alloc (alloca)
import Foreign (allocaArray,peekArray)
import Foreign.Storable (peek,sizeOf)

-- | Platform
data Platform = Platform Library Platform_ deriving (Eq)

instance Entity Platform where 
   unwrap (Platform _ x) = x
   cllib (Platform l _) = l
   retain _ = return ()
   release _ = return ()

-- | Platform information
data PlatformInfo
   = CL_PLATFORM_PROFILE
   | CL_PLATFORM_VERSION
   | CL_PLATFORM_NAME
   | CL_PLATFORM_VENDOR
   | CL_PLATFORM_EXTENSIONS
   deriving (Show,Enum)

instance CEnum PlatformInfo where
   fromCEnum x = fromIntegral (0x0900 + fromEnum x)
   toCEnum x   = toEnum (fromIntegral x - 0x0900)

-- | Return the number of available platforms
getNumPlatforms :: Library -> IO Word32
getNumPlatforms lib = alloca $ \numPlatforms -> do 
   err <- rawClGetPlatformIDs lib 0 nullPtr numPlatforms
   case toCEnum err of
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
         case toCEnum err of
            CL_SUCCESS -> fmap (Platform lib) <$> peekArray (fromIntegral nplats) plats
            _ -> return []

-- | Return the number of available devices
getPlatformNumDevices :: Platform -> IO Word32
getPlatformNumDevices pf = alloca $ \numDevices -> do 
   err <- rawClGetDeviceIDs (cllib pf) (unwrap pf) (BitSet.toBits allDeviceTypes) 0 nullPtr numDevices
   case toCEnum err of
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
         err <- rawClGetDeviceIDs lib (unwrap pf) (BitSet.toBits allDeviceTypes) nbDevices devs nullPtr
         case toCEnum err of
            CL_SUCCESS -> fmap (Device lib) <$> peekArray (fromIntegral nbDevices) devs
            _ -> return []

-- | Get platform info
getPlatformInfo :: PlatformInfo -> Platform -> CLRet String
getPlatformInfo infoid pf = 
      runEitherT (getSize >>= getInfo)
   where
      lib = cllib pf

      -- Get output size
      getSize =  EitherT $ alloca $ \(sz :: Ptr CSize) -> whenSuccess 
         (rawClGetPlatformInfo lib (unwrap pf) (fromCEnum infoid) 0 nullPtr sz) 
         (peek sz)

      -- Get info
      getInfo size = EitherT $ allocaArray (fromIntegral size) $ \buff -> whenSuccess 
         (rawClGetPlatformInfo lib (unwrap pf) (fromCEnum infoid) size (castPtr buff) nullPtr)
         (peekCString buff)

-- | Get platform info (throw an exception if an error occurs)
getPlatformInfo' :: PlatformInfo -> Platform -> IO String
getPlatformInfo' infoid platform = toException <$> getPlatformInfo infoid platform

-- | Get platform name
getPlatformName :: Platform -> CLRet String
getPlatformName = getPlatformInfo CL_PLATFORM_NAME

-- | Get platform name (throw an exception if an error occurs)
getPlatformName' :: Platform -> IO String
getPlatformName' = getPlatformInfo' CL_PLATFORM_NAME

-- | Get platform vendor
getPlatformVendor :: Platform -> CLRet String
getPlatformVendor = getPlatformInfo CL_PLATFORM_VENDOR

-- | Get platform vendor (throw an exception if an error occurs)
getPlatformVendor' :: Platform -> IO String
getPlatformVendor' = getPlatformInfo' CL_PLATFORM_VENDOR

-- | Get platform profile
getPlatformProfile :: Platform -> CLRet String
getPlatformProfile = getPlatformInfo CL_PLATFORM_PROFILE

-- | Get platform profile (throw an exception if an error occurs)
getPlatformProfile' :: Platform -> IO String
getPlatformProfile' = getPlatformInfo' CL_PLATFORM_PROFILE

-- | Get platform version
getPlatformVersion :: Platform -> CLRet String
getPlatformVersion = getPlatformInfo CL_PLATFORM_VERSION

-- | Get platform version (throw an exception if an error occurs)
getPlatformVersion' :: Platform -> IO String
getPlatformVersion' = getPlatformInfo' CL_PLATFORM_VERSION

-- | Get platform extensions
getPlatformExtensions :: Platform -> CLRet [String]
getPlatformExtensions pf = fmap words <$> getPlatformInfo CL_PLATFORM_EXTENSIONS pf

-- | Get platform extensions (throw an exception if an error occurs)
getPlatformExtensions' :: Platform -> IO [String]
getPlatformExtensions' pf = words <$> getPlatformInfo' CL_PLATFORM_EXTENSIONS pf

-- | Aggregation of platform informations
data PlatformInfos = PlatformInfos
   { platformName       :: String
   , platformVendor     :: String
   , platformProfile    :: String
   , platformVersion    :: String
   , platformExtensions :: [String]
   } deriving (Show)

-- | Get platform informations (throw an exception if an error occurs)
getPlatformInfos' :: Platform -> IO PlatformInfos
getPlatformInfos' pf = PlatformInfos
   <$> getPlatformName' pf
   <*> getPlatformVendor' pf
   <*> getPlatformProfile' pf
   <*> getPlatformVersion' pf
   <*> getPlatformExtensions' pf

-- | Retrieve device platform
--
-- FIXME: it belongs to OpenCL.Device but this create a circular dependency with OpenCL.Platform
getDevicePlatform :: Device -> CLRet Platform
getDevicePlatform dev = do
   let
      sz = fromIntegral (sizeOf (undefined :: Platform_))
   alloca $ \(dat :: Ptr Platform_) -> whenSuccess 
      (rawClGetDeviceInfo (cllib dev) (unwrap dev) (fromCEnum CL_DEVICE_PLATFORM) sz (castPtr dat) nullPtr)
      (Platform (cllib dev) <$> peek dat)
