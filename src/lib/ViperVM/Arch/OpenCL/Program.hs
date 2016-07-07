{-# LANGUAGE ScopedTypeVariables #-}

-- | OpenCL program module
module ViperVM.Arch.OpenCL.Program
   ( Program(..)
   , ProgramBuildStatus(..)
   , ProgramInfo(..)
   , ProgramBuildInfo(..)
   , buildProgram
   , getProgramDeviceCount
   , getProgramDeviceCount'
   , getProgramDevices
   , getProgramDevices'
   , getProgramBinarySizes
   , getProgramBinarySizes'
   , getProgramBinary
   , getProgramBinary'
   , createProgramFromSource
   , createProgramFromBinary
   , getProgramBuildLog
   )
where

import ViperVM.Arch.OpenCL.Types
import ViperVM.Arch.OpenCL.Entity
import ViperVM.Arch.OpenCL.Library
import ViperVM.Arch.OpenCL.Device
import ViperVM.Arch.OpenCL.Error
import ViperVM.Arch.OpenCL.Context
import ViperVM.Format.Binary.Enum
import ViperVM.Format.Binary.Word

import Control.Monad (void)
import Control.Monad.Trans.Either
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types (CSize, CChar)
import Foreign.Marshal.Array (withArray, allocaArray, peekArray)
import Foreign.Marshal.Alloc (alloca,allocaBytes)
import Foreign.Marshal.Utils (withMany)
import Foreign.Storable (peek, sizeOf)
import Data.List (elemIndex)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

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

instance CEnum ProgramBuildStatus where
   fromCEnum x = fromIntegral (fromEnum x * (-1))
   toCEnum x   = toEnum (fromIntegral x * (-1))

-- | Program info tag
data ProgramInfo
   = CL_PROGRAM_REFERENCE_COUNT
   | CL_PROGRAM_CONTEXT
   | CL_PROGRAM_NUM_DEVICES
   | CL_PROGRAM_DEVICES
   | CL_PROGRAM_SOURCE
   | CL_PROGRAM_BINARY_SIZES
   | CL_PROGRAM_BINARIES
   deriving (Show,Eq,Enum)

instance CEnum ProgramInfo where
   fromCEnum x = fromIntegral (0x1160 + fromEnum x)
   toCEnum x   = toEnum (fromIntegral x - 0x1160)

-- | Program build info tag
data ProgramBuildInfo
   = CL_PROGRAM_BUILD_STATUS
   | CL_PROGRAM_BUILD_OPTIONS
   | CL_PROGRAM_BUILD_LOG
   deriving (Eq,Show, Enum)

instance CEnum ProgramBuildInfo where
   fromCEnum x = fromIntegral (0x1181 + fromEnum x)
   toCEnum x   = toEnum (fromIntegral x - 0x1181)

-- | Release a program
releaseProgram :: Program -> IO ()
releaseProgram prog = void (rawClReleaseProgram (cllib prog) (unwrap prog))

-- | Retain a program
retainProgram :: Program -> IO ()
retainProgram prog = void (rawClRetainProgram (cllib prog) (unwrap prog))

-- | Build a program for the specified device
buildProgram :: Program -> Device -> String -> CLRet ()
buildProgram prog dev options =
   withCString options $ \options' ->
      withArray [unwrap dev] $ \dev' ->
         whenSuccess
            (rawClBuildProgram (cllib prog) (unwrap prog) 1 dev' options' nullFunPtr nullPtr)
            (return ())

-- | Return a unsigned int program info
getProgramInfoWord32 :: ProgramInfo -> Program -> CLRet Word32
getProgramInfoWord32 infoid prog = do
   let size = fromIntegral $ sizeOf (0 :: Word32)
   alloca $ \(dat :: Ptr Word32) -> whenSuccess 
      (rawClGetProgramInfo (cllib prog) (unwrap prog) (fromCEnum infoid) size (castPtr dat) nullPtr)
      (peek dat)

-- | Return the number of associated devices
getProgramDeviceCount :: Program -> CLRet Word32
getProgramDeviceCount = getProgramInfoWord32 CL_PROGRAM_NUM_DEVICES

-- | Return the number of associated devices
-- throw an exception on error
getProgramDeviceCount' :: Program -> IO Word32
getProgramDeviceCount' = fmap toException . getProgramDeviceCount

-- | Get devices associated with the program
getProgramDevices :: Program -> CLRet [Device]
getProgramDevices prog = runEitherT $ do
   count <- EitherT $ fmap fromIntegral <$> getProgramDeviceCount prog
   
   let 
      size = fromIntegral $ count * sizeOf (undefined :: Device_)
      infoid = CL_PROGRAM_DEVICES

   devs <- EitherT $ allocaArray count $ \(dat :: Ptr Device_) -> whenSuccess 
      (rawClGetProgramInfo (cllib prog) (unwrap prog) (fromCEnum infoid) size (castPtr dat) nullPtr)
      (peekArray count dat)

   return $ fmap (Device (cllib prog)) devs

-- | Get devices associated with the program
-- throw an exception on failure
getProgramDevices' :: Program -> IO [Device]
getProgramDevices' = fmap toException . getProgramDevices


-- | Get binary sizes for all devices (0 if not available)
getProgramBinarySizes :: Program -> CLRet [CSize]
getProgramBinarySizes prog = runEitherT $ do
   count <- EitherT $ fmap fromIntegral <$> getProgramDeviceCount prog

   let 
      size = fromIntegral $ count * sizeOf (undefined :: CSize)
      infoid = CL_PROGRAM_BINARY_SIZES

   EitherT $ allocaArray count $ \(dat :: Ptr CSize) -> whenSuccess 
      (rawClGetProgramInfo (cllib prog) (unwrap prog) (fromCEnum infoid) size (castPtr dat) nullPtr)
      (peekArray count dat)

-- | Get binary sizes for all devices (0 if not available)
-- throw an exception on failure
getProgramBinarySizes' :: Program -> IO [CSize]
getProgramBinarySizes' = fmap toException . getProgramBinarySizes


-- | Get the binary associated to the device (if any)
getProgramBinary :: Program -> Device -> CLRet (Maybe BS.ByteString)
getProgramBinary prog dev = runEitherT $ do

   devs <- EitherT $ getProgramDevices prog

   case elemIndex dev devs of
      Nothing  -> right Nothing
      Just idx -> do

         -- get binary size
         sizes <- EitherT $ getProgramBinarySizes prog
         let binsize = fromIntegral $ sizes !! idx
             ndev    = length devs
             size    = fromIntegral $ ndev * sizeOf (undefined :: Ptr ())
             infoid  = CL_PROGRAM_BINARIES

         case binsize of
            0 -> right Nothing
            _ -> EitherT $
               -- alloc buffer for the binary
               allocaBytes binsize $ \(binPtr :: Ptr Word8) -> do

                  -- create list of pointers: binPtr for our device, NULL otherwise
                  let f x | unwrap x == unwrap dev = binPtr
                          | otherwise              = nullPtr

                  withArray (map f devs) $ \ptrs ->
                     -- get the binary
                     whenSuccess 
                        (rawClGetProgramInfo (cllib prog) (unwrap prog) 
                           (fromCEnum infoid) size (castPtr ptrs) nullPtr)
                        (Just . BS.pack <$> peekArray binsize binPtr)
   

-- | Get the binary associated to the device (if any)
-- throw an exception on failure
getProgramBinary' :: Program -> Device -> IO (Maybe BS.ByteString)
getProgramBinary' prog = fmap toException . getProgramBinary prog


-- | Create a program from its source
-- TODO: provide a Text alternative
createProgramFromSource :: Context -> String -> CLRet Program
createProgramFromSource ctx src =
   withCString src $ \src' ->
      withArray [src'] $ \strings -> do
         p <- wrapPError (rawClCreateProgramWithSource (cllib ctx) (unwrap ctx) 1 strings nullPtr)
         return (Program (cllib ctx) <$> p)

-- | Create a program from a binary
--
-- Each device has an associated binary and returns a specific status
createProgramFromBinary :: Context -> [(Device, BS.ByteString)] -> CLRet (Program, [CLError])
createProgramFromBinary ctx binaries = do
   let 
      devs = map (unwrap . fst) binaries
      bins = map snd binaries
      lens = map (fromIntegral . BS.length) bins :: [CSize]
      n    = length devs

   withArray devs $ \devs' ->
      withArray lens $ \lens' ->
         -- Convert [ByteString] into [CString]
         withMany BS.unsafeUseAsCString bins $ \bins' ->
            -- Convert [CString] into [Ptr Word8] into Ptr CString
            withArray (map castPtr bins') $ \bins'' ->
               allocaArray n $ \(status' :: Ptr Int32) -> do
                  p' <- wrapPError (rawClCreateProgramWithBinary (cllib ctx) (unwrap ctx) 
                                  (fromIntegral n) devs' lens' bins'' status')
                  -- if there is no error, get status for each binary
                  case Program (cllib ctx) <$> p' of
                     Left err -> return (Left err)
                     Right p  -> do
                        status <- fmap toCEnum <$> peekArray n status'
                        return (Right (p,status))

-- | Return program build log for the specified device
getProgramBuildLog :: Program -> Device -> CLRet String
getProgramBuildLog prog dev = do
   let call = rawClGetProgramBuildInfo (cllib prog) (unwrap prog) (unwrap dev)
                  (fromCEnum CL_PROGRAM_BUILD_LOG) 

   runEitherT $ do
      -- Get log size
      size <- EitherT $ alloca $ \(sz :: Ptr CSize) -> whenSuccess 
         (call 0 nullPtr sz)
         (peek sz)

      let intSize = fromIntegral size

      -- Get log
      EitherT $ allocaArray intSize $ \(s :: Ptr CChar) -> whenSuccess 
         (call size (castPtr s) nullPtr)
         (peekCStringLen (s,intSize-1))
