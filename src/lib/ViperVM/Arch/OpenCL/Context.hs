-- | OpenCL context module
module ViperVM.Arch.OpenCL.Context (
   Context,
   createContext
) where

import ViperVM.Arch.OpenCL.Types
import ViperVM.Arch.OpenCL.Entity
import ViperVM.Arch.OpenCL.Library
import ViperVM.Arch.OpenCL.Platform
import ViperVM.Arch.OpenCL.Device
import ViperVM.Arch.OpenCL.Error
import ViperVM.Arch.OpenCL.Bindings

import Control.Applicative ((<$>))
import Control.Monad (void)
import Foreign.Ptr (nullPtr, ptrToIntPtr, nullFunPtr)
import Foreign.Marshal.Array (withArray)

-- | A context is basically a group of devices sharing entities
data Context = Context Library Context_ deriving (Eq)

instance Entity Context where 
   unwrap (Context _ x) = x
   cllib (Context l _) = l
   retain = retainContext
   release = releaseContext

-- | Context information
data CLContextInfo = 
     CL_CONTEXT_REFERENCE_COUNT
   | CL_CONTEXT_DEVICES
   | CL_CONTEXT_PROPERTIES
   | CL_CONTEXT_NUM_DEVICES
   | CL_CONTEXT_PLATFORM
   deriving (Eq,Enum)

instance CLConstant CLContextInfo where
   toCL x = fromIntegral (0x1080 + fromEnum x)
   fromCL x = toEnum (fromIntegral x - 0x1080)

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

-- | Retain a context
retainContext :: Context -> IO ()
retainContext ctx = void (rawClRetainContext (cllib ctx) (unwrap ctx))
