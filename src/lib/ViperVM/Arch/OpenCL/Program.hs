-- | OpenCL program module
module ViperVM.Arch.OpenCL.Program
   ( Program(..)
   , ProgramBuildStatus(..)
   , buildProgram
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
