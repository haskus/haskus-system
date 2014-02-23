module ViperVM.Platform.OpenCL.Program (
   Program(..),
   ProgramBuildStatus(..)
) where

import ViperVM.Platform.OpenCL.Types
import ViperVM.Platform.OpenCL.Entity
import ViperVM.Platform.OpenCL.Library
import ViperVM.Platform.OpenCL.Bindings

data Program = Program Library Program_ deriving (Eq)

instance Entity Program where 
   unwrap (Program _ x) = x
   cllib (Program l _) = l

data ProgramBuildStatus = 
     CL_BUILD_SUCCESS
   | CL_BUILD_NONE
   | CL_BUILD_ERROR
   | CL_BUILD_IN_PROGRESS
   deriving (Show,Enum)

instance CLConstant ProgramBuildStatus where
   toCL x = fromIntegral (fromEnum x * (-1))
   fromCL x = toEnum (fromIntegral x * (-1))

