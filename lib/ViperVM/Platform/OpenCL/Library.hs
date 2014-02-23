{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}
module ViperVM.Platform.OpenCL.Library (
   Library(..), loadOpenCL
) where

import Data.List (stripPrefix)
import System.Posix.DynamicLinker
import System.Posix.DynamicLinker.Template
import Foreign.C.Types (CSize(..))
import Control.Applicative ((<$>))
import Data.Maybe (fromJust)

import ViperVM.Platform.OpenCL.Types

myMod :: String -> String
myMod x = fromJust $ ("c" ++) <$> stripPrefix "rawC" x

$(makeDynamicLinker ''Library CCall 'myMod)

loadOpenCL :: String -> IO Library
loadOpenCL lib = loadLibrary lib [RTLD_NOW,RTLD_LOCAL]
