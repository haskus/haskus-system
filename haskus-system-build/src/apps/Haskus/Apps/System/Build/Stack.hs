{-# LANGUAGE LambdaCase #-}

module Haskus.Apps.System.Build.Stack
   ( stackGetBinPath
   , stackGetGHCVersion
   , stackBuild
   )
where

import System.Process
import System.FilePath

import Haskus.Apps.System.Build.Utils
import Haskus.Utils.Flow

-- | Get GHC version (using stack exec)
stackGetGHCVersion :: IO String
stackGetGHCVersion =
   -- FIXME
   last . words <$> readProcess "stack" ["exec", "--", "ghc", "--version"] ""

stackGetBinPath :: FilePath -> IO FilePath
stackGetBinPath x = do
   p <- readProcess "stack" ["path", "--local-install-root"] ""
   return $ init p </> "bin" </> x


stackBuild :: IO ()
stackBuild = do
   showStep "Configuring Stack..."
   shellWaitErr "stack setup"
      <| failWith "Error during `stack setup`"

   showStep "Building with Stack..."
   shellWaitErr "stack build"
      <| failWith "Error during `stack build`"

