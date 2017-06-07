{-# LANGUAGE LambdaCase #-}

module Haskus.Apps.System.Build.Stack
   ( stackGetBinPath
   , stackGetResolver
   , stackGetGHCVersion
   , stackBuild
   )
where

import System.Process
import System.FilePath
import Data.List

import Haskus.Apps.System.Build.Utils
import Haskus.Utils.Flow

-- | Get GHC version (using stack exec)
stackGetGHCVersion :: IO String
stackGetGHCVersion =
   -- FIXME
   last . words <$> readProcess "stack" ["exec", "--", "ghc", "--version"] ""

-- | Get stack resolver
stackGetResolver :: IO String
stackGetResolver =
   -- FIXME
   last . words . head . filter ("resolver:" `isPrefixOf`) . lines <$> readFile "stack.yaml"

stackGetBinPath :: FilePath -> IO FilePath
stackGetBinPath x = do
   -- read GHC version
   ghcVersion <- stackGetGHCVersion

   -- read stack resolver
   stackResolver <- stackGetResolver

   return $ ".stack-work/install/x86_64-linux"
      </> stackResolver
      </> ghcVersion
      </> "bin"
      </> x


stackBuild :: IO ()
stackBuild = do
   showStep "Configuring Stack..."
   shellWaitErr "stack setup"
      <| failWith "Error during `stack setup`"

   showStep "Building with Stack..."
   shellWaitErr "stack build"
      <| failWith "Error during `stack build`"

