{-# LANGUAGE LambdaCase #-}

module Haskus.Apps.System.Build.Linux
   ( linuxBuild
   , linuxDownloadTarball
   , linuxCheckTarball
   )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad
import System.FilePath
import System.Directory

import Haskus.Apps.System.Build.Config
import Haskus.Apps.System.Build.Utils

-- | Build Linux tree in the given directory
linuxBuild :: LinuxConfig -> FilePath -> IO ()
linuxBuild config path = do
   let shell' = shellInErr path

   -- make default configuration for the arch
   shell' "make x86_64_defconfig"
      $ fail "Unable to build Linux default configuration"

   -- enable/disable/module options
   let opts = linuxOptions config
   forM_ (enableOptions opts) $ \opt ->
      shell' ("./scripts/config -e "++ Text.unpack opt)
         $ fail $ "Unable to enable Linux option: " ++ Text.unpack opt
   forM_ (disableOptions opts) $ \opt ->
      shell' ("./scripts/config -d "++ Text.unpack opt)
         $ fail $ "Unable to disable Linux option: " ++ Text.unpack opt
   forM_ (moduleOptions opts) $ \opt ->
      shell' ("./scripts/config -m "++ Text.unpack opt)
         $ fail $ "Unable to modularize Linux option: " ++ Text.unpack opt

   -- fixup config (interactive)
   shell' "make oldconfig"
      $ fail "Unable to adjust Linux configuration"

   -- build
   shell' ("make " ++ Text.unpack (linuxMakeArgs config))
      $ fail "Unable to build Linux"

-- | Make Linux archive name
linuxMakeTarballName :: Text -> FilePath
linuxMakeTarballName version = "linux-"++Text.unpack version++".tar.xz"


-- | Download a Linux tarball from kernel.org
linuxDownloadTarball :: Text -> IO ()
linuxDownloadTarball version = do
   let
      src  = "https://cdn.kernel.org/pub/linux/kernel/v"
               ++ Text.unpack (head (Text.splitOn (Text.pack ".") version))
               ++ ".x/linux-"
               ++ Text.unpack version
               ++ ".tar.xz"
   
   -- download
   showStep $ "Downloading Linux "++Text.unpack version++"..."
   tgtDir <- getDownloadPath
   download src (tgtDir </> linuxMakeTarballName version)

   -- check signature
   -- TODO

-- | Check if we already have a tarball
linuxCheckTarball :: Text -> IO Bool
linuxCheckTarball version = do
   tgtDir <- getDownloadPath
   doesFileExist (tgtDir </> linuxMakeTarballName version)
