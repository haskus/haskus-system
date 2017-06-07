{-# LANGUAGE LambdaCase #-}

module Haskus.Apps.System.Build.Linux
   ( linuxMain
   , linuxBuild
   , linuxDownloadTarball
   , linuxCheckTarball
   , linuxMakeTarballName
   , linuxMakeTarballPath
   )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad
import System.FilePath
import System.Directory
import System.IO.Temp
import Numeric (showHex)
import Data.Word

import Haskus.Apps.System.Build.Config
import Haskus.Apps.System.Build.Utils

linuxMain :: LinuxConfig -> IO ()
linuxMain config = do
   
   case linuxSource config of
      LinuxGit {}          -> do
         failWith "Building Linux from GIT is not supported for now"

      LinuxTarball version -> do
         tgtfp <- linuxMakeReleasePath config
         let tgtker = tgtfp </> "linux.img"

         -- check if we already have the built kernel
         unlessM (doesFileExist tgtker) $ do

            -- check if we already have the tarball; otherwise download it
            linuxCheckTarball version >>= \case
               False -> do
                  linuxDownloadTarball version
                  -- re-check (for safety)
                  linuxCheckTarball version >>= \case
                     False -> failWith "Unable to download Linux sources"
                     True  -> return ()
               True  -> return ()

            tarballPath <- linuxMakeTarballPath version

            withSystemTempDirectory "haskus-system-build" $ \fp -> do
               -- untar
               showStep "Uncompressing archive..."
               untar tarballPath fp
               let fp2 = fp </> ("linux-"++Text.unpack version)

               -- build
               linuxBuild config fp2

               -- copy the kernel
               showStep "Copying kernel, modules and firmwares..."
               copyFile (fp2 </> "arch" </> "x86" </> "boot" </> "bzImage") tgtker

               -- copy modules/firmwares
               -- TODO


-- | Build Linux tree in the given directory
linuxBuild :: LinuxConfig -> FilePath -> IO ()
linuxBuild config path = do
   let shell' = shellInErr path

   showStep "Configuring Linux..."

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
   showStep "Building Linux..."
   shell' ("make " ++ Text.unpack (linuxMakeArgs config))
      $ fail "Unable to build Linux"

-- | Make Linux archive name
linuxMakeTarballName :: Text -> FilePath
linuxMakeTarballName version = "linux-"++Text.unpack version++".tar.xz"

-- | Make Linux archive path
linuxMakeTarballPath :: Text -> IO FilePath
linuxMakeTarballPath version = do
   p <- getDownloadPath
   return (p </> linuxMakeTarballName version)

-- | Make Linux release path
linuxMakeReleasePath :: LinuxConfig -> IO FilePath
linuxMakeReleasePath config = do
   p <- getAppDir
   let hash = showHex (fromIntegral (linuxConfigHash config) :: Word64) ""
   let d = p </> "linux" </> hash
   createDirectoryIfMissing True d
   return d


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


