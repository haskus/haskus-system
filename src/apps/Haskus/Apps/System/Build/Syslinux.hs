{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskus.Apps.System.Build.Syslinux
   ( syslinuxMain
   , syslinuxDownloadTarball
   , syslinuxCheckTarball
   , syslinuxMakeTarballName
   , syslinuxMakeTarballPath
   , syslinuxConfigFile
   )
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import System.FilePath
import System.Directory
import System.IO.Temp

import Haskus.Apps.System.Build.Config
import Haskus.Apps.System.Build.Utils
import Haskus.Apps.System.Build.Ramdisk

-- | Download and unpack syslinux. Return its path.
syslinuxMain :: SyslinuxConfig -> IO FilePath
syslinuxMain config = do
   
   p <- getAppDir
   let
      tgtfp   = p </> "syslinux" </> Text.unpack (syslinuxVersion config)
      version = syslinuxVersion config

   -- check if we already have the built syslinux
   unlessM (doesDirectoryExist tgtfp) $ do

      -- check if we already have the tarball; otherwise download it
      syslinuxCheckTarball version >>= \case
         False -> do
            syslinuxDownloadTarball version
            -- re-check (for safety)
            syslinuxCheckTarball version >>= \case
               False -> failWith "Unable to download Syslinux"
               True  -> return ()
         True  -> return ()

      tarballPath <- syslinuxMakeTarballPath version

      -- use a temp directory close to the target one so that we can rename
      -- after unpacking
      let tmpfp = p </> "syslinux"
      createDirectoryIfMissing True tmpfp

      withTempDirectory tmpfp "download" $ \fp -> do
         -- untar
         showStep "Unpacking Syslinux archive..."
         untar tarballPath fp

         -- copy Syslinux files
         showStep "Copying Syslinux..."
         let fp2 = fp </> ("syslinux-"++Text.unpack version)
         renameDirectory fp2 tgtfp

   return tgtfp

-- | Make Syslinux archive name
syslinuxMakeTarballName :: Text -> FilePath
syslinuxMakeTarballName version = "syslinux-"++Text.unpack version++".tar.xz"

-- | Make Syslinux archive path
syslinuxMakeTarballPath :: Text -> IO FilePath
syslinuxMakeTarballPath version = do
   p <- getDownloadPath
   return (p </> syslinuxMakeTarballName version)

-- | Download Syslinux tarball from kernel.org
syslinuxDownloadTarball :: Text -> IO ()
syslinuxDownloadTarball version = do
   let
      src  = "https://cdn.kernel.org/pub/linux/utils/boot/syslinux/"
               ++ syslinuxMakeTarballName version
   
   -- download
   showStep $ "Downloading Syslinux "++Text.unpack version++"..."
   tgtDir <- getDownloadPath
   download src (tgtDir </> syslinuxMakeTarballName version)

   -- check signature
   -- TODO

-- | Check if we already have a tarball
syslinuxCheckTarball :: Text -> IO Bool
syslinuxCheckTarball version = do
   tgtDir <- getDownloadPath
   doesFileExist (tgtDir </> syslinuxMakeTarballName version)


syslinuxConfigFile :: SystemConfig -> Text -> Text -> Text
syslinuxConfigFile config ker rd = mconcat $ List.intersperse "\n"
   [ "DEFAULT main"
   , "PROMPT 0"
   , "TIMEOUT 50"
   , "UI vesamenu.c32"
   , ""
   , "LABEL main"
   , "MENU LABEL " `Text.append` ramdiskInit (ramdiskConfig (config))
   , "LINUX /" `Text.append` ker
   , "INITRD /" `Text.append` rd
   , mconcat
      [ "APPEND rdinit=\""
      , ramdiskInitPath (ramdiskConfig config)
      , "\""
      -- TODO: support custom kernel-args
      ]
   ]
