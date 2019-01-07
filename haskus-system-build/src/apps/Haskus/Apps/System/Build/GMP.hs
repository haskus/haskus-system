module Haskus.Apps.System.Build.GMP
   ( gmpMain
   )
where

import Haskus.Apps.System.Build.Utils
import Haskus.Utils.Flow

import System.IO.Temp
import System.Directory
import System.FilePath

gmpMain :: IO ()
gmpMain = do

   appDir <- getAppDir
   let
      usrDir  = appDir </> "usr"
      gmpVer  = "6.1.2"
      libFile = usrDir </> "lib" </> "libgmp.a"
      

   createDirectoryIfMissing True usrDir

   unlessM (doesFileExist libFile) $ do

      withSystemTempDirectory "haskus-system-build" $ \fp -> do

         showStep $ "Downloading libgmp " ++ gmpVer ++"..."
         let fp2 = fp </> "gmp.tar.lz"
         download
            ("https://gmplib.org/download/gmp/gmp-"++gmpVer++".tar.lz")
            fp2

         showStep "Unpacking libgmp..."
         untar fp2 fp

         let fp3 = fp </> ("gmp-"++gmpVer)

         showStep "Configuring libgmp..."
         shellInErr fp3 ("./configure --prefix=" ++ usrDir)
            $ failWith "Cannot configure libgmp"

         showStep "Building libgmp..."
         shellInErr fp3 "make -j8"
            $ failWith "Cannot build libgmp"

         showStep "Installing libgmp..."
         shellInErr fp3 "make install"
            $ failWith "Cannot install libgmp"

   workDir <- getWorkDir
   let libDir = workDir </> "lib"
   createDirectoryIfMissing True libDir

   unlessM (doesFileExist (libDir </> "libgmp.a")) $ do
      showStep "Copying libgmp.a into .system-work"
      copyFile libFile (libDir </> "libgmp.a")
