{-# LANGUAGE LambdaCase #-}

module Haskus.Apps.System.Build.Utils
   ( shellIn
   , shellInErr
   , untar
   , subTitle
   , showStep
   , failWith
   , download
   , getAppDir
   , getDownloadPath
   , unlessM
   )
where

import System.Process
import System.Exit
import System.Directory
import System.FilePath
import System.IO.Temp
import qualified Network.HTTP.Client.Conduit.Download as D

-- | Execute a command in the given directory
shellIn :: FilePath -> String -> IO ExitCode 
shellIn fp cmd = do
   (_,_,_,hdl) <- createProcess ((shell cmd) { cwd = Just fp })
   waitForProcess hdl


-- | Execute a command in the given directory, call callback on error
shellInErr :: FilePath -> String -> IO () -> IO ()
shellInErr fp cmd err = do
   shellIn fp cmd >>= \case
      ExitSuccess   -> return ()
      ExitFailure _ -> err

-- | Uncompress an archive
untar :: FilePath -> FilePath -> IO ()
untar src tgt = shellInErr tgt ("tar xf " ++ src) $
   failWith "Cannot uncompress archive"

-- | Add a subline to a text
subTitle :: String -> String
subTitle t = t ++ "\n" ++ replicate (length t) '-' ++ "\n"

-- | Show progress step
showStep :: String -> IO ()
showStep t = putStrLn $ "==> " ++ t

-- | Print error message
failWith :: String -> IO a
failWith s = die $ "Error: " ++ s

-- | Download a file
download :: String -> FilePath -> IO ()
download url tgt = do
   withSystemTempDirectory "haskus-system-build" $ \fp -> do
      let fp2 = fp </> "download.tmp"
      D.download url fp2
      copyFile fp2 tgt

-- | Return app directory
getAppDir :: IO FilePath
getAppDir = do
   fp <- getAppUserDataDirectory "haskus"
   createDirectoryIfMissing True fp
   return (fp </> "system" </> "build")

-- | Return download path
getDownloadPath :: IO FilePath
getDownloadPath = do
   fp <- getAppDir
   let d = fp </> "downloads"
   createDirectoryIfMissing True d
   return d

-- | Unless with a monadic condition
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM f g = f >>= \case
   False -> g
   True  -> return ()
