{-# LANGUAGE LambdaCase #-}

module Haskus.Apps.System.Build.Utils
   ( shellWait
   , shellWaitErr
   , shellIn
   , shellInErr
   , untar
   , subTitle
   , showStep
   , failWith
   , download
   , getAppDir
   , getWorkDir
   , getDownloadPath
   , unlessM
   , copyDirectory
   )
where

import System.Process
import System.Exit
import System.Directory
import System.FilePath
import System.IO.Temp
import qualified Network.HTTP.Client.Conduit.Download as D
import Haskus.Utils.Flow

-- | Execute a command
shellWait :: String -> IO ExitCode 
shellWait cmd = do
   (_,_,_,hdl) <- createProcess (shell cmd)
   waitForProcess hdl

-- | Execute a command, call callback on error
shellWaitErr :: String -> IO () -> IO () 
shellWaitErr cmd err = do
   shellWait cmd >>= \case
      ExitSuccess   -> return ()
      ExitFailure _ -> err

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
   let d = fp </> "system" </> "build"
   createDirectoryIfMissing True d
   return d

-- | Return work directory
getWorkDir :: IO FilePath
getWorkDir = do
   fp <- getCurrentDirectory
   let d = fp </> ".system-work"
   createDirectoryIfMissing True d
   return d

-- | Return download path
getDownloadPath :: IO FilePath
getDownloadPath = do
   fp <- getAppDir
   let d = fp </> "downloads"
   createDirectoryIfMissing True d
   return d

-- | Unless with a monadic condition
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b g = b >>= \case
   False -> g
   True  -> return ()

-- | When with a monadic condition
whenM :: Monad m => m Bool -> m () -> m ()
whenM b g = b >>= \case
   True -> g
   False  -> return ()

-- | Copy a directory (optionally keeping the structure). Use a predicate to filter
copyDirectory :: FilePath -> FilePath -> Bool -> (FilePath -> IO Bool) -> IO ()
copyDirectory src dst flattenDirs filt = go src
   where
      go currentDir = do
         fs <- listDirectory currentDir
         forM_ fs $ \f -> do
            let fileAbs = currentDir </> f
            isDir <- doesDirectoryExist fileAbs
            if isDir
               then go (currentDir </> f)
               else do
                  -- filter
                  whenM (filt fileAbs) $ do
                     let
                        fileRel = makeRelative src fileAbs
                        dstAbs  = if flattenDirs
                           then dst </> takeFileName (fileAbs)
                           else dst </> fileRel
                     createDirectoryIfMissing True (dropFileName dstAbs)
                     copyFile fileAbs dstAbs


