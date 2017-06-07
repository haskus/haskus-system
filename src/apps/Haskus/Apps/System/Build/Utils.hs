{-# LANGUAGE LambdaCase #-}

module Haskus.Apps.System.Build.Utils
   ( shellIn
   , shellInErr
   , subTitle
   , showStep
   , failWith
   )
where

import System.Process
import System.Exit

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

-- | Add a subline to a text
subTitle :: String -> String
subTitle t = t ++ "\n" ++ replicate (length t) '-' ++ "\n"

-- | Show progress step
showStep :: String -> IO ()
showStep t = putStrLn $ "==> " ++ t

-- | Print error message
failWith :: String -> IO a
failWith s = die $ "Error: " ++ s
