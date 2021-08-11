{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import System.Posix.Process
import Haskus.System.Linux.Trace
import Haskus.System.Linux.Process
import Haskus.Utils.Variant.Excepts
import System.Exit
import System.Environment

main :: IO ()
main = do

  args <- getArgs
  case args of
    [fp] -> traceProcess fp
    _    -> do
      putStrLn "Usage: haskus-tracer COMMAND"
      exitFailure

traceProcess :: FilePath -> IO ()
traceProcess fp = do
  putStrLn "Spawning the tracee process"
  cpid <- forkProcess do
    runE_ sysTraceMe
    executeFile fp False [] Nothing
  let pid = ProcessID (fromIntegral cpid)
  putStrLn $ "  " ++ show pid

  putStrLn "Attaching it"
  runE_ (sysTraceSeize pid [OptTraceFork,OptTraceExec])

  putStrLn "Interrupting it"
  runE_ (sysTraceInterrupt pid)

  putStrLn "Get registers"
  regs <- runE (sysTraceGetRegs pid)
  print regs

  return () 
