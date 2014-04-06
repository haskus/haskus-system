{-# LANGUAGE LambdaCase #-}
module Main where

import ViperVM.Arch.X86_64.Linux.FileSystem
import ViperVM.Arch.X86_64.Linux.Process
import ViperVM.Arch.X86_64.Linux.Memory
import ViperVM.Arch.X86_64.Linux.ErrorCode
import Foreign.C.String (withCString)
import Control.Monad (unless)
import Control.Applicative ((<$>))
import Text.Printf

check :: Either ErrorCode a -> a
check (Right a) = a
check (Left err) = error ("syscall error code " ++ show err)

main :: IO ()
main = do
   
   putStrLn "Checking for access to dumy.result file"
   fExist <- sysAccess "dummy.result" [AccessExist]
   fWrite <- sysAccess "dummy.result" [AccessWrite]
   case (fExist,fWrite) of
      (Right _, Right _) -> putStrLn " - File exists and is writeable"
      (Right _, Left _) -> putStrLn " - File exists and is NOT writeable"
      (Left _, _) -> putStrLn " - File does not exist"

   case fWrite of
      Right _ -> do
         putStrLn "Opening dummy.result file"
         fd <- check <$> sysOpen "dummy.result" [OpenWriteOnly,OpenCreate] [PermUserWrite,PermUserRead]

         let str = "Hello Linux!"
         putStrLn (printf "Writing \"%s\" in it" str)
         withCString str $ \str' -> do
            n <- check <$> sysWrite fd str' (fromIntegral $ length str)
            unless (n == fromIntegral (length str)) $
               error "The full string has not been written"
         
         putStrLn "Closing file"
         check <$> sysClose fd
      _ -> return ()

   sysGetProcessID >>= \(ProcessID pid) -> 
      putStrLn (printf " - Process ID: %d" pid)

   sysGetThreadID >>= \(ThreadID pid) -> 
      putStrLn (printf " - Thread ID: %d" pid)

   sysGetParentProcessID >>= \(ProcessID pid) -> 
      putStrLn (printf " - Parent process ID: %d" pid)

   sysGetRealUserID >>= \(UserID uid) -> 
      putStrLn (printf " - Real user ID: %d" uid)

   sysGetEffectiveUserID >>= \(UserID uid) -> 
      putStrLn (printf " - Effective user ID: %d" uid)

   sysGetRealGroupID >>= \(GroupID uid) -> 
      putStrLn (printf " - Real group ID: %d" uid)

   sysGetEffectiveGroupID >>= \(GroupID uid) -> 
      putStrLn (printf " - Effective group ID: %d" uid)

   check <$> sysGetCPU >>= \(cpu,node) ->
      putStrLn (printf " - CPU %d, NUMA node %d" cpu node)

   putStrLn "Retrieving current program break (brk)"
   brk <- sysBrkGet
   putStrLn (printf"  - BRK 0x%x" brk)

   putStrLn "Trying to increase it of 1024 bytes"
   sysBrkSet (brk+1024) >>= \suc ->
      putStrLn $ if suc then "  - Success" else "  - Failure"
   sysBrkGet >>= \brk' -> 
      putStrLn (printf"  - BRK 0x%x" brk')


   putStrLn "Now forking"
   sysFork >>= \case
      Right (ProcessID 0) -> do
         putStrLn "I'm the child process!"
         sysExit 0
      Right (ProcessID n) -> do
         putStrLn (printf "Child process created with PID %d" n)
      Left _ -> error "Error while forking"

   putStrLn "Now exiting with code 15"
   sysExit 15
   putStrLn "Will not be displayed!"
