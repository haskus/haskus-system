{-# LANGUAGE LambdaCase #-}
module Main where

import ViperVM.Arch.X86_64.Linux.FileSystem
import ViperVM.Arch.X86_64.Linux.Process
import ViperVM.Arch.X86_64.Linux.Memory
import ViperVM.Arch.X86_64.Linux.Info
--import ViperVM.Arch.X86_64.Linux.Futex
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Input
import Foreign.C.String (withCString)
import Control.Monad (unless)
import Control.Applicative ((<$>))
--import Foreign.Marshal.Alloc
--import Foreign.Storable (poke)
import Text.Printf

check :: Either ErrorCode a -> a
check (Right a) = a
check (Left err) = error ("syscall error code " ++ show err)

main :: IO ()
main = do
   
   let writeDummyFile = do
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

   putStrLn "Checking for access to dummy.result file"
   fExist <- sysAccess "dummy.result" [AccessExist]
   fWrite <- sysAccess "dummy.result" [AccessWrite]
   case (fExist,fWrite) of
      (Right _, Left _) -> putStrLn " - File exists and is NOT writeable"
      (Right _, Right _) -> putStrLn " - File exists and is writeable" >> writeDummyFile
      (Left _, _) -> putStrLn " - File does not exist" >> writeDummyFile


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
      Right (ProcessID n) ->
         putStrLn (printf "Child process created with PID %d" n)
      Left _ -> error "Error while forking"

   sysGetCurrentDirectory >>= \(Right cwd) -> 
      putStrLn (printf "Current directory is: %s" cwd)

   let ncwd = "/usr/bin"
   putStrLn (printf "Setting current directory to: %s" ncwd)
   Right _ <- sysSetCurrentDirectoryPath ncwd

   sysGetCurrentDirectory >>= \(Right cwd) -> 
      putStrLn (printf "Current directory is: %s" cwd)

   Right perm <- sysSetProcessUMask [PermUserRead,PermUserWrite,PermUserExecute]
   putStrLn $ "Previous umask: " ++ show perm
   Right perm2 <- sysSetProcessUMask [PermUserRead,PermUserWrite,PermUserExecute]
   putStrLn $ "New umask: " ++ show perm2

   Right info <- sysSystemInfo
   print info

   putStrLn "Get device info"
   dev <- check <$> sysOpen "/dev/input/event6" [OpenReadOnly] [PermUserRead]

   driverVersion <- getDriverVersion sysIoctl dev
   putStrLn $ "Driver version: " ++ show driverVersion

   deviceInfo <- getDeviceInfo sysIoctl dev
   putStrLn $ "Device info: " ++ show deviceInfo

   repeatSettings <- getRepeatSettings sysIoctl dev
   putStrLn $ "Repeat settings: " ++ show repeatSettings

   case repeatSettings of
      Left _ -> putStrLn "Skip: set repeat period"
      Right rs -> do
         putStrLn "Set repeat period very low"
         let nrs = rs { repeatDelay = 250, repeatPeriod = 33 } 
         setRepeatSettings sysIoctl dev nrs >>= \case
            Left err -> putStrLn $ "Failed: " ++ show err
            Right _  -> do
               rs2 <- getRepeatSettings sysIoctl dev
               putStrLn $ "New repeat settings: " ++ show rs2

   -- Use strace to see that syscall is correctly called
   -- BUGGY (segfault)
   --_ <- alloca $ \p ->
   --   alloca $ \p2 -> do
   --      poke p 10
   --      poke p2 12
   --      sysFutexCmpRequeue p 10 15 p2


   putStrLn "Now exiting with code 15"
   sysExit 15
   putStrLn "Will not be displayed!"
