{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.FileSystem
import ViperVM.Arch.Linux.FileSystem.ReadWrite
import ViperVM.Arch.Linux.FileSystem.Directory
import ViperVM.Arch.Linux.Process
import ViperVM.Arch.Linux.Memory
import ViperVM.Arch.Linux.Info
import ViperVM.Arch.Linux.Time
--import ViperVM.Arch.X86_64.Linux.Futex
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Internals.Input
import qualified ViperVM.Format.Binary.BitSet as BitSet


import Foreign.C.String (withCString)
import Control.Monad (unless)
import Data.Foldable (traverse_)
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
         fd <- check <$> sysOpen "dummy.result" [HandleWriteOnly,HandleCreate] [PermUserWrite,PermUserRead]

         let str = "Hello Linux!"
         putStrLn (printf "Writing \"%s\" in it" str)
         withCString str $ \str' -> do
            n <- check <$> sysWrite fd str' (fromIntegral $ length str)
            unless (n == fromIntegral (length str)) $
               error "The full string has not been written"
         
         putStrLn "Closing file"
         check <$> sysClose fd

   putStrLn "Checking for access to dummy.result file"
   fExist <- sysAccess "dummy.result" BitSet.empty
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

   putStrLn "Applying stat to /usr/bin/vim"
   stat <- check <$> sysFileStat "/usr/bin/vim" True
   print stat


   Right perm <- sysSetProcessUMask [PermUserRead,PermUserWrite,PermUserExecute]
   putStrLn $ "Previous umask: " ++ show perm
   Right perm2 <- sysSetProcessUMask [PermUserRead,PermUserWrite,PermUserExecute]
   putStrLn $ "New umask: " ++ show perm2

   Right info <- systemInfo
   print info

   let 
      printTimeSpec ts = ret
         where 
            ret = s * (10^ (9 :: Integer) :: Integer) + ns
            s = fromIntegral (tsSeconds ts) :: Integer
            ns = fromIntegral (tsNanoSeconds ts) :: Integer
      printClock clk = do
         ret <- sysClockGetTime clk
         res <- sysClockGetResolution clk
         putStrLn $ case (ret,res) of
            (Right t, Right r) -> 
               "Clock " ++ show clk ++ ": " ++ show (printTimeSpec t) ++ " (resolution: " ++ show (printTimeSpec r) ++ ")"
            (Right t, Left err) -> 
               "Clock " ++ show clk ++ ": " ++ show (printTimeSpec t) ++ " (no resolution: " ++ show err ++ ")"
            (Left err1, Left err2) -> 
               "Clock " ++ show clk ++ ": error " ++ show err1 ++ " (no resolution: " ++ show err2 ++ ")"
            (Left err1, Right r) -> 
               "Clock " ++ show clk ++ ": error " ++ show err1 ++ " (resolution: " ++ show (printTimeSpec r) ++ ")"
      clocks =
         [ ClockWall
         , ClockMonotonic
         , ClockProcess
         , ClockThread
         , ClockRawMonotonic
         , ClockCoarseWall
         , ClockCoarseMonotonic
         , ClockBoot
         , ClockTAI
         ]

   traverse_ printClock (clocks :: [Clock])

   putStrLn "Listing /etc directory:"
   etcfd <- check <$> sysOpen "/etc" [] BitSet.empty
   entries <- check <$> listDirectory etcfd
   entries2 <- check <$> listDirectory etcfd
   check <$> sysClose etcfd
   print entries
   print entries2

   putStrLn "Sleeping for 2 seconds (interruptible)..."
   sysNanoSleep (TimeSpec 2 0) >>= \case
      Left err -> putStrLn $ "Sleeping failed with " ++ show err
      Right (WokenUp remd) -> putStrLn $ "Woken-up, remaining time: " ++ show remd
      Right CompleteSleep -> putStrLn $ "Sleep completed"

   putStrLn "Sleeping for 2 seconds (automatic relaunch)..."
   nanoSleep (TimeSpec 2 0) >>= \case
      Left err -> putStrLn $ "Sleeping failed with " ++ show err
      Right _  -> putStrLn $ "Sleeping succeeded"

   putStrLn "Get device info"
   dev <- check <$> sysOpen "/dev/input/event0" [] [PermUserRead]

   driverVersion <- getVersion dev
   putStrLn $ "Driver version: " ++ show driverVersion

   deviceInfo <- getDeviceInfo dev
   putStrLn $ "Device info: " ++ show deviceInfo

   repeatSettings <- getRepeatSettings dev
   putStrLn $ "Repeat settings: " ++ show repeatSettings

   devName <- getDeviceName dev
   putStrLn $ "Device name: " ++ show devName

   devUID <- getDeviceUniqueID dev
   putStrLn $ "Device uid: " ++ show devUID

   devLoc <- getDevicePhysicalLocation dev
   putStrLn $ "Device physical location: " ++ show devLoc

   devProp <- getDeviceProperties dev
   putStrLn $ "Device properties: " ++ show devProp

   devKeys <- getDeviceKeys 256 dev
   putStrLn $ "Device keys: " ++ show devKeys

   devLeds <- getDeviceLEDs 50 dev
   putStrLn $ "Device LEDs: " ++ show devLeds

   case repeatSettings of
      Left _ -> putStrLn "Skip: set repeat period"
      Right rs -> do
         putStrLn "Set repeat period very low"
         let nrs = rs { repeatDelay = 250, repeatPeriod = 33 } 
         setRepeatSettings nrs dev >>= \case
            Left err -> putStrLn $ "Failed: " ++ show err
            Right _  -> do
               rs2 <- getRepeatSettings dev
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
