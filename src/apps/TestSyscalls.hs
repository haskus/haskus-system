{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Haskus.Arch.Linux.Handle
import Haskus.Arch.Linux.FileSystem
import Haskus.Arch.Linux.FileSystem.ReadWrite
import Haskus.Arch.Linux.FileSystem.Directory
import Haskus.Arch.Linux.Process
import Haskus.Arch.Linux.Memory
import Haskus.Arch.Linux.Info
import Haskus.Arch.Linux.Time
--import Haskus.Arch.X86_64.Linux.Futex
import Haskus.Arch.Linux.ErrorCode
import Haskus.Arch.Linux.Internals.Input
import qualified Haskus.Format.Binary.BitSet as BitSet
import Haskus.Format.String (withCString)
import Haskus.Utils.Variant
import Haskus.Utils.Flow

import Control.Monad (unless)
import Data.Foldable (traverse_)
--import Foreign.Marshal.Alloc
--import Foreign.Storable (poke)
import Text.Printf

check :: Variant '[a,ErrorCode] -> a
check v = case headVariant v of
   Right a  -> a
   Left err -> error ("syscall error code " ++ show err)

check' :: Show a => String -> a -> IO ()
check' text v = do
   let msg = printf "%s (failed with %s)" text (show v)
   error msg

main :: IO ()
main = do
   
   let writeDummyFile = do
         putStrLn "Opening dummy.result file"
         fd <- open Nothing "dummy.result" [HandleWriteOnly,HandleCreate] [PermUserWrite,PermUserRead]
               >..~!!> check' "open dummy.result file"

         let str = "Hello Linux!"
         putStrLn (printf "Writing \"%s\" in it" str)
         withCString str $ \str' -> do
            n <- check <$> sysWrite fd str' (fromIntegral $ length str)
            unless (n == fromIntegral (length str)) $
               error "The full string has not been written"
         
         putStrLn "Closing file"
         close fd >..~!!> check' "close"

   putStrLn "Checking for access to dummy.result file"
   fExist <- sysAccess "dummy.result" BitSet.empty
   fWrite <- sysAccess "dummy.result" [AccessWrite]
   case (headVariant fExist, headVariant fWrite) of
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
   sysFork
      >%~=> \case
         ProcessID 0 -> do
            putStrLn "I'm the child process!"
            sysExit 0
         ProcessID n ->
            putStrLn (printf "Child process created with PID %d" n)
      >..%~!> \(err :: ErrorCode) -> error ("Error while forking: " ++ show err)

   sysGetCurrentDirectory >.~!> \cwd -> 
      putStrLn (printf "Current directory is: %s" cwd)

   let ncwd = "/usr/bin"
   putStrLn (printf "Setting current directory to: %s" ncwd)
   _ <- sysSetCurrentDirectoryPath ncwd

   sysGetCurrentDirectory >.~!> \cwd -> 
      putStrLn (printf "Current directory is: %s" cwd)

   putStrLn "Applying stat to /usr/bin/vim"
   stat <- check <$> sysFileStat "/usr/bin/vim" True
   print stat


   perm <- sysSetProcessUMask [PermUserRead,PermUserWrite,PermUserExecute]
   putStrLn $ "Previous umask: " ++ show perm
   perm2 <- sysSetProcessUMask [PermUserRead,PermUserWrite,PermUserExecute]
   putStrLn $ "New umask: " ++ show perm2

   info <- systemInfo
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
         putStrLn $ case (headVariant ret, headVariant res) of
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
   etcfd <- open Nothing "/etc" [] BitSet.empty
            >..~!!> check' "open"
   entries <- check <$> listDirectory etcfd
   entries2 <- check <$> listDirectory etcfd
   close etcfd >..~!!> check' "close"
   print entries
   print entries2

   putStrLn "Sleeping for 2 seconds (interruptible)..."
   sysNanoSleep (TimeSpec 2 0)
      >..~=> (\err -> putStrLn $ "Sleeping failed with " ++ show err)
      >.~!> \case
         WokenUp remd  -> putStrLn $ "Woken-up, remaining time: " ++ show remd
         CompleteSleep -> putStrLn $ "Sleep completed"

   putStrLn "Sleeping for 2 seconds (automatic relaunch)..."
   nanoSleep (TimeSpec 2 0)
      >..~=> (\err -> putStrLn $ "Sleeping failed with " ++ show err)
      >.~!> (const (putStrLn $ "Sleeping succeeded"))

   putStrLn "Get device info"
   dev <- open Nothing "/dev/input/event0" [] [PermUserRead]
          >..~!!> check' "open"

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

   repeatSettings
      ..~=> const (putStrLn "Skip: set repeat period")
      >.~!> \rs -> do
         putStrLn "Set repeat period very low"
         let nrs = rs { repeatDelay = 250, repeatPeriod = 33 } 
         setRepeatSettings nrs dev
            >..~=> (\err -> putStrLn ("Failed: " ++ show err))
            >.~!> \_ -> do
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
