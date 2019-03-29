{-# LANGUAGE LambdaCase #-}

import Haskus.Apps.System.Build.Config
import Haskus.Apps.System.Build.Linux
import Haskus.Apps.System.Build.Syslinux
import Haskus.Apps.System.Build.CmdLine
import Haskus.Apps.System.Build.Utils
import Haskus.Apps.System.Build.Ramdisk
import Haskus.Apps.System.Build.Stack
import Haskus.Apps.System.Build.GMP
import Haskus.Apps.System.Build.QEMU
import Haskus.Apps.System.Build.ISO
import Haskus.Apps.System.Build.Disk

import qualified Data.Text as Text
import Haskus.Utils.Flow
import Options.Applicative.Simple
import Paths_haskus_system_build
import Data.Version
import System.IO.Temp
import System.Directory
import System.FilePath
 

main :: IO ()
main = do

   -- read command-line options
   (_,runCmd) <-
      simpleOptions (showVersion version)
                   "haskus-system-build"
                   "This tool lets you build systems using haskus-system framework. It manages Linux/Syslinux (download and build), it builds ramdisk, it launches QEMU, etc."
                   (pure ()) $ do
         addCommand "init"
                   "Create a new project from a template"
                   initCommand
                   initOptions
         addCommand "build"
                   "Build a project"
                   buildCommand
                   buildOptions
         addCommand "test"
                   "Test a project with QEMU"
                   testCommand
                   testOptions
         addCommand "make-iso"
                   "Create an ISO image"
                   (const makeISOCommand)
                   (pure ())
         addCommand "test-iso"
                   "Test an ISO image"
                   (const testISOCommand)
                   (pure ())
         addCommand "make-disk"
                   "Create a disk directory"
                   makeDiskCommand
                   (makeDiskOptions)
         addCommand "make-device"
                   "Create a bootable device (WARNING: IT ERASES IT). You must be in the sudoers list"
                   makeDeviceCommand
                   (makeDeviceOptions)
   runCmd

initCommand :: InitOptions -> IO ()
initCommand opts = do
   let
      template = initOptTemplate opts

   cd <- getCurrentDirectory

   withSystemTempDirectory "haskus-system-build" $ \fp -> do
      -- get latest templates
      showStep "Retrieving templates..."
      shellInErr fp "git clone --depth=1 https://github.com/haskus/haskus-system-templates.git" $
         failWith "Cannot retrieve templates. Check that `git` is installed and that github is reachable using https."

      let fp2 = fp </> "haskus-system-templates"
      dirs <- listDirectory fp2
      unless (any (== template) dirs) $
         failWith $ "Cannot find template \"" ++ template ++"\""

      -- copy template
      showStep $ "Copying \"" ++ template ++ "\" template..."
      shellInErr fp2
         ("cp -i -r ./" ++ template ++ "/* " ++ cd) $
            failWith "Cannot copy the selected template"

readConfig :: IO SystemConfig
readConfig = do
   let configFile = "system.yaml"
   
   unlessM (doesFileExist configFile) $
      failWith $ "Cannot find \"" ++ configFile ++ "\""

   mconfig <- readSystemConfig configFile

   case mconfig of
      Left e  -> failWith $ "Cannot parse \"" ++ configFile ++ "\": " ++ show e
      Right c -> return c

buildCommand :: BuildOptions -> IO ()
buildCommand opts = do
   config' <- readConfig

   -- override config
   let config = if buildOptInit opts /= ""
                  -- TODO: use lenses
                  then config'
                        { ramdiskConfig = (ramdiskConfig config')
                           { ramdiskInit = Text.pack (buildOptInit opts)
                           }
                        }
                  else config'

   showStatus config
   gmpMain
   linuxMain (linuxConfig config)
   _ <- syslinuxMain (syslinuxConfig config)
   stackBuild


testCommand :: TestOptions -> IO ()
testCommand opts = do
   config' <- readConfig

   -- override config
   let config = if testOptInit opts /= ""
                  -- TODO: use lenses
                  then config'
                        { ramdiskConfig = (ramdiskConfig config')
                           { ramdiskInit = Text.pack (testOptInit opts)
                           }
                        }
                  else config'

   showStatus config
   gmpMain
   linuxMain (linuxConfig config)
   stackBuild
   ramdiskMain (ramdiskConfig config)
   qemuExecRamdisk config

showStatus :: SystemConfig -> IO ()
showStatus config = do
   let linuxVersion' = Text.unpack (linuxConfigVersion (linuxConfig config))

   let syslinuxVersion' = config
                           |> syslinuxConfig
                           |> syslinuxVersion
                           |> Text.unpack
   let initProgram = config
                           |> ramdiskConfig
                           |> ramdiskInit
                           |> Text.unpack

   ghcVersion    <- stackGetGHCVersion

   putStrLn "==================================================="
   putStrLn "       Haskus system - build config"
   putStrLn "---------------------------------------------------"
   putStrLn ("GHC version:      " ++ ghcVersion)
   putStrLn ("Linux version:    " ++ linuxVersion')
   putStrLn ("Syslinux version: " ++ syslinuxVersion')
   putStrLn ("Init program:     " ++ initProgram)
   putStrLn "==================================================="


makeISOCommand :: IO ()
makeISOCommand = do
   config <- readConfig

   showStatus config
   gmpMain
   linuxMain (linuxConfig config)
   stackBuild
   ramdiskMain (ramdiskConfig config)
   _ <- isoMake config
   return ()

makeDiskCommand :: MakeDiskOptions -> IO ()
makeDiskCommand opts = do
   config <- readConfig

   showStatus config
   gmpMain
   linuxMain (linuxConfig config)
   stackBuild
   ramdiskMain (ramdiskConfig config)
   makeDisk config (diskOptPath opts)

testISOCommand :: IO ()
testISOCommand = do
   config <- readConfig

   showStatus config
   gmpMain
   linuxMain (linuxConfig config)
   stackBuild
   ramdiskMain (ramdiskConfig config)
   isoFile <- isoMake config
   qemuExecISO config isoFile

makeDeviceCommand :: MakeDeviceOptions -> IO ()
makeDeviceCommand opts = do
   config <- readConfig

   showStatus config
   gmpMain
   linuxMain (linuxConfig config)
   stackBuild
   ramdiskMain (ramdiskConfig config)
   makeDevice config (deviceOptPath opts)
