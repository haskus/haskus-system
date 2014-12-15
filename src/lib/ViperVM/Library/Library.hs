{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module ViperVM.Library.Library
   ( loadPreprocessedKernel
   )
where

import ViperVM.Library.Kernel
import ViperVM.Platform.Types (Proc,procModelHash,procPeer)

import ViperVM.Platform.PlatformInfo
import qualified ViperVM.Platform.Drivers as Peer
import qualified ViperVM.Platform.Drivers.OpenCL as CL
-- TODO: move low-level parts into Platform.Drivers
import qualified ViperVM.Arch.OpenCL.Error as CL
import qualified ViperVM.Arch.OpenCL.Program as CL
import qualified ViperVM.Arch.OpenCL.Context as CL
import qualified ViperVM.Arch.OpenCL.Platform as CL
import qualified ViperVM.Arch.OpenCL.Entity as CL

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)
import Filesystem (isFile,getAppCacheDirectory)
import Filesystem.Path.CurrentOS
import qualified Data.ByteString as BS
import Data.Serialize (runGet, runPut)
import Data.SafeCopy (safePut,safeGet)
import qualified Data.Text as T
import Control.Applicative ((<$>))

-- | Try to load a preprocessed kernel for a processor
-- (e.g. a compiled OpenCL kernel)
loadPreprocessedKernel :: Proc -> Kernel -> IO KernelBin
loadPreprocessedKernel proc kernel = do
   
   -- Get processor model
   let modelHash = procModelHash proc
       kerHash = kernelHash kernel

   -- Get application cache directory
   appDir <- getAppCacheDirectory "vipervm"

   -- Compute kernel cache filename for the proc model
   let file = appDir </> fromText "library" </> fromText (T.pack kerHash) </> fromText "preprocessed" </> fromText (T.pack modelHash)

   -- Try to load the preprocessed kernel
   k1 <- isFile file >>= \case
      True -> runGet safeGet <$> BS.readFile (encodeString file)
      False -> return (Left "Preprocessed kernel not found")

   -- Preprocess and save the kernel if necessary
   case k1 of
      Right k -> return k
      Left _ -> do
         preproc <- preprocess proc kernel
         BS.writeFile (encodeString file) (runPut (safePut preproc))
         return preproc


-- | Preprocess a kernel for a given proc
-- (e.g. compile an OpenCL source into a program)
preprocess :: Proc -> Kernel -> IO KernelBin
preprocess proc (OpenCLSource src) = do
   putStrLn $ "Compiling on " ++ procInfo proc
   case procPeer proc of
      Peer.OpenCLProc p -> do
         let 
            options = ""
            dev = CL.clProcDevice p

         bin <- runEitherT $ do
            pf <- EitherT $ CL.getDevicePlatform dev
            ctx <- EitherT $ CL.createContext pf [dev]

            -- Create program
            prog <- EitherT $ CL.createProgramFromSource ctx src
 
            -- Compile program
            EitherT $ do
               err <- CL.buildProgram prog dev options
               case err of
                  Left CL.CL_BUILD_PROGRAM_FAILURE -> do
                     -- Show log
                     errlog <- CL.getProgramBuildLog prog dev
                     putStrLn $ "Error: " ++ show err
                     putStrLn "Log:"
                     putStrLn (show errlog)
                     return err
                  _ -> return err

            -- Try to retrieve binary
            b <- EitherT $ CL.getProgramBinary prog dev

            -- Cleanup
            liftIO $ CL.release prog
            liftIO $ CL.release ctx

            right b

         case bin of
            Left err       -> error (show err)
            Right Nothing  -> error "Unable to get binary"
            Right (Just b) -> return (OpenCLBinary b)

      _ -> error "Invalid processor for the given kernel"
