{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module ViperVM.Library.Library
   ( loadPreprocessedKernel
   )
where

import ViperVM.Library.Kernel
import ViperVM.Platform.Types (Proc,procModelHash,procPeer)

import qualified ViperVM.Platform.Drivers as Peer
import qualified ViperVM.Platform.Drivers.OpenCL as OpenCL

--import System.FilePath
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
      Left err -> do
         preproc <- preprocess proc kernel
         BS.writeFile (encodeString file) (runPut (safePut preproc))
         return preproc


-- | Preprocess a kernel for a given proc
-- (e.g. compile an OpenCL source into a program)
preprocess :: Proc -> Kernel -> IO KernelBin
preprocess proc (OpenCLSource src) = do
   case procPeer proc of
      Peer.OpenCLProc p -> do
         -- Compile program
         -- TODO

         -- Retrieve binary
         -- TODO

         --return (OpenCLBinary bin)
         undefined
      _ -> error "Invalid proc"
