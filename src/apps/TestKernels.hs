import ViperVM.Platform.Host
import ViperVM.Platform.Loading
import ViperVM.Platform.Config
import ViperVM.Library.Library

import ViperVM.Library.Maths.MatrixOps

import Control.Monad (void)
import Control.Applicative ((<$>),(<*>))
import Control.Exception (SomeException,catch)
import Data.Foldable (traverse_)

main :: IO ()
main = do
   host <- loadPlatform defaultConfig {
               enableOpenCLCPUs = True
            }
   procs <- allProcessorsFromHostIO host

   let 
      kernels =
         [ clMapOpKernel TyInt   OpAdd
         , clMapOpKernel TyFloat OpDiv
         ]
      compile (p,k) = do
         catch 
            (void $ loadPreprocessedKernel p k)
            (\e -> putStrLn $ "Error: " ++ show (e :: SomeException))

   traverse_ compile ((,) <$> procs <*> kernels) 

   return ()
