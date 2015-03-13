{-# LANGUAGE LambdaCase #-}

import ViperVM.Arch.Linux.KernelEvent

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)

main :: IO ()
main = do
   let 
      try str a = EitherT (a >>= \case
         Left err -> return (Left (str,err))
         Right v  -> return (Right v))

   ret <- runEitherT $ do
      fd <- try "Create a kernel event socket" $ createKernelEventSocket

      _ <- forever $ do
         msg <- try "Reading socket" $ receiveKernelEvent fd
         liftIO $ putStrLn (show msg)

      return ()

   case ret of
      Left (str,err) -> putStrLn $ "Error while trying to " ++ str ++ " (" ++ show err ++ ")"
      Right _ -> putStrLn "Done"


