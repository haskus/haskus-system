{-# LANGUAGE LambdaCase #-}

import ViperVM.Arch.X86_64.Linux.Network
import ViperVM.Arch.Linux.Network.SendReceive

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
   let 
      try str a = EitherT (a >>= \case
         Left err -> return (Left (str,err))
         Right v  -> return (Right v))

   ret <- runEitherT $ do
      fd <- try "Create a netlink socket" $ sysSocket (SockTypeNetlink NetlinkTypeKernelEvent) []

      try "Bind socket" $ sysBindNetlink fd 0 0

      bs <- try "Reading socket" $ receiveByteString fd 500 []

      liftIO $ putStrLn (show bs)

   case ret of
      Left (str,err) -> putStrLn $ "Error while trying to " ++ str ++ " (" ++ show err ++ ")"
      Right _ -> putStrLn "Done"
