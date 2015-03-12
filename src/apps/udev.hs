{-# LANGUAGE LambdaCase #-}

import ViperVM.Arch.X86_64.Linux.Network
import ViperVM.Arch.X86_64.Linux.Process
import Control.Monad.Trans.Either

main :: IO ()
main = do
   let 
      try str a = EitherT (a >>= \case
         Left err -> return (Left (str,err))
         Right v  -> return (Right v))

   ret <- runEitherT $ do
      fd <- try "Create a netlink socket" $ sysSocket (SockTypeNetlink NetlinkTypeKernelEvent) []

      try "Bind socket" $ sysBindNetlink fd (ProcessID 0) 0

   case ret of
      Left (str,err) -> putStrLn $ "Error while trying to " ++ str ++ " (" ++ show err ++ ")"
      Right _ -> putStrLn "Done"
