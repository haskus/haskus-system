module Main where

import Haskus.System.Sys
import Haskus.System.Network
import Haskus.Utils.Flow

main :: IO ()
main = runSys' $ do
   fd <- createKernelEventSocket
   forever $ do
      msg <- receiveKernelEvent fd
      liftIO $ putStrLn (show msg)
