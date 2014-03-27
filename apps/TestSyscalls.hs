module Main where

import ViperVM.Arch.X86_64.Linux.FileSystem
import ViperVM.Arch.X86_64.Linux.Process
import ViperVM.Arch.X86_64.Linux.ErrorCode
import Foreign.C.String (withCString)
import Control.Monad (unless)
import Control.Applicative ((<$>))

check :: Either ErrorCode a -> a
check (Right a) = a
check (Left err) = error ("syscall error code " ++ show err)

main :: IO ()
main = do
   putStrLn "Opening dummy.result file"
   fd <- check <$> sysOpen "dummy.result" [OpenWriteOnly,OpenCreate] [PermUserWrite,PermUserRead]

   putStrLn "Writing Hello Linux in it"
   let str = "Hello Linux!"
   withCString str $ \str' -> do
      n <- check <$> sysWrite fd str' (fromIntegral $ length str)
      unless (n == fromIntegral (length str)) $
         error "The full string has not been written"
   
   putStrLn "Closing file"
   check <$> sysClose fd

   putStrLn "Now exiting with code 15"
   sysExit 15
   putStrLn "Will not be displayed!"
