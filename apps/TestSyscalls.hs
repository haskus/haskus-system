module Main where

import ViperVM.Arch.X86_64.Linux.FileSystem
import ViperVM.Arch.X86_64.Linux.Process
import ViperVM.Arch.X86_64.Linux.Memory
import ViperVM.Arch.X86_64.Linux.ErrorCode
import Foreign.C.String (withCString)
import Control.Monad (unless)
import Control.Applicative ((<$>))
import Text.Printf

check :: Either ErrorCode a -> a
check (Right a) = a
check (Left err) = error ("syscall error code " ++ show err)

main :: IO ()
main = do
   putStrLn "Opening dummy.result file"
   fd <- check <$> sysOpen "dummy.result" [OpenWriteOnly,OpenCreate] [PermUserWrite,PermUserRead]

   let str = "Hello Linux!"
   putStrLn (printf "Writing \"%s\" in it" str)
   withCString str $ \str' -> do
      n <- check <$> sysWrite fd str' (fromIntegral $ length str)
      unless (n == fromIntegral (length str)) $
         error "The full string has not been written"
   
   putStrLn "Closing file"
   check <$> sysClose fd

   putStrLn "Retrieving current CPU and NUMA node"
   (cpu,node) <- check <$> sysGetCPU
   putStrLn (printf "  - CPU %d, NODE %d" cpu node)

   putStrLn "Retrieving current program break (brk)"
   brk <- sysBrkGet
   putStrLn (printf"  - BRK 0x%x" brk)

   putStrLn "Trying to increase it of 1024 bytes"
   sysBrkSet (brk+1024) >>= \suc ->
      putStrLn $ if suc then "  - Success" else "  - Failure"
   sysBrkGet >>= \brk' -> 
      putStrLn (printf"  - BRK 0x%x" brk')


   putStrLn "Now exiting with code 15"
   sysExit 15
   putStrLn "Will not be displayed!"
