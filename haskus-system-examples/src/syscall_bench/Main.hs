{-# LANGUAGE MagicHash #-}

import Haskus.Arch.X86_64.Linux.Syscall
import GHC.Ptr
import Control.Monad

main :: IO ()
main = do
   let num = 1                    -- "write" syscall number
   let fd  = 1 :: Int             -- "stdout" file descriptor number
   let str = Ptr ""# -- primitive string to print
   let sz  = 0 :: Int            -- size of the string in bytes
   replicateM_ 100000000 $ do
      -- 17,11s user 19,76s system 99% cpu 36,891 total
      --void (syscall3interruptible num fd str sz) 

      -- 17,72s user 19,09s system 99% cpu 36,841 total
      --void (syscall3safe num fd str sz) 

      -- 12,37s user 19,99s system 99% cpu 32,361 total
      --void (syscall3unsafe num fd str sz) 

      -- 12,27s user 19,65s system 99% cpu 31,921 total
      void (syscall3primop num fd str sz) 
      
