import ViperVM.Arch.X86_64.Linux.Power

import Control.Monad (void)

main :: IO ()
main = do

   putStrLn "Booting HaskOS"

   putStrLn "And now, shutting down"
   void $ sysPower PowerOff
