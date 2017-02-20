import Haskus.System

main :: IO ()
main = runSys <| do

   -- Initialize the terminal
   term <- defaultTerminal

   -- print a string on the standard output
   writeStrLn term "Hello World!"

   -- wait for a key to be pressed
   waitForKey term

   sysLogPrint

   -- shutdown the computer
   void powerOff
