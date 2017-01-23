import Haskus.System

main :: IO ()
main = runSys' <| do
   -- comment two of the three commands to test the third one
   void powerOff  -- shutdown the computer
   void restart   -- restart the computer
   void halt      -- halt the system
