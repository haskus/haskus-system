import Haskus.System

main :: IO ()
main = runSys' <| do

   term <- defaultTerminal
   sys  <- defaultSystemInit
   let dm = systemDeviceManager sys

   -- Display kernel events
   onEvent (dmEvents dm) <| \ev ->
      writeStrLn term (show ev)

   waitForKey term
   powerOff
