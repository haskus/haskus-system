import Haskus.System

main :: IO ()
main = runSys' <| do

   sys  <- defaultSystemInit
   term <- defaultTerminal
   let dm = systemDeviceManager sys

   inputDevs   <- listDevicesWithClass dm "input"
   graphicDevs <- listDevicesWithClass dm "drm"

   let
      showDev dev = writeStrLn term ("  - " ++ show (fst dev))
      showDevs    = mapM_ showDev

   writeStrLn term "Input devices:"
   showDevs inputDevs

   writeStrLn term "Display devices:"
   showDevs graphicDevs

   powerOff
