import Haskus.System
import Haskus.Arch.Linux.Graphics.State

main :: IO ()
main = runSys' <| do

   sys   <- defaultSystemInit
   term  <- defaultTerminal

   -- get graphic card devices
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   -- for each card
   forM_ cards <| \card -> do
      state <- readGraphicsState (graphicCardHandle card)
               >..~!!> assertShow "Cannot read graphics state"

      -- get connector state and info
      let conns = graphicsConnectors state
      
      -- show connector state and info
      writeStrLn term (show conns)

   powerOff
