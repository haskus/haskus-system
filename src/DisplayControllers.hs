import Haskus.System
import Haskus.Arch.Linux.Graphics.State

main :: IO ()
main = runSys' <| do

   sys   <- defaultSystemInit
   term  <- defaultTerminal
   cards <- loadGraphicCards (systemDeviceManager sys)

   forM_ cards <| \card -> do
      state <- readGraphicsState (graphicCardHandle card)
               >..~!!> assertShow "Cannot read graphics state"

      -- get controller state and info
      let ctrls = graphicsControllers state
      
      writeStrLn term (show ctrls)

   powerOff
