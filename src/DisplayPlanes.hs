import Haskus.System
import Haskus.Arch.Linux.Graphics.State
import qualified Haskus.Utils.Map as Map

main :: IO ()
main = runSys' <| do

   sys   <- defaultSystemInit
   term  <- defaultTerminal
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   forM_ cards <| \card -> do
      state <- readGraphicsState (graphicCardHandle card)
               >..~!!> assertShow "Cannot read graphics state"
      writeStrLn term (show (Map.elems (graphicsPlanes state)))

   powerOff
