import Haskus.System
import Haskus.Arch.Linux.Handle
import Haskus.Arch.Linux.Graphics.Object
import Haskus.Arch.Linux.Graphics.State
import Haskus.Arch.Linux.Graphics.Property
import qualified Haskus.Utils.Map as Map

main :: IO ()
main = runSys' <| do

   sys   <- defaultSystemInit
   term  <- defaultTerminal

   -- get graphic card devices
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   let
      makeProperty hdl (RawProperty x y) = getPropertyMeta hdl x >.-.> \m -> Property m y

      showProps :: (Show o, Object o) => Handle -> o -> Sys ()
      showProps hdl o = do
         writeStrLn term (show o)

         getObjectProperties hdl o
            >.~=> (\props' -> writeStrLn term ("Properties: " ++ show props'))
            >.~|> flowTraverse (makeProperty hdl)
            >.~!> \props -> writeStrLn term ("Properties: " ++ show props)

   forM_ cards <| \card -> do
      state <- readGraphicsState (graphicCardHandle card)
               >..~!!> assertShow "Cannot read graphics state"

      let
         showProps' :: (Show o, Object o) => o -> Sys ()
         showProps' = showProps (graphicCardHandle card)

      mapM_ showProps' (Map.elems (graphicsConnectors state))
      mapM_ showProps' (Map.elems (graphicsEncoders state))
      mapM_ showProps' (Map.elems (graphicsControllers state))
      mapM_ showProps' (Map.elems (graphicsPlanes state))

   sysLogPrint
   powerOff
