{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Demo.Graphics
   ( graphicsPage
   )
where

import Haskus.System.Graphics
import Haskus.System.Linux.Graphics.State
import Haskus.System.Linux.Graphics.Entities
import Haskus.System.Linux.Handle
import Haskus.System.Graphics.Diagrams
import Haskus.Utils.Flow
import Haskus.Utils.List
import qualified Haskus.Utils.Map as Map

data DisconnectedCard = DisconnectedCard

graphicsPage :: MonadInIO m => GraphicCard -> Excepts '[DisconnectedCard] m VDiagram
graphicsPage card = do
   state <- readGraphicsState (graphicCardHandle card)
            |> catchE (\InvalidHandle -> failureE DisconnectedCard)
   encs <- getHandleEncoders (graphicCardHandle card)
            |> catchE (\InvalidHandle -> failureE DisconnectedCard)
   return (drawGraphics state encs)

drawGraphics :: GraphicsState -> [Encoder] -> VDiagram
drawGraphics state encoders = diag
   where
      planesIds = Map.keys (graphicsPlanes      state)
      ctrlsIds  = Map.keys (graphicsControllers state)
      encsIds   = fmap encoderID encoders
      connsIds  = Map.keys (graphicsConnectors  state)
      fbIds     = graphicsFrameBuffers  state

      possibleClones = nub [(encoderID enc, c)
                           | enc <- encoders
                           , c <- encoderPossibleClones enc
                           ]

      box name txt = btn 
            |> alignX (-1)
            |> named (name ++ ":left")
            |> alignX 1
            |> named (name ++ ":right")
         where
            btn     = btt txt <> btnbg

            btnbg   = rect 100 30
                        |> fc lightgray
            btt t   = text t
                        -- |> fontSize (local 10.0)
                        |> scale 10.0
                        |> fc black
                        |> center

      boxCol xs = vcat
         <| intersperse (strutY 10)
         <| fmap (uncurry box) xs

      diag = boxes
           -- connector --> encoder arrows
           |> connEncArrows [(c,e) | conn <- Map.elems (graphicsConnectors state)
                                   , let e = connectorEncoderID conn
                                   , let c = connectorID conn
                                   ]
           -- encoder --> controller arrows
           |> encCtrlArrows [(e,c) | enc <- encoders
                                   , let c = encoderControllerID enc
                                   , let e = encoderID enc
                                   ]
           -- plane --> controller arrows
           |> planeCtrlArrows [(e,c) | pl <- Map.elems (graphicsPlanes state)
                                     , let c = planeControllerId pl
                                     , let e = planeID pl
                                     ]
           -- plane --> framebuffer arrows
           |> planeFbArrows [(e,c) | pl <- Map.elems (graphicsPlanes state)
                                   , let c = planeFrameSourceId pl
                                   , let e = planeID pl
                                   ]
           -- encoders that can use the same controller at the same time
           -- (clones)
           |> cloneArrows possibleClones
           |> frame 25
         
      boxes = hcat <| intersperse (strutX 60)
               [ fbs,planes,ctrls,encs,conns]

      connEncArrows [] b = b
      connEncArrows ((_, Nothing):xs) b = connEncArrows xs b
      connEncArrows ((EntityID co, Just (EntityID en)):xs) b =
         connEncArrows xs (arrw ("conn"++show co++":left") ("enc"++show en++":right") b)

      encCtrlArrows [] b = b
      encCtrlArrows ((_, Nothing):xs) b = encCtrlArrows xs b
      encCtrlArrows ((EntityID en, Just (EntityID ct)):xs) b =
         encCtrlArrows xs (arrw ("enc"++show en++":left") ("ctrl"++show ct++":right") b)

      planeCtrlArrows [] b = b
      planeCtrlArrows ((_, Nothing):xs) b = planeCtrlArrows xs b
      planeCtrlArrows ((EntityID pl, Just (EntityID ct)):xs) b =
         planeCtrlArrows xs (arrw ("ctrl"++show ct++":left") ("plane"++show pl++":right") b)

      planeFbArrows [] b = b
      planeFbArrows ((_, Nothing):xs) b = planeFbArrows xs b
      planeFbArrows ((EntityID pl, Just (EntityID fb)):xs) b =
         planeFbArrows xs (arrw ("plane"++show pl++":left") ("fb"++show fb++":right") b)

      cloneArrows [] b           = b
      cloneArrows ((e1,e2):xs) b = cloneArrows xs (arrw ("enc"++show e1++":left") ("enc"++show e2++":left") b)

      conns  = boxCol [ ("conn"++ show x, "Connector "++show x)
                      | EntityID x <- connsIds
                      ]
      encs   = boxCol [ ("enc"++show x, "Encoder "++show x)
                      | EntityID x <- encsIds
                      ]
      ctrls  = boxCol [ ("ctrl"++show x, "Controller "++show x)
                      | EntityID x <- ctrlsIds
                      ]
      planes = boxCol [ ("plane"++show x, "Plane "++show x)
                      | EntityID x <- planesIds
                      ]
      fbs    = boxCol [ ("fb"++show x, "FrameBuffer "++show x)
                      | EntityID x <- fbIds
                      ]

      arrw dst src = connect' (with |> arrowHead .~ spike) src dst
