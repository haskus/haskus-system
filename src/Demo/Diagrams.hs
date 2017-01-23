{-# LANGUAGE FlexibleContexts #-}

module Demo.Diagrams
   ( topBarDiag
   , infoPageDiag
   )
where

import Haskus.System
import Haskus.System.Graphics.Diagrams
import Haskus.Format.String
import Haskus.Arch.Linux.Info

-- | Top-bar
topBarDiag :: Float -> Float -> VDiagram
topBarDiag screenWidth _screenHeight = diag
   where
      diag    = mconcat [pbts, bgrect]
      lbls    = ["Info","Display", "Input"]
      bts     = [ lbl n t | (t,n) <- lbls `zip` [(1::Int)..]]
      pbts    = position (zip (map mkPoint [0..]) bts)
                   |> translateY 5.0
                   |> alignBy (V2 1 0) (-1)
                   |> translateX 5.0
      mkPoint x = p2 (x*75,0)
      -- bar background
      bgrect  = rect screenWidth 20.0
                  |> lw none
                  |> fc white
                  |> alignBy (V2 1 0) (-1)
                  |> alignBy (V2 0 1) (-1)

      -- button + label 
      lbl n t = (btn n <> lbt t)
                  |> alignBy (V2 1 0) 1

      -- button
      btn n   = (btt ("F"++show n) <> btnbg)
                  |> translateX (-12)
                  |> alignBy (V2 0 1) (-1)
                  |> translateY (-3)

      -- button background
      btnbg   = square 16
                  |> fc lightgray
      -- button text
      btt t   = text t
                  -- |> fontSize (local 10.0)
                  |> scale 10.0
                  |> fc black
                  |> center

      -- label text
      lbt t   = text t
                  -- |> fontSize (normalized 50.0)
                  |> scale 10.0
                  |> fc black


infoPageDiag :: SystemInfo -> VDiagram
infoPageDiag info = d
   where
      d = position (join tss)
      tss = [ [(p2 (0,-2*y), lbl), (p2 (10,-2*y), val)] | ((lbl,val),y) <- ts `zip` [0..]]
      ts = [ mt "OS name"      systemName
           , mt "Release"      systemRelease
           , mt "Version"      systemVersion
           , mt "Machine"      systemMachine
           , mt "Network name" systemNodeName
           ]
      mt lbl f = ( text (lbl ++ ":")
                 , text (fromCStringBuffer (f info))
                 )
