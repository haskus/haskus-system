{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Clock.Render where

import Haskus.System.Graphics.Diagrams
import Haskus.System.Graphics.Colour
import Haskus.Utils.Flow

-- | Clock drawn with Diagrams
clockDiag :: Float -> Float -> Float -> Float -> Float -> VDiagram
clockDiag screenWidth screenHeight h m s = diag
   where
      diag    = mconcat [secs, hrs, mins, nbrs, ctrcirc, bgcirc, bgrect]
      -- background
      bgrect  = rect screenWidth screenHeight |> lw none |> fc white
      bgcirc  = circle (screenHeight / 2.0 - 20) 
                  |> fc darkgray
                  |> lc gray
                  |> lw 30
      ctrcirc = circle 7 # fc white # lw none
      -- numbers
      nbrs    = position (map mkPoint [1..12 :: Int] `zip` map nb [1..12 :: Int])
      mkPoint x = p2 (300 * cos (angle x), 300 * sin (angle x))
               
      angle n = fromIntegral (n-3) * (-pi/6)
      nb n    = text (show n)
                  |> fc white
                  |> scale 50.0
                  |> center
      -- clock hands
      hours   = h / 12 * 2 * pi
      minutes = m / 60 * 2 * pi
      seconds = s / 60 * 2 * pi
      hrs     = hand hours 180   |> lw 10
      mins    = hand minutes 240 |> lw 10
      secs    = hand seconds 300 |> lw 3
      hand pos len = (p2 (0,0)) ~~ (p2 (len*sin pos,len*cos pos))
                        |> lc white # opacity 0.6
