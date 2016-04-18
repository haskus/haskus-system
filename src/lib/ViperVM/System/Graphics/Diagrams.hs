module ViperVM.System.Graphics.Diagrams
   ( renderDiagram
   , module Diagrams
   )
where

import Diagrams
import Diagrams.Backend.Rasterific
import Codec.Picture.Types
import Data.Monoid (Any)

renderDiagram :: (TypeableFloat n) => SizeSpec V2 n -> QDiagram Rasterific V2 n Any -> Image PixelRGBA8
renderDiagram spec = renderDia Rasterific (RasterificOptions spec)
