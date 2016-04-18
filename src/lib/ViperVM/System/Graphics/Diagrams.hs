module ViperVM.System.Graphics.Diagrams
   ( renderDiagram
   , module Diagrams
   )
where

import Diagrams
import Diagrams.Backend.Rasterific
import Codec.Picture.Types

renderDiagram :: (TypeableFloat n,Monoid' m) => SizeSpec V2 n -> QDiagram Rasterific V2 n m -> Image PixelRGBA8
renderDiagram spec = renderDia Rasterific (RasterificOptions spec)
