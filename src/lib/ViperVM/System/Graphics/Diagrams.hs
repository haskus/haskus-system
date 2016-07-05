
-- | Diagrams utilities
module ViperVM.System.Graphics.Diagrams
   ( renderDiagram
   , VDiagram
   , VDiagram'
   , module Diagrams
   )
where

import Diagrams
import Diagrams.Backend.Rasterific
import Codec.Picture.Types
import Data.Monoid (Any)

type VDiagram' n = QDiagram Rasterific V2 n Any
type VDiagram    = QDiagram Rasterific V2 Float Any

-- | Render a diagram into an image that can be displayed on a framebuffer
renderDiagram :: (TypeableFloat n) => SizeSpec V2 n -> VDiagram' n -> Image PixelRGBA8
renderDiagram spec = renderDia Rasterific (RasterificOptions spec)
