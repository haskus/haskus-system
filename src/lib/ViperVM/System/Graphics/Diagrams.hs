
-- | Diagrams utilities
module ViperVM.System.Graphics.Diagrams
   ( rasterizeDiagram
   , VDiagram
   , VDiagram'
   , module Diagrams
   )
where

-- TODO
-- We might use Diagrams queries to handle mouse clicks, etc.
-- http://projects.haskell.org/diagrams/blog/2015-04-30-GTK-coordinates.html

import Diagrams
import Diagrams.Backend.Rasterific
import Codec.Picture.Types
import Data.Monoid (Any)

type VDiagram' n = QDiagram Rasterific V2 n Any
type VDiagram    = QDiagram Rasterific V2 Float Any

-- | Render a diagram into an image that can be displayed on a framebuffer
rasterizeDiagram :: (TypeableFloat n) => SizeSpec V2 n -> VDiagram' n -> Image PixelRGBA8
rasterizeDiagram spec = renderDia Rasterific (RasterificOptions spec)

