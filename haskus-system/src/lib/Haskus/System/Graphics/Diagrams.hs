{-# LANGUAGE FlexibleContexts #-}

-- | Diagrams utilities (specialized for the rasterific backend)
module Haskus.System.Graphics.Diagrams
   ( rasterizeDiagram
   , VDiagram
   , VDiagram'
   , module Diagrams
   , module Diagrams.Prelude
   , text'
   , text
   )
where

-- TODO
-- We might use Diagrams queries to handle mouse clicks, etc.
-- http://projects.haskell.org/diagrams/blog/2015-04-30-GTK-coordinates.html

import Data.Typeable
import Diagrams.Prelude hiding ((|>),(<|),text)
import Diagrams hiding (text)
import Diagrams.TwoD.Text (FontSlant(..),FontWeight(..),Text(..))
import Diagrams.Backend.Rasterific
import Codec.Picture.Types

type VDiagram' n = QDiagram Rasterific V2 n Any
type VDiagram    = QDiagram Rasterific V2 Float Any

-- | Render a diagram into an image that can be displayed on a framebuffer
rasterizeDiagram :: (RealFloat n, Typeable n) => SizeSpec V2 n -> VDiagram' n -> Image PixelRGBA8
rasterizeDiagram spec = renderDia Rasterific (RasterificOptions spec)

-- | Create a primitive text diagram from the given FontSlant, FontWeight, and
-- string, with baseline alignment, envelope and trace based on the BoundingBox
-- of the text.
text' :: (RealFloat n, Typeable n, Renderable (Text n) b) => FontSlant -> FontWeight -> String -> QDiagram b V2 n Any
text' = texterific'


-- | Create a primitive text diagram from the given string, with baseline
-- alignment, envelope and trace based on the BoundingBox of the text. 
text :: (RealFloat n, Typeable n, Renderable (Text n) b) => String -> QDiagram b V2 n Any
text = texterific
