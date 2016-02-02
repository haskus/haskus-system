-- | Manage graphics devices
module ViperVM.System.Graphics
   ( GraphicCard(..)
   , loadGraphicCards
   )
where

import ViperVM.System.System
import ViperVM.Arch.Linux.FileSystem.OpenClose
import ViperVM.Arch.Linux.Error

import Prelude hiding (init,tail)
import Control.Monad (void)
import Data.Maybe (isJust)

import Text.Megaparsec
import Text.Megaparsec.Lexer hiding (space)

-- | Graphic card
data GraphicCard = GraphicCard
   { graphicCardPath    :: FilePath    -- ^ Path to the graphic card in SysFS
   , graphicCardDev     :: Device      -- ^ Device major/minor to create the device file descriptor
   , graphicCardID      :: Int         -- ^ Card identifier
   } deriving (Show)


-- | Return detected graphic cards
--
-- Graphic cards are /class/drm/cardN directories in SysFS where N is the card
-- identifier. The this directory, the dev file contains device major/minor to
-- create appropriate device node.
loadGraphicCards :: System -> Sys [GraphicCard]
loadGraphicCards system = sysLogSequence "Load graphic cards" $ do
      devs <- listDevicesWithClass system "drm" isCard
      return $ fmap makeCard devs
   where
      parseCard = void (string "card" >> decimal)
      isCard    = isJust . parseMaybe parseCard
      makeCard (path,dev) = GraphicCard path dev devid
         where  devid = read (drop 4 path)
