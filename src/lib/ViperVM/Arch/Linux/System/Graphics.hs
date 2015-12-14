-- | Manage graphics devices in SysFS
module ViperVM.Arch.Linux.System.Graphics
   ( GraphicCard(..)
   , loadGraphicCards
   )
where

import ViperVM.Arch.Linux.System.System
import ViperVM.Arch.Linux.FileSystem.Directory
import ViperVM.Arch.Linux.FileSystem.ReadWrite
import ViperVM.Arch.Linux.FileSystem.OpenClose
import ViperVM.Arch.Linux.ErrorCode
import qualified ViperVM.Format.Binary.BitSet as BitSet

import Prelude hiding (init,tail)
import Control.Monad.Trans.Either
import System.FilePath ((</>))
import Control.Monad (forM,void)
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
loadGraphicCards :: System -> SysRet [GraphicCard]
loadGraphicCards system = do
   -- open drm directory
   withOpenAt (systemSysFS system) "class/drm" [OpenReadOnly] BitSet.empty $ \fd -> runEitherT $ do

      -- detect cardN directories
      -- FIXME: the fd is not closed in case of error
      dirs <- EitherT $ listDirectory fd
      let
         parseCard = void (string "card" >> decimal)
         isCard    = isJust . parseMaybe parseCard
         cardDirs  = filter isCard (fmap entryName dirs)

      forM cardDirs $ \dir -> do
         -- read device major and minor in "dev" file
         -- content format is: MMM:mmm\n (where M is major and m is minor)
         EitherT $ withOpenAt fd (dir </> "dev") [OpenReadOnly] BitSet.empty $ \devfd -> runEitherT $ do
            content <- EitherT $ readByteString devfd 16 -- 16 bytes should be enough
            let 
               parseDevFile = do
                  major' <- fromIntegral <$> decimal
                  void (char ':')
                  minor' <- fromIntegral <$> decimal
                  return (major',minor')
               dev = case parseMaybe parseDevFile content of
                  Nothing      -> error "Invalid dev file format"
                  Just (ma,mi) -> Device ma mi
               devid = read (drop 4 dir)
               path  = "class/drm" </> dir
               card  = GraphicCard path dev devid
            return card
