-- | Manage graphics devices in SysFS
module ViperVM.Arch.Linux.System.Graphics
   ( GraphicCard(..)
   , loadGraphicCards
   )
where

import ViperVM.Arch.Linux.System.SysFS
import ViperVM.Arch.Linux.FileSystem.Directory
import ViperVM.Arch.Linux.FileSystem.ReadWrite
import ViperVM.Arch.Linux.FileSystem.OpenClose
import ViperVM.Arch.Linux.ErrorCode

import Prelude hiding (init,tail)
import Control.Monad.Trans.Either
import System.FilePath ((</>))
import Data.List (isPrefixOf)
import Control.Monad (forM)
import Control.Arrow ((***))
import Data.ByteString (breakByte,init,tail)
import Data.ByteString.Char8 (unpack)
import Data.Char (ord)

data GraphicCard = GraphicCard
   { graphicCardPath    :: FilePath
   , graphicCardDev     :: Device
   , graphicCardID      :: Int
   } deriving (Show)


-- | Return detected graphic cards
--
-- Graphic cards are /class/drm/cardN directories in SysFS
loadGraphicCards :: SysFS -> SysRet [GraphicCard]
loadGraphicCards sysfs = do
   -- open drm directory
   withOpenAt (sysfsDescriptor sysfs) "class/drm" [OpenReadOnly] [] $ \fd -> runEitherT $ do

      -- detect cardN directories
      -- FIXME: the fd is not closed in case of error
      dirs <- EitherT $ listDirectory fd
      let cardDirs = filter ("card" `isPrefixOf`) (fmap entryName dirs)

      forM cardDirs $ \dir -> do
         -- read device major and minor in "dev" file
         -- content format is: MMM:mmm\n (where M is major and m is minor)
         EitherT $ withOpenAt fd (dir </> "dev") [OpenReadOnly] [] $ \devfd -> runEitherT $ do
            content <- EitherT $ readByteString devfd 16 -- 16 bytes should be enough
            let 
               f = read . unpack
               sep = fromIntegral (ord ':')
               (major,minor) = (f *** (f . tail)) (breakByte sep (init content))
               dev = Device major minor
               devid = read (drop 4 dir)
               path  = "class/drm" </> dir
               card  = GraphicCard path dev devid
            return card
