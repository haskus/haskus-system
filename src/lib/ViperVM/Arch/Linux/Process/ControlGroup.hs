-- | Control-groups
module ViperVM.Arch.Linux.Process.ControlGroup
   ( ControlGroupEntry (..)
   , readControlGroup
   , parseControlGroup
   )
where

import Prelude hiding (takeWhile)

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Control.Monad (void)

data ControlGroupEntry = ControlGroupEntry
   { cgroupHierarchy  :: Int
   , cgroupSubsystems :: [Text]
   , cgroupOwner      :: Text
   } deriving (Show)

-- | Read /proc/[pid]/cgroup
readControlGroup :: FilePath -> IO [ControlGroupEntry]
readControlGroup = fmap parseControlGroup . BS.readFile

-- | Read /proc/[pid]/maps files
parseControlGroup :: ByteString -> [ControlGroupEntry]
parseControlGroup bs = do
   case parseOnly parseFile bs of
      Right v  -> v
      Left err -> error ("control group parsing error: "++ err)

   where
      parseFile = (parseLine `sepBy'` endOfLine)
      parseLine = do
         hier <- decimal
         void (char ':')
         subs <- Text.splitOn (Text.pack ",") . decodeUtf8 <$> takeWhile (/= ':')
         void (char ':')
         own  <- decodeUtf8 <$> takeWhile (/= '\n')
         return $ ControlGroupEntry hier subs own
