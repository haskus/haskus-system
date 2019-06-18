-- | Control-groups
module Haskus.System.Linux.Process.ControlGroup
   ( ControlGroupEntry (..)
   , readControlGroup
   , parseControlGroup
   )
where

import Prelude hiding (takeWhile)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import Data.Void

import Haskus.Format.Binary.Buffer (bufferReadFile)
import Haskus.Utils.Text (Text)
import Haskus.Utils.Flow
import qualified Haskus.Utils.Text as Text

type Parser = Parsec Void Text

-- | Control group entry
data ControlGroupEntry = ControlGroupEntry
   { cgroupHierarchy  :: Int
   , cgroupSubsystems :: [Text]
   , cgroupOwner      :: Text
   } deriving (Show)

-- | Read /proc/[pid]/cgroup
readControlGroup :: FilePath -> IO [ControlGroupEntry]
readControlGroup p = do
   buf <- bufferReadFile p
   case parse parseControlGroup p (Text.bufferDecodeUtf8 buf) of
      Right v  -> return v
      Left err -> error ("control group parsing error: "++ show err)

-- | Read /proc/[pid]/maps files
parseControlGroup :: Parser [ControlGroupEntry]
parseControlGroup = parseFile
   where
      parseFile = do
         es <- many parseLine
         eof
         return es
      parseLine = do
         hier <- decimal
         void (char ':')
         subs <- Text.splitOn (Text.pack ",") . Text.pack <$> someTill anySingle (char ':')
         void (char ':')
         own  <- Text.pack <$> manyTill anySingle eol
         return $ ControlGroupEntry hier subs own
