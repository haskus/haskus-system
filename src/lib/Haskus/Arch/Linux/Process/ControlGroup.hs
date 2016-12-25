-- | Control-groups
module Haskus.Arch.Linux.Process.ControlGroup
   ( ControlGroupEntry (..)
   , readControlGroup
   , parseControlGroup
   )
where

import Prelude hiding (takeWhile)

import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Lexer hiding (space)

import Haskus.Format.Binary.Buffer (bufferReadFile)
import Haskus.Format.Text (Text)
import Haskus.Utils.Flow
import qualified Haskus.Format.Text as Text

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
         hier <- fromIntegral <$> decimal
         void (char ':')
         subs <- Text.splitOn (Text.pack ",") . Text.pack <$> someTill anyChar (char ':')
         void (char ':')
         own  <- Text.pack <$> manyTill anyChar eol
         return $ ControlGroupEntry hier subs own
