-- | Control-groups
module ViperVM.Arch.Linux.Process.ControlGroup
   ( ControlGroupEntry (..)
   , readControlGroup
   , parseControlGroup
   )
where

import Prelude hiding (takeWhile)

import Text.Megaparsec
import Text.Megaparsec.ByteString
import Text.Megaparsec.Lexer hiding (space)

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad (void)

data ControlGroupEntry = ControlGroupEntry
   { cgroupHierarchy  :: Int
   , cgroupSubsystems :: [Text]
   , cgroupOwner      :: Text
   } deriving (Show)

-- | Read /proc/[pid]/cgroup
readControlGroup :: FilePath -> IO [ControlGroupEntry]
readControlGroup p = do
   r <- parseFromFile parseControlGroup p
   case r of
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
