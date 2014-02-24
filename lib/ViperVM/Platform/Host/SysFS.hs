-- | SysFS (Linux) management module
module ViperVM.Platform.Host.SysFS (
   readMemInfo
) where

import Control.Applicative ((<$>))
import Control.Monad (void)
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec
import Data.Word
import Data.Maybe (isJust)
import Data.Map as Map

-- | Read
readMemInfo :: FilePath -> IO (Map String Word64)
readMemInfo path = do
   meminfo <- readFile path

   Map.fromList <$> case parse parseFile "" meminfo of
      Left err -> error ("meminfo parsing error: " ++ show err)
      Right v -> return v

   where
      parseFile = many1 parseLine
      parseLine = do
         void (string "Node ")
         void (many1 digit)
         void (char ' ')
         lbl <- manyTill anyChar (char ':')
         skipMany1 space
         value <- read <$> many1 digit
         kb <- optionMaybe (try(string " kB"))
         void (void newline <|> eof)
         let f x = if isJust kb then x * 1024 else x
         return (lbl, f value)
