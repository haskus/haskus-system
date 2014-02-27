-- | SysFS (Linux) management module
module ViperVM.Platform.Host.SysFS (
   readMemInfo,
   CPUMap(..), readCPUMap, member, toList
) where

import Control.Applicative ((<$>), (<*))
import Control.Monad (void)
import Text.ParserCombinators.Parsec.Number
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec
import Data.Word
import Data.Bits
import Data.Maybe (isJust,mapMaybe)
import qualified Data.Map as Map
import qualified Data.Vector as V

data CPUMap = CPUMap (V.Vector Word32) deriving (Show)

-- | Read meminfo files
readMemInfo :: FilePath -> IO (Map.Map String Word64)
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


-- | Read cpumap files
readCPUMap :: FilePath -> IO CPUMap
readCPUMap path = do
   f <- readFile path

   CPUMap . V.fromList . reverse . dropWhile (== 0) <$> case parse parseFile "" f of
      Left err -> error ("cpumap parsing error: " ++ show err)
      Right v -> return v

   where
      parseFile = sepBy1 hexnum (char ',') <* newline <* eof

-- | Check that a CPU belongs to a CPU Map
member :: Word -> CPUMap -> Bool
member idx (CPUMap v) = q < V.length v && testBit (v V.! q) r
   where
      (q,r) = fromIntegral idx `quotRem` 32


toList :: CPUMap -> [Word]
toList (CPUMap v) = go 0 (V.toList v)
   where
      go _ [] = []
      go n (x:xs) = mapMaybe (f x n) [0..31] ++ go (n+1) xs
      f x n idx = if testBit x idx then Just (n * 32 + fromIntegral idx) else Nothing
