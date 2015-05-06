-- | Parser for /proc/*/maps
module ViperVM.Arch.Linux.Process.MemoryMap
   ( Entry (..)
   , readMaps
   , entryToBytestring
   )
where

import Text.ParserCombinators.Parsec.Number
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec
import Control.Monad (void)
import Data.Word (Word8,Word64)
import Data.Int (Int64)
import Data.ByteString.Unsafe
import Data.ByteString (ByteString)
import Foreign.Ptr (wordPtrToPtr)

data Entry = Entry
   { entryStartAddr :: Word64
   , entryStopAddr  :: Word64
   , entryPerms     :: [Perm]
   , entrySharing   :: Sharing
   , entryOffset    :: Word64
   , entryDevice    :: (Word8,Word8)
   , entryInode     :: Int64
   , entryPath      :: String
   } deriving (Show)

data Perm
   = PermRead
   | PermWrite
   | PermExec
   deriving (Show)

data Sharing
   = Shared 
   | Private
   deriving (Show)

-- | Read meminfo files
readMaps :: FilePath -> IO [Entry]
readMaps path = do
   procmaps <- readFile path

   case parse parseFile "" procmaps of
      Left err -> error ("proc maps parsing error: " ++ show err)
      Right v -> return v

   where
      parseFile = many parseLine
      parseLine = do
         start <- hexnum
         void (char '-')
         stop  <- hexnum
         void space
         perms <- do
            r <- (char 'r' *> return [PermRead]) <|> (char '-' *> return [])
            w <- (char 'w' *> return [PermWrite]) <|> (char '-' *> return [])
            x <- (char 'x' *> return [PermExec]) <|> (char '-' *> return [])
            return (r++w++x)
         sharing <- (char 'p' *> return Private)
                <|> (char 's' *> return Shared)
         void space
         offset <- hexnum
         void space
         dev <- do
            major <- hexnum
            _ <- char ':'
            minor <- hexnum
            return (major,minor)
         void space
         inode <- decimal
         void spaces
         pth <- manyTill anyChar (void newline <|> eof)
         return $ Entry start stop perms sharing offset dev inode pth


entryToBytestring :: Entry -> IO ByteString
entryToBytestring e = unsafePackCStringLen (ptr,len)
   where
      ptr = wordPtrToPtr (fromIntegral (entryStartAddr e))
      len = fromIntegral $ entryStopAddr e - entryStartAddr e
