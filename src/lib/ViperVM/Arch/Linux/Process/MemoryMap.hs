-- | Parser for /proc/*/maps
module ViperVM.Arch.Linux.Process.MemoryMap
   ( Entry (..)
   , readMemoryMap
   , parseMemoryMap
   , entryToBytestring
   )
where

import Prelude hiding (takeWhile)

import Data.Attoparsec.ByteString.Char8
import Control.Monad (void)
import Data.Word (Word8,Word64)
import Data.Int (Int64)
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe
import Data.ByteString (ByteString)
import Foreign.Ptr (wordPtrToPtr)
import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

data Entry = Entry
   { entryStartAddr :: Word64
   , entryStopAddr  :: Word64
   , entryPerms     :: [Perm]
   , entrySharing   :: Sharing
   , entryOffset    :: Word64
   , entryDevice    :: (Word8,Word8)
   , entryInode     :: Int64
   , entryPath      :: Text
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

readMemoryMap :: FilePath -> IO [Entry]
readMemoryMap = fmap parseMemoryMap . BS.readFile

-- | Read meminfo files
parseMemoryMap :: ByteString -> [Entry]
parseMemoryMap bs = do
   case parseOnly parseFile bs of
      Right v  -> v
      Left err -> error ("memory map parsing error: "++ err)

   where
      parseFile = parseLine `sepBy'` endOfLine
      parseLine = do
         start <- hexadecimal
         void (char '-')
         stop  <- hexadecimal
         skipSpace
         perms <- do
            r <- (char 'r' *> return [PermRead])  <|> (char '-' *> return [])
            w <- (char 'w' *> return [PermWrite]) <|> (char '-' *> return [])
            x <- (char 'x' *> return [PermExec])  <|> (char '-' *> return [])
            return (r++w++x)
         sharing <- (char 'p' *> return Private)
                <|> (char 's' *> return Shared)
         skipSpace
         offset <- hexadecimal
         skipSpace
         dev <- do
            major <- hexadecimal
            _ <- char ':'
            minor <- hexadecimal
            return (major,minor)
         skipSpace
         inode <- decimal
         skipMany1 space
         pth <- takeWhile (/= '\n')
         return $ Entry start stop perms sharing offset dev inode (decodeUtf8 pth)


entryToBytestring :: Entry -> IO ByteString
entryToBytestring e = unsafePackCStringLen (ptr,len)
   where
      ptr = wordPtrToPtr (fromIntegral (entryStartAddr e))
      len = fromIntegral $ entryStopAddr e - entryStartAddr e
