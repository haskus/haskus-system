-- | Parser for /proc/*/maps
module Haskus.System.Linux.Process.MemoryMap
   ( MemoryMapEntry (..)
   , MappingType (..)
   , Perm (..)
   , Sharing (..)
   , readMemoryMap
   , parseMemoryMap
   , memoryMapParser
   , memoryMapToBufferList
   , memoryMapToBuffer
   )
where

import Prelude hiding (takeWhile)

import Haskus.Format.Binary.Buffer
import Haskus.Format.Binary.BufferList
import Haskus.Format.Binary.Word
import Foreign.Ptr (wordPtrToPtr)
import qualified Haskus.Utils.Text as Text
import Haskus.Utils.Text (Text,bufferDecodeUtf8)
import Haskus.Utils.Flow

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser = Parsec Void Text

-- | Memory map entry
data MemoryMapEntry = MemoryMapEntry
   { entryStartAddr :: Word64        -- ^ Starting address
   , entryStopAddr  :: Word64        -- ^ End address
   , entryPerms     :: [Perm]        -- ^ Permissions
   , entrySharing   :: Sharing       -- ^ Shared or copy-on-write
   , entryType      :: MappingType   -- ^ Type of mapping
   } deriving (Show)

-- | Type of memory mapping
data MappingType
   = AnonymousMapping   -- ^ Anonymous mapping
   | NamedMapping Text  -- ^ Mapping with a name
   -- | File mapping
   | FileMapping
      { fileMappingDevice :: (Word8,Word8) -- ^ Device containing the inode
      , fileMappingInode  :: Int64         -- ^ Inode
      , fileMappingPath   :: Text          -- ^ File path
      , fileMappingOffset :: Word64        -- ^ Offset in the file
      }
   deriving (Show,Eq)

-- | Memory permission
data Perm
   = PermRead  -- ^ Read allowed
   | PermWrite -- ^ Write allowed
   | PermExec  -- ^ Execute allowed
   deriving (Eq,Show)

-- | Memory sharing
data Sharing
   = Shared    -- ^ Shared
   | Private   -- ^ Private (copy-on-write)
   deriving (Eq,Show)

-- | Read /proc/[pid]/maps files
readMemoryMap :: FilePath -> IO [MemoryMapEntry]
readMemoryMap p = parseMemoryMap <$> bufferReadFile p

-- | Parse a memory map in a buffer
parseMemoryMap :: Buffer -> [MemoryMapEntry]
parseMemoryMap b = 
   case runParser memoryMapParser "" (bufferDecodeUtf8 b) of
      Right v  -> v
      Left err -> error ("memory map parsing error: "++ show err)

-- | Parse /proc/[pid]/maps files
memoryMapParser :: Parser [MemoryMapEntry]
memoryMapParser = parseFile
   where
      parseFile = manyTill parseLine eof
      parseLine = do
         start <- hexadecimal
         void (char '-')
         stop  <- hexadecimal
         void spaceChar
         perms <- do
            r <- (char 'r' *> return [PermRead])  <|> (char '-' *> return [])
            w <- (char 'w' *> return [PermWrite]) <|> (char '-' *> return [])
            x <- (char 'x' *> return [PermExec])  <|> (char '-' *> return [])
            return (r++w++x)
         sharing <- (char 'p' *> return Private)
                <|> (char 's' *> return Shared)
         void spaceChar
         offset <- hexadecimal
         void spaceChar
         dev <- do
            major <- hexadecimal
            void (char ':')
            minor <- hexadecimal
            return (major,minor)
         void spaceChar
         inode <- decimal
         void (many (char ' '))
         pth <- Text.pack <$> manyTill anySingle eol
         let typ = case (inode, Text.null pth) of
                     (0,True)  -> AnonymousMapping
                     (0,False) -> NamedMapping pth
                     _         -> FileMapping dev inode pth offset
         return $ MemoryMapEntry start stop perms sharing typ

-- | Convert a memory-map entry into a BufferList
memoryMapToBufferList :: MemoryMapEntry -> IO BufferList
memoryMapToBufferList = fmap toBufferList . memoryMapToBuffer

-- | Convert a memory-map entry into a Buffer
--
-- Warning: The buffer directly maps the entry (i.e. there is no copy of the
-- data). Hence the referential transparency can be broken if the entry is
-- written into 
memoryMapToBuffer :: MemoryMapEntry -> IO Buffer
memoryMapToBuffer e = bufferUnsafeMapMemory len ptr
   where
      ptr = wordPtrToPtr (fromIntegral (entryStartAddr e))
      len = fromIntegral $ entryStopAddr e - entryStartAddr e
