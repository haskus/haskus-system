-- | Parser for /proc/*/maps
module ViperVM.Arch.Linux.Process.MemoryMap
   ( MemoryMapEntry (..)
   , readMemoryMap
   , parseMemoryMap
   , memoryMapToBufferList
   , memoryMapToBuffer
   )
where

import Prelude hiding (takeWhile)

import ViperVM.Format.Binary.Buffer
import ViperVM.Format.Binary.BufferList
import ViperVM.Format.Binary.Word
import ViperVM.Format.Text as Text

import Text.Megaparsec
import Text.Megaparsec.ByteString
import Text.Megaparsec.Lexer hiding (space)

import Control.Monad (void)
import Foreign.Ptr (wordPtrToPtr)

-- | Memory map entry
data MemoryMapEntry = MemoryMapEntry
   { entryStartAddr :: Word64
   , entryStopAddr  :: Word64
   , entryPerms     :: [Perm]
   , entrySharing   :: Sharing
   , entryOffset    :: Word64
   , entryDevice    :: (Word8,Word8)
   , entryInode     :: Int64
   , entryPath      :: Text
   } deriving (Show)

-- | Memory permission
data Perm
   = PermRead
   | PermWrite
   | PermExec
   deriving (Show)

-- | Memory sharing
data Sharing
   = Shared 
   | Private
   deriving (Show)

-- | Read /proc/[pid]/maps files
readMemoryMap :: FilePath -> IO [MemoryMapEntry]
readMemoryMap p = do
   r <- parseFromFile parseMemoryMap p
   case r of
      Right v  -> return v
      Left err -> error ("memory map parsing error: "++ show err)


-- | Parse /proc/[pid]/maps files
parseMemoryMap :: Parser [MemoryMapEntry]
parseMemoryMap = parseFile
   where
      parseFile = manyTill parseLine eof
      parseLine = do
         start <- fromIntegral <$> hexadecimal
         void (char '-')
         stop  <- fromIntegral <$> hexadecimal
         void spaceChar
         perms <- do
            r <- (char 'r' *> return [PermRead])  <|> (char '-' *> return [])
            w <- (char 'w' *> return [PermWrite]) <|> (char '-' *> return [])
            x <- (char 'x' *> return [PermExec])  <|> (char '-' *> return [])
            return (r++w++x)
         sharing <- (char 'p' *> return Private)
                <|> (char 's' *> return Shared)
         void spaceChar
         offset <- fromIntegral <$> hexadecimal
         void spaceChar
         dev <- do
            major <- fromIntegral <$> hexadecimal
            void (char ':')
            minor <- fromIntegral <$> hexadecimal
            return (major,minor)
         void spaceChar
         inode <- fromIntegral <$> decimal
         void (many (char ' '))
         pth <- Text.pack <$> manyTill anyChar eol
         return $ MemoryMapEntry start stop perms sharing offset dev inode pth

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
