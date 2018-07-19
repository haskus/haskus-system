{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | GZip compression
module Haskus.Format.Compression.GZip
   ( Member(..)
   , Flag(..)
   , Flags
   , decompressGet
   , decompress
   )
where

import Data.Foldable (toList)

import qualified Haskus.Format.Compression.Algorithms.Deflate as D
import Haskus.Format.Binary.Get as Get
import Haskus.Format.Binary.Bits.Order
import Haskus.Format.Binary.Buffer
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.BitSet (BitSet,CBitSet)
import qualified Haskus.Format.Binary.BitSet as BitSet
import qualified Haskus.Format.Text as Text
import Haskus.Format.Text (Text,getTextUtf8Nul,unpack)
import Haskus.Format.Text (textFormat,shown,hex,(%))
import Haskus.Utils.Flow (when)

-- | Member file
data Member = Member 
   { memberFlags        :: Flags
   , memberTime         :: Word32
   , memberExtraFlags   :: Word8
   , memberOS           :: Word8
   , memberName         :: Text
   , memberComment      :: Text
   , memberContent      :: Buffer
   , memberCRC          :: Word16
   , memberCRC32        :: Word32
   , memberSize         :: Word32   -- ^ uncompressed input size (module 1^32)
   }
   deriving (Show)


-- | Decompress the members of the archive
decompress :: Buffer -> [Member]
decompress = runGetOrFail decompressGet

-- | Decompress the members of the archive
decompressGet :: Get [Member]
decompressGet = rec []
   where
      rec xs = Get.isEmpty >>= \case
         True  -> return (reverse xs)
         False -> do
            x <- getMember
            rec (x:xs)


-- | Get a member of the archive
getMember :: Get Member
getMember = do
   id1   <- getWord8
   id2   <- getWord8
   when (id1 /= 0x1f || id2 /= 0x8b) $
      error $ unpack $ textFormat ("Invalid archive file: " % hex % " " % hex) id1 id2

   comp  <- getWord8
   when (comp /= 8) $
      error "Unknown compression method"

   flags <- BitSet.fromBits <$> getWord8
   mtime <- getWord32le   -- modification time
   xfl   <- getWord8      -- extra flags
   os    <- getWord8      -- os

   when (BitSet.member flags FlagExtra) $ do
      xlen <- getWord16le
      skip (fromIntegral xlen)

   name <- if BitSet.member flags FlagName
      then getTextUtf8Nul
      else return Text.empty

   comment <- if BitSet.member flags FlagComment
      then getTextUtf8Nul
      else return Text.empty

   crc <- if BitSet.member flags FlagCRC
      then getWord16le
      else return 0

   getBitGet BB D.decompress $ \content -> do

      crc32 <- getWord32le
      isize <- getWord32le
         
      return $ Member flags mtime xfl os name comment
                  (bufferPackByteList (toList content)) crc crc32 isize
      

-- | Information flag
data Flag
   = FlagText
   | FlagCRC
   | FlagExtra
   | FlagName
   | FlagComment
   deriving (Show,Eq,Enum,CBitSet)

-- | Flags
type Flags = BitSet Word8 Flag
