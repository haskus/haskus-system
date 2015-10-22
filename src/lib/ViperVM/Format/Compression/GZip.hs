{-# LANGUAGE LambdaCase #-}
module ViperVM.Format.Compression.GZip
   ( Member(..)
   , Flag(..)
   , Flags
   , decompressGet
   , decompress
   )
where

import qualified ViperVM.Format.Compression.Algorithms.Deflate as D

import Data.Foldable (toList)
import Data.Word
import ViperVM.Format.Binary.Get
import ViperVM.Utils.BitSet (BitSet,EnumBitSet)
import Data.Binary.Bits.Get (runBitGet)
import qualified ViperVM.Utils.BitSet as BitSet
import Control.Monad (when)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Lazy (pack,ByteString)
import Text.Printf

data Member = Member 
   { memberFlags        :: Flags
   , memberTime         :: Word32
   , memberExtraFlags   :: Word8
   , memberOS           :: Word8
   , memberName         :: String
   , memberComment      :: String
   , memberContent      :: ByteString
   , memberCRC          :: Word16
   , memberCRC32        :: Word32
   , memberSize         :: Word32   -- ^ uncompressed input size (module 1^32)
   }
   deriving (Show)


-- | Decompress the members of the archive
decompress :: ByteString -> [Member]
decompress = runGet decompressGet

-- | Decompress the members of the archive
decompressGet :: Get [Member]
decompressGet = rec []
   where
      rec xs = isEmpty >>= \case
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
      error $ printf "Invalid archive file: %x %x" id1 id2

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
      then unpack <$> getLazyByteStringNul
      else return ""

   comment <- if BitSet.member flags FlagComment
      then unpack <$> getLazyByteStringNul
      else return ""

   crc <- if BitSet.member flags FlagCRC
      then getWord16le
      else return 0

   content <- runBitGet D.decompress

   crc32 <- getWord32le
   isize <- getWord32le
      
   return $ Member flags mtime xfl os name comment (pack (toList content)) crc crc32 isize
      


data Flag
   = FlagText
   | FlagCRC
   | FlagExtra
   | FlagName
   | FlagComment
   deriving (Show,Eq,Enum)

instance EnumBitSet Flag

type Flags = BitSet Word8 Flag

