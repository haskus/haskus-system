{-# LANGUAGE LambdaCase #-}
module ViperVM.Format.Compression.GZip
   ( Member(..)
   , decompressGet
   , decompress
   )
where

import qualified ViperVM.Format.Compression.Algorithms.Deflate as D

import Data.Sequence (Seq)
import Data.Word
import Data.Binary.Get
import Data.Binary.Bits.Get (block,bool,runBitGet,skipBits)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Lazy (ByteString)
import Text.Printf

data Member = Member 
   { memberFlags   :: Flags
   , memberTime    :: Word32
   , memberName    :: String
   , memberComment :: String
   , memberContent :: Seq Word8
   }


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

   flags <- getFlags
   mtime <- getWord32le
   xfl   <- getWord8
   os    <- getWord8

   when (flagExtra flags) $ do
      xlen <- getWord16le
      skip (fromIntegral xlen)

   name <- if flagName flags
      then unpack <$> getLazyByteStringNul
      else return ""

   comment <- if flagComment flags
      then unpack <$> getLazyByteStringNul
      else return ""

   crc <- if flagCRC flags
      then getWord16le
      else return 0

   content <- runBitGet D.decompress

   crc32 <- getWord32le
   isize <- getWord32le
      
   return $ Member flags mtime name comment content
      


data Flags = Flags
   { flagComment :: Bool
   , flagName    :: Bool
   , flagExtra   :: Bool
   , flagCRC     :: Bool
   , flagText    :: Bool
   } deriving (Show)

-- | Read flags
getFlags :: Get Flags
getFlags = runBitGet $ do
   skipBits 3
   block $ Flags <$> bool <*> bool <*> bool <*> bool <*> bool
