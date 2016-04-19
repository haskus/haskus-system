{-# LANGUAGE DeriveAnyClass #-}

-- | Byte order ("endianness")
--
-- Indicate in which order bytes are stored in memory for multi-bytes types.
-- Big-endian means that most-significant bytes come first. Little-endian means
-- that least-significant bytes come first.
module ViperVM.Format.Binary.Endianness
   ( Endianness(..)
   , WordGetters (..)
   , WordPutters (..)
   , getWordGetters
   , getWordPutters
   , WordSize (..)
   , ExtendedWordGetters (..)
   , ExtendedWordPutters (..)
   , getExtendedWordGetters
   , getExtendedWordPutters
   )
where

import Data.Word
import ViperVM.Format.Binary.Get
import ViperVM.Format.Binary.Put
import ViperVM.Format.Binary.Enum

-- | Endianness
data Endianness 
   = LittleEndian    -- ^ Less significant bytes first
   | BigEndian       -- ^ Most significant bytes first
   deriving (Eq,Show,Enum,CEnum)

-- | Word getter
data WordGetters = WordGetters
   { wordGetter8  :: Get Word8   -- ^ Read a Word8
   , wordGetter16 :: Get Word16  -- ^ Read a Word16
   , wordGetter32 :: Get Word32  -- ^ Read a Word132
   , wordGetter64 :: Get Word64  -- ^ Read a Word64
   }

-- | Word putters
data WordPutters = WordPutters
   { wordPutter8  :: Word8  -> Put -- ^ Write a Word8
   , wordPutter16 :: Word16 -> Put -- ^ Write a Word16
   , wordPutter32 :: Word32 -> Put -- ^ Write a Word132
   , wordPutter64 :: Word64 -> Put -- ^ Write a Word64
   }

-- | Get getters for the given endianness
getWordGetters :: Endianness -> WordGetters
getWordGetters e = case e of
   LittleEndian -> WordGetters getWord8 getWord16le getWord32le getWord64le
   BigEndian    -> WordGetters getWord8 getWord16be getWord32be getWord64be

-- | Get putters for the given endianness
getWordPutters :: Endianness -> WordPutters
getWordPutters e = case e of
   LittleEndian -> WordPutters putWord8 putWord16le putWord32le putWord64le
   BigEndian    -> WordPutters putWord8 putWord16be putWord32be putWord64be



-- | Size of a machine word
data WordSize
   = WordSize32      -- ^ 32-bit
   | WordSize64      -- ^ 64-bit
   deriving (Show, Eq)

-- | Extended word getters
data ExtendedWordGetters = ExtendedWordGetters
   { extwordGetter8  :: Get Word8   -- ^ Read a Word8
   , extwordGetter16 :: Get Word16  -- ^ Read a Word16
   , extwordGetter32 :: Get Word32  -- ^ Read a Word132
   , extwordGetter64 :: Get Word64  -- ^ Read a Word64
   , extwordGetterN  :: Get Word64  -- ^ Read a native size word into a Word64
   }

-- | Extended word putters
data ExtendedWordPutters = ExtendedWordPutters
   { extwordPutter8  :: Word8  -> Put -- ^ Write a Word8
   , extwordPutter16 :: Word16 -> Put -- ^ Write a Word16
   , extwordPutter32 :: Word32 -> Put -- ^ Write a Word132
   , extwordPutter64 :: Word64 -> Put -- ^ Write a Word64
   , extwordPutterN  :: Word64 -> Put -- ^ Write a Word64 into a native size word
   }

-- | Return extended getters
getExtendedWordGetters :: Endianness -> WordSize -> ExtendedWordGetters
getExtendedWordGetters endian ws = ExtendedWordGetters gw8 gw16 gw32 gw64 gwN
   where
      WordGetters gw8 gw16 gw32 gw64 = getWordGetters endian
      gwN = case ws of
         WordSize64 -> gw64
         WordSize32 -> fromIntegral <$> gw32

-- | Return extended putters
getExtendedWordPutters :: Endianness -> WordSize -> ExtendedWordPutters
getExtendedWordPutters endian ws = ExtendedWordPutters pw8 pw16 pw32 pw64 pwN
   where
      WordPutters pw8 pw16 pw32 pw64 = getWordPutters endian
      pwN x = case ws of
         WordSize64 -> pw64 x
         WordSize32 -> if x > 0xffffffff
            then error $ "Number too big to be stored in 32-bit word ("++show x++")"
            else pw32 (fromIntegral x)

