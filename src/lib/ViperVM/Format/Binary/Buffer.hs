{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A memory buffer
--
-- A buffer is a strict ByteString but with:
--   - a better interface: use Word instead of Int for sizes
--   - a better name: "string" is misleading
--   - some additional primitives
module ViperVM.Format.Binary.Buffer
   ( Buffer (..)
   , withBufferPtr
   , bufferSize
   , isBufferEmpty
   , emptyBuffer
   , bufferPeek
   , bufferMap
   , bufferReverse
   , bufferRead
   , bufferDrop
   , bufferTail
   , bufferAppend
   , bufferCons
   , bufferSnoc
   , bufferInit
   , bufferSplitOn
   , bufferHead
   , bufferIndex
   , bufferTake
   , bufferTakeWhile
   , bufferTakeAtMost
   -- * Packing/Unpacking
   , bufferPackByteString
   , bufferPackByteList
   , bufferPackStorable
   , bufferPackStorableList
   , bufferPackPtr
   , bufferUnpackByteList
   , bufferUnpackByteString
   -- * Unsafe
   , bufferUnsafeDrop
   , bufferUnsafeTake
   , bufferUnsafeTail
   , bufferUnsafeHead
   , bufferUnsafeLast
   , bufferUnsafeInit
   , bufferUnsafeIndex
   , bufferUnsafeMapMemory
   , bufferUnsafeUsePtr
   -- * IO
   , bufferReadFile
   , bufferWriteFile
   )
where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import System.IO.Unsafe
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Bits.Basic
import ViperVM.Utils.Memory (memCopy)

-- | A buffer
newtype Buffer = Buffer ByteString deriving (Eq,Ord)

instance Show Buffer where
   show b = concatMap bToHex (bufferUnpackByteList b)
      where
         bToHex x = toHex (x `shiftR` 4) ++ toHex (x .&. 0x0F)
         toHex 0xA = "A"
         toHex 0xB = "B"
         toHex 0xC = "C"
         toHex 0xD = "D"
         toHex 0xE = "E"
         toHex 0xF = "F"
         toHex x   = show x

-- | Unsafe: be careful if you modify the buffer contents or you may break
-- referential transparency
withBufferPtr :: Buffer -> (Ptr b -> IO a) -> IO a
withBufferPtr (Buffer bs) f = BS.unsafeUseAsCString bs (f . castPtr)

-- | Test if the buffer is empty
isBufferEmpty :: Buffer -> Bool
isBufferEmpty (Buffer bs) = BS.null bs

-- | Empty buffer
emptyBuffer :: Buffer
emptyBuffer = Buffer BS.empty

-- | Buffer size
bufferSize :: Buffer -> Word
bufferSize (Buffer bs) = 
      if s < 0
         then error "ByteString with size < 0"
         else fromIntegral s
   where
      s = BS.length bs

-- | Peek a storable
bufferPeek :: forall a. Storable a => Buffer -> a
bufferPeek buf
   | bufferSize buf < sza = error "bufferPeek: out of bounds"
   | otherwise            = unsafePerformIO $ withBufferPtr buf peek
   where
      sza = fromIntegral (sizeOf (undefined :: a)) 

-- | Map
bufferMap :: (Word8 -> Word8) -> Buffer -> Buffer
bufferMap f (Buffer bs) = Buffer (BS.map f bs)

-- | Reverse
bufferReverse :: Buffer -> Buffer
bufferReverse (Buffer bs) = Buffer (BS.reverse bs)

-- | Drop some bytes O(1)
bufferDrop :: Word -> Buffer -> Buffer
bufferDrop n (Buffer bs) = Buffer $ BS.drop (fromIntegral n) bs

-- | Split on the given Byte values
bufferSplitOn :: Word8 -> Buffer -> [Buffer]
bufferSplitOn n (Buffer bs) = fmap Buffer (BS.split n bs)

-- | Tail
bufferTail :: Buffer -> Buffer
bufferTail (Buffer bs) = Buffer $ BS.tail bs

-- | Append
bufferAppend :: Buffer -> Buffer -> Buffer
bufferAppend (Buffer a) (Buffer b) = Buffer $ BS.append a b

-- | Cons
bufferCons :: Word8 -> Buffer -> Buffer
bufferCons w (Buffer bs) = Buffer $ BS.cons w bs

-- | Snoc
bufferSnoc :: Buffer -> Word8 -> Buffer
bufferSnoc (Buffer bs) w = Buffer $ BS.snoc bs w


-- | Init
bufferInit :: Buffer -> Buffer
bufferInit (Buffer bs) = Buffer $ BS.init bs

-- | Head
bufferHead :: Buffer -> Word8
bufferHead (Buffer bs) = BS.head bs

-- | Index
bufferIndex :: Buffer -> Word -> Word8
bufferIndex (Buffer bs) n = BS.index bs (fromIntegral n)

-- | Unpack
bufferUnpackByteList :: Buffer -> [Word8]
bufferUnpackByteList (Buffer bs) = BS.unpack bs

-- | Unpack
bufferUnpackByteString :: Buffer -> ByteString
bufferUnpackByteString (Buffer bs) = bs

-- | Read a Storable and return the new buffer
bufferRead :: forall a. Storable a => Buffer -> (Buffer,a)
bufferRead buf
   | bufferSize buf < sza = error "bufferRead: out of bounds"
   | otherwise            = unsafePerformIO $ do
         a <- withBufferPtr buf peek
         return (bufferDrop sza buf, a)
   where
      sza = fromIntegral (sizeOf (undefined :: a)) 

-- | Take some bytes O(1)
bufferTake :: Word -> Buffer -> Buffer
bufferTake n (Buffer bs) = Buffer $ BS.take (fromIntegral n) bs

-- | Take some bytes O(n)
bufferTakeWhile :: (Word8 -> Bool) -> Buffer -> Buffer
bufferTakeWhile f (Buffer bs) = Buffer $ BS.takeWhile f bs

-- | Take some bytes O(1)
bufferTakeAtMost :: Word -> Buffer -> Buffer
bufferTakeAtMost n buf
   | bufferSize buf < n = buf
   | otherwise          = bufferTake n buf


-- | Pack a ByteString
bufferPackByteString :: BS.ByteString -> Buffer
bufferPackByteString = Buffer

-- | Pack a list of bytes
bufferPackByteList :: [Word8] -> Buffer
bufferPackByteList = Buffer . BS.pack

-- | Pack a Storable
bufferPackStorable :: forall a. Storable a => a -> Buffer
bufferPackStorable x = Buffer $ unsafePerformIO $ do
   let sza = sizeOf (undefined :: a)
   p <- malloc
   poke p x
   BS.unsafePackMallocCStringLen (castPtr p, sza)

-- | Pack a list of Storable
bufferPackStorableList :: forall a. Storable a => [a] -> Buffer
bufferPackStorableList xs = Buffer $ unsafePerformIO $ do
   let 
      sza = sizeOf (undefined :: a)
      lxs = length xs
   p <- mallocArray lxs
   forM_ (xs `zip` [0..]) $ \(x,o) ->
      pokeElemOff p o x
   BS.unsafePackMallocCStringLen (castPtr p, sza * lxs)

-- | Pack from a pointer
bufferPackPtr :: Word -> Ptr () -> IO Buffer
bufferPackPtr sz ptr = do
   p <- mallocBytes (fromIntegral sz)
   memCopy p ptr (fromIntegral sz)
   Buffer <$> BS.unsafePackMallocCStringLen (castPtr p, fromIntegral sz)

-- | Unsafe drop (don't check the size)
bufferUnsafeDrop :: Word -> Buffer -> Buffer
bufferUnsafeDrop n (Buffer bs) = Buffer (BS.unsafeDrop (fromIntegral n) bs)

-- | Unsafe take (don't check the size)
bufferUnsafeTake :: Word -> Buffer -> Buffer
bufferUnsafeTake n (Buffer bs) = Buffer (BS.unsafeTake (fromIntegral n) bs)

-- | Unsafe tail (don't check the size)
bufferUnsafeTail :: Buffer -> Buffer
bufferUnsafeTail (Buffer bs) = Buffer (BS.unsafeTail bs)

-- | Unsafe head (don't check the size)
bufferUnsafeHead :: Buffer -> Word8
bufferUnsafeHead (Buffer bs) = BS.unsafeHead bs

-- | Unsafe last (don't check the size)
bufferUnsafeLast :: Buffer -> Word8
bufferUnsafeLast (Buffer bs) = BS.unsafeLast bs

-- | Unsafe init (don't check the size)
bufferUnsafeInit :: Buffer -> Buffer
bufferUnsafeInit (Buffer bs) = Buffer (BS.unsafeInit bs)

-- | Unsafe index (don't check the size)
bufferUnsafeIndex :: Buffer -> Word -> Word8
bufferUnsafeIndex (Buffer bs) n = BS.unsafeIndex bs (fromIntegral n)

-- | Map memory
bufferUnsafeMapMemory :: Word -> Ptr () -> IO Buffer
bufferUnsafeMapMemory sz ptr =
   Buffer <$> BS.unsafePackMallocCStringLen (castPtr ptr, fromIntegral sz)

-- | Use buffer pointer
bufferUnsafeUsePtr :: Buffer -> (Ptr () -> Word -> IO a) -> IO a
bufferUnsafeUsePtr bu@(Buffer b) f = BS.unsafeUseAsCString b $ \p -> f (castPtr p) (bufferSize bu)

-- | Read file
bufferReadFile :: FilePath -> IO Buffer
bufferReadFile path = Buffer <$> BS.readFile path

-- | Write file
bufferWriteFile :: FilePath -> Buffer -> IO ()
bufferWriteFile path (Buffer bs) =BS.writeFile path bs
