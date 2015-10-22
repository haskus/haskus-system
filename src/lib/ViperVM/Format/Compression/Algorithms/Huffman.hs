{-# LANGUAGE LambdaCase #-}

-- | Implement Huffman coding
module ViperVM.Format.Compression.Algorithms.Huffman
   ( 
   -- * Huffman Tree
     Tree(..)
   , computeHuffmanTreeFromFoldable
   , computeHuffmanTreeFromPriorityQueue
   , computeHuffmanTreeFromCodes

   -- * Coding table
   , Encoder (..)
   , inverseEncoder
   , stringEncoder
   , textEncoder
   , binaryEncoder
   , buildCodingTable

   -- * Huffman Code
   , Code(..)
   , emptyCode
   , codeAdd
   , codeShiftL
   , codeShiftR
   , codeTestBit
   , codeReverseBits
   , codeAppend

   -- * Encoding
   , makeBitGet
   , toBinary
   , fromBinary
   , fromBinaryLen

   -- * Helpers
   , computePriorityTable
   , computePriorityQueue
   )
where

import Prelude hiding (mapM_)

import Data.Foldable (foldl', mapM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQueue
import Data.Word
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tuple (swap)
import Data.Bits
import Data.Binary.Bits.Get as BitGet
import ViperVM.Format.Binary.Get as Get
import ViperVM.Format.Binary.BitPut
import ViperVM.Format.Binary.BitOrder
import ViperVM.Format.Binary.BitOps (reverseLeastBits)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString      as BS
import Control.Arrow (first)

-- | Priority (number of occurences)
type Priority = Word64
type PriorityTable a = Map a Priority
type PriorityQueue a = MinPQueue Priority a

-- | Compute a priority table (i.e. number of occurences for each element)
computePriorityTable :: (Foldable m, Eq a, Ord a) => m a -> PriorityTable a
computePriorityTable = foldl' f Map.empty 
   where
      f ocs k = Map.insertWith' (+) k 1 ocs

-- | Build min priority queue (priority is number of occurences)
computePriorityQueue :: PriorityTable a -> PriorityQueue a
computePriorityQueue = PQueue.fromList . fmap swap . Map.toList

--------------------------------------------------
-- Tree
--------------------------------------------------

-- | A binary tree
data Tree a
   = Node (Tree a) (Tree a)
   | Leaf a
   | Empty
   deriving (Eq,Show)

instance Functor Tree where
   fmap _ Empty        = Empty
   fmap f (Leaf a)     = Leaf (f a)
   fmap f (Node t1 t2) = Node (fmap f t1) (fmap f t2)

-- | Build the Huffman tree
computeHuffmanTreeFromPriorityQueue :: PriorityQueue a -> Tree a
computeHuffmanTreeFromPriorityQueue pq = rec pq'
   where
      pq'   = PQueue.map Leaf pq
      rec q = case PQueue.size q of
         0 -> Empty
         1 -> snd (PQueue.findMin q)
         _ -> rec q''' where
                  q'  = PQueue.deleteMin q
                  q'' = PQueue.deleteMin q'
                  (k1,m1) = PQueue.findMin q
                  (k2,m2) = PQueue.findMin q'
                  q'''= PQueue.insert (k1+k2) (Node m1 m2) q''

-- | Create a Huffman tree
computeHuffmanTreeFromFoldable :: (Foldable m, Eq a, Ord a) => m a -> Tree a
computeHuffmanTreeFromFoldable = computeHuffmanTreeFromPriorityQueue . computePriorityQueue . computePriorityTable

-- | Build a tree from a set of codes
computeHuffmanTreeFromCodes :: Show a => [(Code,a)] -> Tree a
computeHuffmanTreeFromCodes = rec . fmap (first codeReverseBits)
   where
      rec [] = Empty
      rec ps = case ff [] [] [] ps of
         ([],lp,rp)  -> Node (rec lp) (rec rp)
         ([v],[],[]) -> Leaf v
         x           -> error $ "Invalid (ambiguous) codes: " ++ show x


      -- characterize codes in three ways:
      --    * empty codes
      --    * codes starting with 0
      --    * codes starting with 1
      ff ep lp rp [] = (ep,lp,rp)
      ff ep lp rp ((c,v):ps)
         | codeLength c == 0 = ff (v:ep) lp rp ps
         | codeTestBit c 0   = ff ep ((codeShiftR 1 c,v):lp) rp ps
         | otherwise         = ff ep lp ((codeShiftR 1 c,v):rp) ps

--------------------------------------------------
-- Coding table
--------------------------------------------------

-- | Build a coding table for a Huffman tree
buildCodingTable :: Ord a => Encoder b -> Tree a -> Map a b
buildCodingTable (Encoder left right op) tree = rec Nothing tree
   where
      rec Nothing t = case t of
         (Node l r) -> Map.union (rec (Just left)  l) 
                                 (rec (Just right) r)
         (Leaf x)   -> Map.singleton x left -- arbitrarily chose left
         Empty      -> Map.empty

      rec (Just cur) t = case t of
         (Node l r) -> Map.union (rec (Just $ cur `op` left)  l) 
                                 (rec (Just $ cur `op` right) r)
         (Leaf x)   -> Map.singleton x cur
         Empty      -> Map.empty

-- | Huffman tree encoder
data Encoder a = Encoder
   { encodeLeft   :: a
   , encodeRight  :: a
   , encodeAppend :: a -> a -> a
   }

-- | Inverse left and right in the given encoder
inverseEncoder :: Encoder a -> Encoder a
inverseEncoder (Encoder l r a) = Encoder r l a

-- | String encoder ("0" and "1" chars)
stringEncoder :: Encoder String
stringEncoder = Encoder "0" "1" (++)

-- | Text encoder ("0" and "1" chars)
textEncoder :: Encoder Text
textEncoder = Encoder (Text.pack "0") (Text.pack "1") Text.append

-- | Binary encoder
binaryEncoder :: Encoder Code
binaryEncoder = Encoder (Code 1 0) (Code 1 1) codeAppend

--------------------------------------------------
-- Encoding
--------------------------------------------------

-- | Create a binary parser that reads a single encoded element. Use the given
-- Tree to decode it.
--
-- You can specify which tree side (left or right) is 0
makeBitGet :: Bool -> Tree a -> BitGet (Maybe a)
makeBitGet leftIsZero tree = rec tree
   where
      rec (Leaf x)   = return (Just x)
      rec Empty      = return Nothing
      rec (Node l r) = do
         empty <- BitGet.isEmpty
         if empty
            then return Nothing
            else do
               b <- getBool
               if b `xor` leftIsZero
                  then rec l
                  else rec r

-- | Convert a binary sequence into a token sequence
fromBinary :: Bool -> Tree a -> LBS.ByteString -> [a]
fromBinary leftIsZero tree = runGet (runBitGet (g []))
   where
      bg    = makeBitGet leftIsZero tree
      g xs  = bg >>= \case
         Nothing -> return (reverse xs)
         Just x  -> g (x:xs)

-- | Convert a binary sequence into a delimited token sequence
fromBinaryLen :: Bool -> Tree a -> Int -> LBS.ByteString -> [a]
fromBinaryLen leftIsZero tree n = runGet (runBitGet (g [] n))
   where
      bg     = makeBitGet leftIsZero tree
      g xs 0 = return (reverse xs)
      g xs m = bg >>= \case
         Nothing -> return (reverse xs)
         Just x  -> g (x:xs) (m-1)


-- | Put a code
putCode :: Monad m => Code -> BitPutT m ()
putCode (Code len w) = putBitsM len w

-- | Convert a sequence into a compressed binary
toBinary :: (Foldable m, Ord a) => Map a Code -> m a -> BS.ByteString
toBinary table xs = runBitPut BB (mapM_ bp xs)
   where
      bp x   = putCode (table Map.! x)

        

--------------------------------------------------
-- Code
--------------------------------------------------


-- | Huffman binary code
--
-- Length of the code is limited to 64
data Code = Code
   { codeLength :: Word       -- ^ Length of the code
   , codeValue  :: Word64     -- ^ Code in least-significant bits
   } deriving (Eq,Ord)

instance Show Code where
   show = rec . codeReverseBits
      where
         rec c
            | codeLength c == 0  = ""
            | codeTestBit c 0    = "1" ++ rec (codeShiftR 1 c)
            | otherwise          = "0" ++ rec (codeShiftR 1 c)

-- | Empty code
emptyCode :: Code
emptyCode = Code 0 0


-- | Reverse bits in a code
codeReverseBits :: Code -> Code
codeReverseBits (Code len v) = Code len (reverseLeastBits len v)

-- | Test a bit in the code (no check if out of bounds)
codeTestBit :: Code -> Int -> Bool
codeTestBit (Code _ v) n = testBit v n

-- | Increase code length
codeShiftL :: Word -> Code -> Code
codeShiftL n p = p
   { codeLength = codeLength p + n
   , codeValue  = codeValue p `shiftL` fromIntegral n
   }

-- | Decrease code length
codeShiftR :: Word -> Code -> Code
codeShiftR n p = p
   { codeLength = codeLength p - n
   , codeValue  = codeValue p `shiftR` fromIntegral n
   }

-- | Add a value to a code
codeAdd :: Word64 -> Code -> Code
codeAdd 0 p = p
codeAdd n p = codeAdd (n-1) p'
   where
      v   = codeValue p + 1
      len = if testBit v (fromIntegral $ codeLength p)
         then codeLength p + 1
         else codeLength p
      p'  = p
            { codeLength = len
            , codeValue  = v
            }

-- | Append two codes into a single one
codeAppend :: Code -> Code -> Code
codeAppend (Code l1 c1) (Code l2 c2) = Code (l1+l2) ((c1 `shiftL` (fromIntegral l2)) .|. c2)

