{-# LANGUAGE LambdaCase #-}

-- | Implement Huffman coding
module ViperVM.Format.Compression.Algorithms.Huffman
   ( 
   -- * Huffman Tree
     Tree(..)
   , makeTree
   , buildTree

   -- * Huffman Code
   , Code(..)
   , emptyCode
   , buildCoding
   , buildCodingString
   , buildTreeFromCodes
   , codeAdd
   , codeShiftL
   , codeShiftR

   -- * Encoding
   , makeBitGet
   , makeBitPut
   , toBinary
   , fromBinary
   , fromBinaryLen

   -- * Helpers
   , computeOccurences
   , buildQueue
   )
where

import Prelude hiding (mapM_)

import Data.Foldable (foldl', mapM_)
import qualified Data.Map as Map
import qualified Data.PQueue.Prio.Min as PQueue
import Data.Word
import Data.Tuple (swap)
import Data.Bits (xor,shiftL,shiftR,testBit)
import Data.Binary.Bits.Get as BitGet
import Data.Binary.Bits.Put
import ViperVM.Format.Binary.Get as Get
import ViperVM.Format.Binary.Put
import qualified Data.ByteString.Lazy as BS
import Control.Arrow (first)

type Prio = Word64

-- | Get occurences of each element
computeOccurences :: (Foldable m, Eq a, Ord a) => m a -> Map.Map a Prio
computeOccurences = foldl' f Map.empty 
   where
      f ocs k = Map.insertWith' (+) k 1 ocs

-- | Build min priority queue (priority is number of occurences)
buildQueue :: Map.Map a Prio -> PQueue.MinPQueue Prio a
buildQueue = PQueue.fromList . fmap swap . Map.toList

-- | A binary tree
data Tree a
   = Node (Tree a) (Tree a)
   | Leaf a
   | Empty
   deriving (Eq,Show)

-- | Build the Huffman tree
buildTree :: PQueue.MinPQueue Prio a -> Tree a
buildTree pq = rec pq'
   where
      pq' = PQueue.map Leaf pq
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
makeTree :: (Foldable m, Eq a, Ord a) => m a -> Tree a
makeTree xs = tree
   where
      occs  = computeOccurences xs
      queue = buildQueue occs
      tree  = buildTree queue

-- | Get Huffman coding from a Huffman tree
buildCoding :: Ord a => b -> b -> (b -> b -> b) -> Tree a -> Map.Map a b
buildCoding left right op tree = rec Nothing tree
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
      
-- | Compute strings containing binary coding ("0" and "1" chars)
buildCodingString :: Ord a => Tree a -> Map.Map a String
buildCodingString = buildCoding "0" "1" (++)

-- | Create a BitGet that reads a single element
--
-- You can specify which child is 0 or 1
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

-- | Create a BitPut from a Huffman tree
--
-- You can specify which child is 0 or 1
makeBitPut :: (Ord a) => Bool -> Tree a -> a -> BitPut ()
makeBitPut leftIsZero tree x = code Map.! x
   where
      l = putBool (not leftIsZero)
      r = putBool leftIsZero
      code = buildCoding l r (>>) tree


-- | Convert a sequence into a compressed binary
toBinary :: (Foldable m, Ord a, Eq a) => Bool -> Tree a -> m a -> BS.ByteString
toBinary leftIsZero tree xs = runPut p
   where
      bp    = makeBitPut leftIsZero tree
      p     = runBitPut (mapM_ bp xs)

-- | Convert a binary sequence into a token sequence
fromBinary :: Bool -> Tree a -> BS.ByteString -> [a]
fromBinary leftIsZero tree = runGet (runBitGet (g []))
   where
      bg    = makeBitGet leftIsZero tree
      g xs  = bg >>= \case
         Nothing -> return (reverse xs)
         Just x  -> g (x:xs)

-- | Convert a binary sequence into a delimited token sequence
fromBinaryLen :: Bool -> Tree a -> Int -> BS.ByteString -> [a]
fromBinaryLen leftIsZero tree n = runGet (runBitGet (g [] n))
   where
      bg     = makeBitGet leftIsZero tree
      g xs 0 = return (reverse xs)
      g xs m = bg >>= \case
         Nothing -> return (reverse xs)
         Just x  -> g (x:xs) (m-1)


-- | A Huffman code
--
-- Length of the code is limited to 64
data Code = Code
   { codeLength :: Int
   , codeValue  :: Word64
   } deriving (Show,Eq,Ord)

data LeftRight = L | R deriving (Show)

-- | Empty code
emptyCode :: Code
emptyCode = Code 0 0

-- | Convert a code into a binary list
codeToList :: Code -> [LeftRight]
codeToList = rec []
   where
      rec xs p 
         | codeLength p == 0 = xs
         | otherwise = rec (x:xs) p'
            where
               lk = codeValue p
               x = if testBit lk 0
                  then R
                  else L
               p' = p 
                  { codeValue = lk `shiftR` 1
                  , codeLength = codeLength p - 1
                  }

-- | Build a tree from a set of codes
buildTreeFromCodes :: Show a => [(Code,a)] -> Tree a
buildTreeFromCodes = rec . fmap (first codeToList)
   where
      rec [] = Empty
      rec ps = case ff [] [] [] ps of
         ([],lp,rp)  -> Node (rec lp) (rec rp)
         ([v],[],[]) -> Leaf v
         x           -> error $ "Invalid (ambiguous) codes: " ++ show x


      -- characterize codes in three ways:
      --    * empty codes
      --    * codes going to the left first
      --    * codes going to the right first
      ff ep lp rp [] = (ep,lp,rp)
      ff ep lp rp (p:ps)  = case p of
         ([],v)    -> ff (v:ep) lp rp ps
         ((L:r),v) -> ff ep ((r,v):lp) rp ps
         ((R:r),v) -> ff ep lp ((r,v):rp) ps
         
-- | Increase code length
codeShiftL :: Int -> Code -> Code
codeShiftL n p = p
   { codeLength = codeLength p + n
   , codeValue  = codeValue p `shiftL` n
   }

-- | Decrease code length
codeShiftR :: Int -> Code -> Code
codeShiftR n p = p
   { codeLength = codeLength p - n
   , codeValue  = codeValue p `shiftR` n
   }

-- | Add a value to a code
codeAdd :: Word64 -> Code -> Code
codeAdd 0 p = p
codeAdd n p = codeAdd (n-1) p'
   where
      v   = codeValue p + 1
      len = if testBit v (codeLength p)
         then codeLength p + 1
         else codeLength p
      p'  = p
            { codeLength = len
            , codeValue  = v
            }

