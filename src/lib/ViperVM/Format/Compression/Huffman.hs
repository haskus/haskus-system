-- | Implement Huffman coding
module ViperVM.Format.Compression.Huffman
   ( computeOccurences
   , buildQueue
   , buildTree
   , buildCoding
   , computeCoding
   , computeCodingString
   )
where

import Data.Foldable (foldl', Foldable)
import qualified Data.Map as Map
import qualified Data.PQueue.Prio.Min as PQueue
import Data.Word
import Data.Tuple (swap)

type Prio = Word64

-- | Get occurences of each element
computeOccurences :: (Foldable m, Eq a, Ord a) => m a -> Map.Map a Prio
computeOccurences = foldl' f Map.empty 
   where
      f ocs k = Map.insertWith' (+) k 1 ocs

-- | Build min priority queue (priority is number of occurences)
buildQueue :: Map.Map a Prio -> PQueue.MinPQueue Prio a
buildQueue = PQueue.fromList . fmap swap . Map.toList

data Tree a
   = Node (Tree a) (Tree a)
   | Leaf a

-- | Build the Huffman tree
buildTree :: PQueue.MinPQueue Prio a -> Tree a
buildTree pq = rec pq'
   where
      pq' = PQueue.map Leaf pq
      rec q = case PQueue.size q of
         0 -> error "Invalid empty queue"
         1 -> snd (PQueue.findMin q)
         _ -> rec q''' where
                  q'  = PQueue.deleteMin q
                  q'' = PQueue.deleteMin q'
                  (k1,m1) = PQueue.findMin q
                  (k2,m2) = PQueue.findMin q'
                  q'''= PQueue.insert (k1+k2) (Node m1 m2) q''

-- | Get Huffman coding from a Huffman tree
buildCoding :: Ord a => b -> b -> (b -> b -> b) -> Tree a -> Map.Map a b
buildCoding left right op tree = rec Nothing tree
   where
      rec Nothing t = case t of
         (Node l r) -> Map.union (rec (Just left)  l) 
                                 (rec (Just right) r)
         (Leaf x)   -> Map.singleton x left -- arbitrarily chose left

      rec (Just cur) t = case t of
         (Node l r) -> Map.union (rec (Just $ cur `op` left)  l) 
                                 (rec (Just $ cur `op` right) r)
         (Leaf x)   -> Map.singleton x cur
      
-- | Compute a Huffman coding
computeCoding :: (Foldable m, Ord a, Eq a) => b -> b -> (b -> b -> b) -> m a -> Map.Map a b
computeCoding left right op xs = code
   where
      occs  = computeOccurences xs
      queue = buildQueue occs
      tree  = buildTree queue
      code  = buildCoding left right op tree

-- | Compute strings containing binary coding ("0" and "1" chars)
computeCodingString :: (Foldable m, Ord a, Eq a) => m a -> Map.Map a String
computeCodingString = computeCoding "0" "1" (++)
