-- | Implementation of LZ77
--
-- From the paper:
--    "A Universal Algorithm for Sequential Data Compression"
--       Ziv, Jacob; Lempel, Abraham (May 1977)
--       IEEE Transactions on Information Theory 23 (3): 337–343
module ViperVM.Format.Compression.LZ77
   ( Code(..)
   , compress
   , decompress
   )
where

import Data.List (maximumBy,tails)
import Data.Ord (comparing)

data Code a = Code 
   { codePosition :: Int
   , codeLength   :: Int
   , codeElem     :: a
   } deriving (Show)

-- | Compress a sequence, using LZ77
--
-- `maxWordLength` (ls) is the maximum of elements that can compose a word
-- `n` is the buffer length
-- `ini` is the initial value contained in the buffer
compress :: (Eq a) => Int -> Int -> a -> [a] -> [Code a]
compress ls n ini input = rec initBuffer input
   where
      initBuffer = replicate (n-ls) ini

      -- return the length and the value of the longest prefix
      prefixLen :: Eq a => [a] -> [a] -> Int
      prefixLen ss bs = prefixLen' 0 ss bs

      prefixLen' len u v = case (u,v) of
         (s:ss,b:bs) | s == b -> prefixLen' (len+1) ss (bs++[s]) 
         _                    -> len

      rec :: Eq a => [a] -> [a] -> [Code a]
      rec b s = case s of
         [] -> []
         _  -> Code pos len k : rec newb ks
            where
               -- current word (max length = ls-1) of s
               w = take (ls-1) s

               -- prefix lengths and their position [(len,pos)]
               prefixes = (map (prefixLen w) (tails b)) `zip` [0..]
   
               -- longest prefix length and its position
               (len,pos) = maximumBy (comparing fst) prefixes

               -- last char and remaining sequence
               (k:ks) = drop len s

               -- new buffer
               newb = drop (len+1) b ++ take (len+1) s


-- | Decompress a sequence, using LZ77
--
-- `maxWordLength` (ls) is the maximum of elements that can compose a word
-- `n` is the buffer length
-- `ini` is the initial value contained in the buffer
decompress :: Int -> Int -> a -> [Code a] -> [a]
decompress ls n ini input = rec initBuffer input
   where
      initBuffer = replicate (n-ls) ini
      extract p l = take l . drop p

      rec b s = case s of
         [] -> []
         (Code pos len c:ss) -> w ++ rec newb ss
            where 
               b'   = b ++ drop pos b'
               w    = extract pos len b' ++ [c]
               newb = drop (len+1) b ++ w
