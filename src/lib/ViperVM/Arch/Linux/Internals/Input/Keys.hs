-- | Input keys
module ViperVM.Arch.Linux.Internals.Input.Keys
   ( keyTable
   )
where

import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Storable

-- | Bytes of the permutation table for input keys
keyTable :: [Word8]
keyTable = concatMap wordBytes keyTable'

-- | Permutation table for input keys
keyTable' :: [Word16]
keyTable' = go 0 0 holes
   where
      -- holes have to be in ascending order
      holes = [84] ++ [195..199] ++ [249..255]
              ++ [0x10a..0x10f]
              ++ [0x118..0x11f]
              ++ [0x12c..0x12e]
              ++ [0x13f]
              ++ [0x149]
              ++ [0x152..0x15f]
              ++ [0x1bb..0x1bf]
              ++ [0x1c4..0x1cf]
              ++ [0x1e5..0x1f0]
              ++ [0x1fb..0x1ff]
              ++ [0x21f]
              ++ [0x224..0x22f]
              ++ [0x231..0x23f]
              ++ [0x247..0x24f]
              ++ [0x252..0x25f]
              ++ [0x277..0x2bf]
              ++ [0x2e8..0x2ff]

      maxKey = 0x300

      go i _ _ | i == maxKey = []
      go i n (x:xs) -- hole found
         | i == x    = (maxKey -1 - n) : go (i+1) (n+1) xs
      go i n xs      = (i - n)         : go (i+1) n xs


