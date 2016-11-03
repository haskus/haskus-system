-- | Static tables
module ViperVM.Arch.Linux.Internals.Tables
   ( errorTable
   , errorTableMax
   , keyTable
   , keyTableMax
   , computeHoles
   )
where

import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Storable
import Data.List (sort)

-- | Maximal value in the error table
errorTableMax :: Word
errorTableMax = 133

-- | Error codes (EPERM, ENOENT, etc.)
errorTable :: [Word8]
errorTable = fmap fromIntegral $ computeHoles errorTableMax [0,41,58]


-- | Maximal value in the key table
keyTableMax :: Word
keyTableMax = 0x300

-- | Permutation table for input keys (keys are Word16)
keyTable :: [Word8]
keyTable = concatMap wordBytes keyTable'
   where
      keyTable' :: [Word16]
      keyTable' = fmap fromIntegral $ computeHoles keyTableMax $
         [84] ++ [195..199] ++ [249..255] ++ [0x10a..0x10f]
              ++ [0x118..0x11f] ++ [0x12c..0x12e]
              ++ [0x13f] ++ [0x149] ++ [0x152..0x15f]
              ++ [0x1bb..0x1bf] ++ [0x1c4..0x1cf]
              ++ [0x1e5..0x1f0] ++ [0x1fb..0x1ff] ++ [0x21f]
              ++ [0x224..0x22f] ++ [0x231..0x23f]
              ++ [0x247..0x24f] ++ [0x252..0x25f]
              ++ [0x277..0x2bf] ++ [0x2e8..0x2ff]

-- | Compute a permutation table from a list of holes
computeHoles :: Word -> [Word] -> [Word]
computeHoles imax holes = go 0 0 (sort holes)
   where
      go i _ _      | i > imax = []
      go i n (x:xs) | i == x   = (imax -1 - n) : go (i+1) (n+1) xs
      go i n xs                = (i - n)       : go (i+1) n xs

