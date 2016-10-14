module ViperVM.Utils.List
   ( checkLength
   , module Data.List
   )
where

import Data.List

-- | Check that a list has the given length (support infinite lists)
checkLength :: Word -> [a] -> Bool
checkLength 0 []     = True
checkLength 0 _      = False
checkLength _ []     = False
checkLength i (x:xs) = checkLength (i-1) xs
