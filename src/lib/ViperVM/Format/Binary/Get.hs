module ViperVM.Format.Binary.Get
   ( module Data.Binary.Get
   , getWhile
   , getWhole
   )
where

import Data.Binary.Get

-- | Get while True (read and discard the ending element)
getWhile :: (a -> Bool) -> Get a -> Get [a]
getWhile cond getter = rec []
   where
      rec xs = do
         x <- getter
         if cond x
            then rec (x:xs)
            else return (reverse xs)

-- | Repeat the getter to read the whole bytestring
getWhole :: Get a -> Get [a]
getWhole getter = rec []
   where
      rec xs = do
         cond <- isEmpty
         if cond
            then return (reverse xs)
            else do
               x <- getter
               rec (x:xs)

