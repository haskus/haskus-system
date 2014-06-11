module ViperVM.Arch.X86_64.Linux.Time (
   TimeSpec(..)
) where

import Foreign.Storable
import Data.Int
import Control.Applicative

data TimeSpec = TimeSpec {
   tsSeconds :: Int64,
   tsNanosecondes :: Int64
} deriving (Show)

instance Storable TimeSpec where
   alignment _ = alignment (undefined :: Int64)
   sizeOf _ = 16
   peek p = TimeSpec 
      <$> peekByteOff p 0 
      <*> peekByteOff p 8
   poke p (TimeSpec sec nsec) = do
      pokeByteOff p 0 sec
      pokeByteOff p 8 nsec
