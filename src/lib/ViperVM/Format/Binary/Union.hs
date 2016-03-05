{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module ViperVM.Format.Binary.Union
   ( Union2
   , Union3
   , union1
   , union2
   , union3
   , union4
   )
where

import ViperVM.Utils.Memory (memCopy)

import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Word
import System.IO.Unsafe (unsafePerformIO)

data Buffer = Buffer !Word64 !(ForeignPtr ())

peekBuffer :: Storable b => (Buffer -> b) -> Ptr a -> IO b
peekBuffer f ptr = do
   let sz = sizeOf (f undefined)
   fp <- mallocForeignPtrBytes sz
   withForeignPtr fp $ \p -> 
      memCopy p (castPtr ptr) (fromIntegral sz)
   return (f (Buffer (fromIntegral sz) fp))

pokeBuffer :: Buffer -> Ptr a -> IO ()
pokeBuffer (Buffer sz fp) ptr =
   withForeignPtr fp $ \p ->
      memCopy (castPtr ptr) p sz

peekElem :: Storable a => Buffer -> a
peekElem (Buffer _ fp) = unsafePerformIO $
   withForeignPtr fp $ \p -> peek (castPtr p)


newtype Union2 a b     = Union2 Buffer
newtype Union3 a b c   = Union3 Buffer
newtype Union4 a b c d = Union4 Buffer

class Union a where
   getBuffer :: a -> Buffer

instance (Storable a, Storable b) => Storable (Union2 a b) where
   sizeOf _    = maximum
                  [ sizeOf (undefined :: a)
                  , sizeOf (undefined :: b)
                  ]
   alignment _ = maximum
                  [ alignment (undefined :: a)
                  , alignment (undefined :: b)
                  ]
   peek        = peekBuffer Union2
   poke ptr (Union2 b) = pokeBuffer b ptr

instance Union (Union2 a b) where
   getBuffer (Union2 b) = b


instance (Storable a, Storable b, Storable c) => Storable (Union3 a b c) where
   sizeOf _    = maximum
                  [ sizeOf (undefined :: a)
                  , sizeOf (undefined :: b)
                  , sizeOf (undefined :: c)
                  ]
   alignment _ = maximum
                  [ alignment (undefined :: a)
                  , alignment (undefined :: b)
                  , alignment (undefined :: c)
                  ]
   peek        = peekBuffer Union3
   poke ptr (Union3 b) = pokeBuffer b ptr

instance Union (Union3 a b c) where
   getBuffer (Union3 b) = b

instance (Storable a, Storable b, Storable c, Storable d) => Storable (Union4 a b c d) where
   sizeOf _    = maximum
                  [ sizeOf (undefined :: a)
                  , sizeOf (undefined :: b)
                  , sizeOf (undefined :: c)
                  , sizeOf (undefined :: d)
                  ]
   alignment _ = maximum
                  [ alignment (undefined :: a)
                  , alignment (undefined :: b)
                  , alignment (undefined :: c)
                  , alignment (undefined :: d)
                  ]
   peek        = peekBuffer Union4
   poke ptr (Union4 b) = pokeBuffer b ptr

instance Union (Union4 a b c d) where
   getBuffer (Union4 b) = b



type family E1 a
type instance E1 (Union2 a b)     = a
type instance E1 (Union3 a b c)   = a
type instance E1 (Union4 a b c d) = a

type family E2 a
type instance E2 (Union2 a b)     = b
type instance E2 (Union3 a b c)   = b
type instance E2 (Union4 a b c d) = b

type family E3 a
type instance E3 (Union3 a b c)   = c
type instance E3 (Union4 a b c d) = c

type family E4 a
type instance E4 (Union4 a b c d) = d

union1 :: (Union u, Storable (E1 u)) => u -> E1 u
union1 u = peekElem (getBuffer u)

union2 :: (Union u, Storable (E2 u)) => u -> E2 u
union2 u = peekElem (getBuffer u)

union3 :: (Union u, Storable (E3 u)) => u -> E3 u
union3 u = peekElem (getBuffer u)

union4 :: (Union u, Storable (E3 u)) => u -> E3 u
union4 u = peekElem (getBuffer u)
