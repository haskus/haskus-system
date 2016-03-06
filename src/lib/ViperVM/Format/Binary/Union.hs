{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

-- | Union (as in C)
--
-- Unions are storable and can contain any storable data. Currently there are
-- Union2, Union3 and Union4 (respectively to contain 2, 3 or 4 members). We
-- could easily extend this to 5+ members.
-- 
-- Use 'fromUnion' to read a alternative:
--
-- @
-- {-# LANGUAGE DataKinds #-}
--
-- import Data.Proxy
--
-- getUnion :: IO (Union3 Word16 Word32 Word64)
-- getUnion = ...
--
-- test = do
--    u <- getUnion
--
--    -- to get the Word16
--    let v = fromUnion (Proxy :: Proxy 1) u
--    -- to get the Word32
--    let v = fromUnion (Proxy :: Proxy 2) u
--    -- to get the Word64
--    let v = fromUnion (Proxy :: Proxy 3) u
-- @
--
-- Use 'toUnion' to create a new union:
-- @
--
-- let
--    u2 :: Union2 Word32 (Vector 4 Word8)
--    u2 = toUnion (Proxy :: Proxy 1) 0x12345678
-- @
module ViperVM.Format.Binary.Union
   ( Union2
   , Union3
   , Union4
   , fromUnion
   , toUnion
   )
where

import ViperVM.Utils.Memory (memCopy)

import GHC.TypeLits
import Data.Proxy
import Foreign.Storable
import Foreign.CStorable
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Word
import System.IO.Unsafe (unsafePerformIO)

data Buffer = Buffer !Word64 !(ForeignPtr ()) deriving (Show)

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

newtype Union2 a b     = Union2 Buffer deriving (Show)
newtype Union3 a b c   = Union3 Buffer deriving (Show)
newtype Union4 a b c d = Union4 Buffer deriving (Show)

class Union a where
   -- | Get the buffer backing an union
   getBuffer  :: a -> Buffer
   -- | Create an union from a buffer
   fromBuffer :: Buffer -> a

instance Union (Union2 a b) where
   getBuffer (Union2 b) = b
   fromBuffer           = Union2

instance Union (Union3 a b c) where
   getBuffer (Union3 b) = b
   fromBuffer           = Union3

instance Union (Union4 a b c d) where
   getBuffer (Union4 b) = b
   fromBuffer           = Union4



type family E (n :: Nat) a :: *
type instance E 1 (Union2 a b)     = a
type instance E 1 (Union3 a b c)   = a
type instance E 1 (Union4 a b c d) = a
type instance E 2 (Union2 a b)     = b
type instance E 2 (Union3 a b c)   = b
type instance E 2 (Union4 a b c d) = b
type instance E 3 (Union3 a b c)   = c
type instance E 3 (Union4 a b c d) = c
type instance E 4 (Union4 a b c d) = d

-- | Retrieve a union member from its index
fromUnion :: forall (n :: Nat) u . (KnownNat n, Union u, Storable (E n u))
            => Proxy n -> u -> E n u
fromUnion _ u = unsafePerformIO $ peekElem (getBuffer u)
   where
      peekElem :: Storable a => Buffer -> IO a
      peekElem (Buffer _ fp) = withForeignPtr fp (peek . castPtr)


-- | Create a new union
toUnion :: forall (n :: Nat) u . (Storable u, KnownNat n, Union u, Storable (E n u))
            => Proxy n -> E n u -> u
toUnion _ v = unsafePerformIO $ do
   let sz = sizeOf (undefined :: u)
   fp <- mallocForeignPtrBytes sz
   withForeignPtr fp $ \p -> 
      poke (castPtr p) v
   return (fromBuffer (Buffer (fromIntegral sz) fp))


-- TODO: rewrite rules
-- poke p (toUnion (Proxy :: Proxy n) x) = poke (castPtr p :: Ptr (E n u)) x
--
-- fromUnion n <$> peek p = peek (castPtr p :: Ptr (E n u))

instance (Storable a, Storable b) => Storable (Union2 a b) where
   sizeOf _            = maximum
                          [ sizeOf (undefined :: a)
                          , sizeOf (undefined :: b)
                          ]
   alignment _         = maximum
                          [ alignment (undefined :: a)
                          , alignment (undefined :: b)
                          ]
   peek                = peekBuffer Union2
   poke ptr (Union2 b) = pokeBuffer b ptr

instance (Storable a, Storable b) => CStorable (Union2 a b) where
   cPeek      = peek
   cPoke      = poke
   cAlignment = alignment
   cSizeOf    = sizeOf


instance (Storable a, Storable b, Storable c) => Storable (Union3 a b c) where
   sizeOf _            = maximum
                          [ sizeOf (undefined :: a)
                          , sizeOf (undefined :: b)
                          , sizeOf (undefined :: c)
                          ]
   alignment _         = maximum
                          [ alignment (undefined :: a)
                          , alignment (undefined :: b)
                          , alignment (undefined :: c)
                          ]
   peek                = peekBuffer Union3
   poke ptr (Union3 b) = pokeBuffer b ptr

instance (Storable a, Storable b, Storable c) => CStorable (Union3 a b c) where
   cPeek      = peek
   cPoke      = poke
   cAlignment = alignment
   cSizeOf    = sizeOf


instance (Storable a, Storable b, Storable c, Storable d) => Storable (Union4 a b c d) where
   sizeOf _            = maximum
                          [ sizeOf (undefined :: a)
                          , sizeOf (undefined :: b)
                          , sizeOf (undefined :: c)
                          , sizeOf (undefined :: d)
                          ]
   alignment _         = maximum
                          [ alignment (undefined :: a)
                          , alignment (undefined :: b)
                          , alignment (undefined :: c)
                          , alignment (undefined :: d)
                          ]
   peek                = peekBuffer Union4
   poke ptr (Union4 b) = pokeBuffer b ptr

instance (Storable a, Storable b, Storable c, Storable d) => CStorable (Union4 a b c d) where
   cPeek      = peek
   cPoke      = poke
   cAlignment = alignment
   cSizeOf    = sizeOf
