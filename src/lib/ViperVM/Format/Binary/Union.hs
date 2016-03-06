{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
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
-- getUnion :: IO (Union3 Word16 Word32 Word64)
-- getUnion = ...
--
-- test = do
--    u <- getUnion
--
--    -- to get one of the member
--    let v = fromUnion u :: Word16
--    let v = fromUnion u :: Word32
--    let v = fromUnion u :: Word64
--
--    -- This won't compile (Word8 is not a member of the union)
--    let v = fromUnion u :: Word8
-- @
--
-- Use 'toUnion' to create a new union:
-- @
--
-- let
--    u2 :: Union2 Word32 (Vector 4 Word8)
--    u2 = toUnion (0x12345678 :: Word32)
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



type family   UnionMember a u :: *
type instance UnionMember a (Union2 a b)     = a
type instance UnionMember a (Union3 a b c)   = a
type instance UnionMember a (Union4 a b c d) = a
type instance UnionMember b (Union2 a b)     = b
type instance UnionMember b (Union3 a b c)   = b
type instance UnionMember b (Union4 a b c d) = b
type instance UnionMember c (Union3 a b c)   = c
type instance UnionMember c (Union4 a b c d) = c
type instance UnionMember d (Union4 a b c d) = d

-- | Retrieve a union member from its index
fromUnion :: (Union u, Storable a, UnionMember a u ~ a) => u -> a
fromUnion u = unsafePerformIO $ peekElem (getBuffer u)
   where
      peekElem :: Storable a => Buffer -> IO a
      peekElem (Buffer _ fp) = withForeignPtr fp (peek . castPtr)


-- | Create a new union
toUnion :: forall a u . (Storable u, Union u, Storable a, UnionMember a u ~ a) => a -> u
toUnion v = unsafePerformIO $ do
   fp <- mallocForeignPtr :: IO (ForeignPtr u)
   withForeignPtr fp $ \p -> 
      poke (castPtr p) v
   return (fromBuffer (Buffer (fromIntegral (sizeOf (undefined :: u))) (castForeignPtr fp)))


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
