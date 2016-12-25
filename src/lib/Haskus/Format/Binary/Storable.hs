{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Storable class
module Haskus.Format.Binary.Storable
   ( StaticStorable (..)
   , staticPeek
   , staticPoke
   , RequiredPadding
   , Padding
   , PaddingEx
   , staticSizeOf
   , staticAlignment
   , wordBytes
   -- * Storable
   , Storable (..)
   , peek
   , poke
   , sizeOf'
   , sizeOfT
   , sizeOfT'
   , alignment'
   , alignmentT
   , alignmentT'
   , peekByteOff
   , pokeByteOff
   , peekElemOff
   , pokeElemOff
   , alloca
   , allocaBytes
   , allocaBytesAligned
   , malloc
   , with
   , withMany
   , allocaArray
   , mallocArray
   , withArray
   , withArrayLen
   , peekArray
   , pokeArray
   )
where

import qualified Foreign.Storable as FS
import Foreign.C.Types (CSize,CChar,CULong,CLong,CUInt,CInt,CUShort,CShort)
import qualified Foreign.Marshal.Alloc as P
import System.IO.Unsafe

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Ptr
import Haskus.Utils.Types
import Haskus.Utils.Types.Generics
import Haskus.Utils.Flow

-- | A storable data in constant space whose size is known at compile time
class StaticStorable a where
   -- | Size of the stored data (in bytes)
   type SizeOf a    :: Nat

   -- | Alignment requirement (in bytes)
   type Alignment a :: Nat

   -- | Peek (read) a value from a memory address
   staticPeekIO :: Ptr a -> IO a

   -- | Poke (write) a value at the given memory address
   staticPokeIO :: Ptr a -> a -> IO ()

-- | Peek (read) a value from a memory address
staticPeek :: (StaticStorable a, MonadIO m) => Ptr a -> m a
staticPeek p = liftIO (staticPeekIO p)

-- | Poke (write) a value at the given memory address
staticPoke :: (StaticStorable a, MonadIO m) => Ptr a -> a -> m ()
staticPoke p a = liftIO (staticPokeIO p a)


-- | Compute the required padding between a and b to respect b's alignment
type family RequiredPadding a b where
   RequiredPadding a b = Padding (SizeOf a) b

-- | Compute the required padding between the size sz and b to respect b's alignment
type family Padding (sz :: Nat) b where
   Padding sz b = PaddingEx (Modulo sz (Alignment b)) (Alignment b)

type family PaddingEx (m :: Nat) (a :: Nat) where
   PaddingEx 0 a = 0
   PaddingEx m a = a - m


-- | Get statically known size
staticSizeOf :: forall a.
   ( KnownNat (SizeOf a)
   ) => a -> Word
staticSizeOf _ = natValue' @(SizeOf a)

-- | Get statically known alignment
staticAlignment :: forall a.
   ( KnownNat (Alignment a)
   ) => a -> Word
staticAlignment _ = natValue' @(Alignment a)


-- | Get bytes in host-endianness order
wordBytes :: forall a.
   ( Storable a
   , KnownNat (SizeOf a)
   ) => a -> [Word8]
{-# INLINE wordBytes #-}
wordBytes x = unsafePerformIO $
   with x $ \p -> mapM (peekByteOff (castPtr p)) [0..natValue @(SizeOf a) - 1]



-- | Storable data-types
--
-- Currently we cannot automatically derive a Storable class with type-level
-- naturals for "alignment" and "sizeOf". Instead we define a Storable class
-- isomorphic to the Foreign.Storable's one but with default methods using
-- DefaultSignatures (i.e., the Storable instance can be automatically derived
-- from a Generic instance).
class Storable a where
  peekIO            :: Ptr a -> IO a
  default peekIO    :: (Generic a, GStorable (Rep a)) => Ptr a -> IO a
  peekIO p          = fmap to $ gcPeek 0 (castPtr p)

  pokeIO            :: Ptr a -> a -> IO ()
  default pokeIO    :: (Generic a, GStorable (Rep a)) => Ptr a -> a -> IO ()
  pokeIO p x        = gcPoke 0 (castPtr p) $ from x

  alignment         :: a -> Word
  default alignment :: (Generic a, GStorable (Rep a)) => a -> Word
  alignment         = gcAlignment . from

  sizeOf            :: a -> Word
  default sizeOf    :: (Generic a, GStorable (Rep a)) => a -> Word
  sizeOf            = gcSizeOf 0 . from

-- | Peek a value from a pointer
peek :: (Storable a, MonadIO m) => Ptr a -> m a
peek p = liftIO (peekIO p)

-- | Poke a value to a pointer
poke :: (Storable a, MonadIO m) => Ptr a -> a -> m ()
poke p v = liftIO (pokeIO p v)

-- | Generalized 'sizeOf'
sizeOf' :: (Integral b, Storable a) => a -> b
{-# INLINE sizeOf' #-}
sizeOf' = fromIntegral . sizeOf

-- | SizeOf (for type-application)
sizeOfT :: forall a. (Storable a) => Word
{-# INLINE sizeOfT #-}
sizeOfT = sizeOf (undefined :: a)

-- | SizeOf' (for type-application)
sizeOfT' :: forall a b. (Storable a, Integral b) => b
{-# INLINE sizeOfT' #-}
sizeOfT' = sizeOf' (undefined :: a)

-- | Generalized 'alignment'
alignment' :: (Integral b, Storable a) => a -> b
{-# INLINE alignment' #-}
alignment' = fromIntegral . alignment

-- | Alignment (for type-application)
alignmentT :: forall a. (Storable a) => Word
{-# INLINE alignmentT #-}
alignmentT = alignment (undefined :: a)

-- | Alignment' (for type-application)
alignmentT' :: forall a b. (Storable a, Integral b) => b
{-# INLINE alignmentT' #-}
alignmentT' = alignment' (undefined :: a)

-- | Peek with byte offset
peekByteOff :: (MonadIO m, Storable a) => Ptr a -> Int -> m a
{-# INLINE peekByteOff #-}
peekByteOff ptr off = peek (ptr `indexPtr` off)

-- | Poke with byte offset
pokeByteOff :: (MonadIO m, Storable a) => Ptr a -> Int -> a -> m ()
{-# INLINE pokeByteOff #-}
pokeByteOff ptr off = poke (ptr `indexPtr` off)

-- | Peek with element size offset
peekElemOff :: forall a m. (MonadIO m, Storable a) => Ptr a -> Int -> m a
peekElemOff ptr off = peekByteOff ptr (off * sizeOfT' @a)

-- | Poke with element size offset
pokeElemOff :: (MonadIO m, Storable a) => Ptr a -> Int -> a -> m ()
pokeElemOff ptr off val = pokeByteOff ptr (off * sizeOf' val) val

-- | Allocate some bytes
allocaBytes :: MonadInIO m => Word -> (Ptr a -> m b) -> m b
allocaBytes sz = liftWith (P.allocaBytes (fromIntegral sz))

-- | Allocate some aligned bytes
allocaBytesAligned :: MonadInIO m => Word -> Word -> (Ptr a -> m b) -> m b
allocaBytesAligned sz align = liftWith (P.allocaBytesAligned (fromIntegral sz) (fromIntegral align))

-- | @'alloca' f@ executes the computation @f@, passing as argument
-- a pointer to a temporarily allocated block of memory sufficient to
-- hold values of type @a@.
--
-- The memory is freed when @f@ terminates (either normally or via an
-- exception), so the pointer passed to @f@ must /not/ be used after this.
--
alloca :: forall a b m. (MonadInIO m, Storable a) => (Ptr a -> m b) -> m b
{-# INLINE alloca #-}
alloca = allocaBytesAligned (sizeOfT' @a) (alignmentT' @a)

-- | Allocate a block of memory that is sufficient to hold values of type
-- @a@. The size of the area allocated is determined by the 'sizeOf'
-- method from the instance of 'Storable' for the appropriate type.
--
-- The memory may be deallocated using 'free' or 'finalizerFree' when
-- no longer required.
malloc :: forall a m. (MonadIO m, Storable a) => m (Ptr a)
{-# INLINE malloc #-}
malloc = liftIO (mallocBytes (sizeOfT @a))

-- | @'with' val f@ executes the computation @f@, passing as argument
-- a pointer to a temporarily allocated block of memory into which
-- @val@ has been marshalled (the combination of 'alloca' and 'poke').
--
-- The memory is freed when @f@ terminates (either normally or via an
-- exception), so the pointer passed to @f@ must /not/ be used after this.
with :: (MonadInIO m, Storable a) => a -> (Ptr a -> m b) -> m b
{-# INLINE with #-}
with val f =
   alloca $ \ptr -> do
      poke ptr val
      f ptr

-- | Temporarily allocate space for the given number of elements
-- (like 'alloca', but for multiple elements).
allocaArray :: forall a b m. (MonadInIO m, Storable a) => Word -> (Ptr a -> m b) -> m b
allocaArray size = liftWith (allocaBytesAligned (size * sizeOfT' @a) (alignmentT' @a))

-- | Allocate space for the given number of elements
-- (like 'malloc', but for multiple elements).
mallocArray :: forall a m. (MonadIO m, Storable a) => Word -> m (Ptr a)
mallocArray size = mallocBytes (size * sizeOfT @a)

-- | Convert an array of given length into a Haskell list.  The implementation
-- is tail-recursive and so uses constant stack space.
peekArray :: (MonadIO m, Storable a) => Word -> Ptr a -> m [a]
peekArray size ptr
   | size <= 0 = return []
   | otherwise = f (size-1) []
  where
    f 0 acc = (:acc) <$> peekElemOff ptr 0
    f n acc = f (n-1) =<< ((:acc) <$> peekElemOff ptr (fromIntegral n))

-- | Write the list elements consecutive into memory
pokeArray :: (MonadIO m, Storable a) => Ptr a -> [a] -> m ()
pokeArray ptr vals0 = go vals0 0
  where go [] _         = return ()
        go (val:vals) n = do pokeElemOff ptr n val; go vals (n+1)

-- | Temporarily store a list of storable values in memory
-- (like 'with', but for multiple elements).
withArray :: (MonadInIO m, Storable a) => [a] -> (Ptr a -> m b) -> m b
withArray vals = withArrayLen vals . const

-- | Like 'withArray', but the action gets the number of values
-- as an additional parameter
withArrayLen :: (MonadInIO m, Storable a) => [a] -> (Word -> Ptr a -> m b) -> m b
withArrayLen vals f  =
  allocaArray len $ \ptr -> do
      pokeArray ptr vals
      f len ptr
  where
    len = fromIntegral (length vals)

-- | Replicates a @withXXX@ combinator over a list of objects, yielding a list of
-- marshalled objects
withMany :: (a -> (b -> res) -> res)  -- withXXX combinator for one object
         -> [a]                       -- storable objects
         -> ([b] -> res)              -- action on list of marshalled obj.s
         -> res
withMany _       []     f = f []
withMany withFoo (x:xs) f = withFoo x $ \x' ->
                              withMany withFoo xs (\xs' -> f (x':xs'))

class GStorable a where
  gcAlignment :: a x -> Word
  gcPeek      :: Word -> Ptr (a x)-> IO (a x)
  gcPoke      :: Word -> Ptr (a x) -> a x -> IO ()
  gcSizeOf    :: Word -> a x -> Word

  -- padding before the field to align from the given offset
  gcPadding   :: Word -> a x -> Word
  gcPadding off a = (gcAlignment a - off) `mod` gcAlignment a

instance GStorable U1 where
  gcAlignment _ = 0
  gcPeek _ _    = return U1
  gcPoke _ _ _  = return ()
  gcSizeOf _ _  = 0
  gcPadding _ _ = 0

instance (GStorable a, GStorable b) => GStorable (a :*: b) where
  gcAlignment _ = lcm (gcAlignment (undefined :: a x))
                      (gcAlignment (undefined :: b y))

  gcPeek off p = do
    a <- gcPeek off                    $ castPtr p
    b <- gcPeek (off + gcSizeOf off a) $ castPtr p
    return $ a :*: b

  gcPoke off p (a :*: b) = do
    gcPoke off                    (castPtr p) a
    gcPoke (off + gcSizeOf off a) (castPtr p) b

  gcSizeOf off _    = let
    a = undefined :: a x
    b = undefined :: b y
    off2 = off + gcSizeOf off a
    in gcSizeOf off a + gcSizeOf off2 b

instance (GStorable a) => GStorable (M1 i c a) where
  gcAlignment (M1 x)     = gcAlignment x
  gcPeek off p           = fmap M1 $ gcPeek off (castPtr p)
  gcPoke off p (M1 x)    = gcPoke off (castPtr p) x
  gcSizeOf off (M1 x)    = gcSizeOf off x
  gcPadding off (M1 x)   = gcPadding off x

instance (Storable a) => GStorable (K1 i a) where
  gcAlignment (K1 x)     = alignment x
  gcPeek off p           = fmap K1 $ peek (castPtr p `indexPtr'` (off + gcPadding off (undefined :: K1 i a x)))
  gcPoke off p (K1 x)    = poke (castPtr p `indexPtr'` (off + gcPadding off (undefined :: K1 i a x))) x
  gcSizeOf off (K1 x)    = gcPadding off (undefined :: K1 i a x) + sizeOf x


-- | Generalize FS.peek
fsPeek :: (FS.Storable a, MonadIO m) => Ptr a -> m a
fsPeek = liftIO . FS.peek

-- | Generalize FS.poke
fsPoke :: (FS.Storable a, MonadIO m) => Ptr a -> a -> m ()
fsPoke ptr a = liftIO (FS.poke ptr a)

instance StaticStorable Word8 where
   type SizeOf    Word8 = 1
   type Alignment Word8 = 1
   staticPeekIO         = fsPeek
   staticPokeIO         = fsPoke

instance StaticStorable Word16 where
   type SizeOf    Word16 = 2
   type Alignment Word16 = 2
   staticPeekIO          = fsPeek
   staticPokeIO          = fsPoke

instance StaticStorable Word32 where
   type SizeOf    Word32 = 4
   type Alignment Word32 = 4
   staticPeekIO          = fsPeek
   staticPokeIO          = fsPoke

instance StaticStorable Word64 where
   type SizeOf    Word64 = 8
   type Alignment Word64 = 8
   staticPeekIO          = fsPeek
   staticPokeIO          = fsPoke

instance StaticStorable Int8 where
   type SizeOf    Int8 = 1
   type Alignment Int8 = 1
   staticPeekIO        = fsPeek
   staticPokeIO        = fsPoke

instance StaticStorable Int16 where
   type SizeOf    Int16 = 2
   type Alignment Int16 = 2
   staticPeekIO         = fsPeek
   staticPokeIO         = fsPoke

instance StaticStorable Int32 where
   type SizeOf    Int32 = 4
   type Alignment Int32 = 4
   staticPeekIO         = fsPeek
   staticPokeIO         = fsPoke

instance StaticStorable Int64 where
   type SizeOf    Int64 = 8
   type Alignment Int64 = 8
   staticPeekIO         = fsPeek
   staticPokeIO         = fsPoke


instance Storable Word8 where
   sizeOf    _ = 1
   alignment _ = 1
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable Word16 where
   sizeOf    _ = 2
   alignment _ = 2
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable Word32 where
   sizeOf    _ = 4
   alignment _ = 4
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable Word64 where
   sizeOf    _ = 8
   alignment _ = 8
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable Int8 where
   sizeOf    _ = 1
   alignment _ = 1
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable Int16 where
   sizeOf    _ = 2
   alignment _ = 2
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable Int32 where
   sizeOf    _ = 4
   alignment _ = 4
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable Int64 where
   sizeOf    _ = 8
   alignment _ = 8
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable Float where
   sizeOf    _ = 4
   alignment _ = 4
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable Double where
   sizeOf    _ = 8
   alignment _ = 8
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable Char where
   sizeOf      = fromIntegral . FS.sizeOf
   alignment   = fromIntegral . FS.alignment
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable Word where
   sizeOf      = fromIntegral . FS.sizeOf
   alignment   = fromIntegral . FS.alignment
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable Int where
   sizeOf      = fromIntegral . FS.sizeOf
   alignment   = fromIntegral . FS.alignment
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable (Ptr a) where
   sizeOf      = fromIntegral . FS.sizeOf
   alignment   = fromIntegral . FS.alignment
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable CSize where
   sizeOf      = fromIntegral . FS.sizeOf
   alignment   = fromIntegral . FS.alignment
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable CChar where
   sizeOf      = fromIntegral . FS.sizeOf
   alignment   = fromIntegral . FS.alignment
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable CULong where
   sizeOf      = fromIntegral . FS.sizeOf
   alignment   = fromIntegral . FS.alignment
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable CLong where
   sizeOf      = fromIntegral . FS.sizeOf
   alignment   = fromIntegral . FS.alignment
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable CUInt where
   sizeOf      = fromIntegral . FS.sizeOf
   alignment   = fromIntegral . FS.alignment
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable CInt where
   sizeOf      = fromIntegral . FS.sizeOf
   alignment   = fromIntegral . FS.alignment
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable CUShort where
   sizeOf      = fromIntegral . FS.sizeOf
   alignment   = fromIntegral . FS.alignment
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable CShort where
   sizeOf      = fromIntegral . FS.sizeOf
   alignment   = fromIntegral . FS.alignment
   peekIO      = fsPeek
   pokeIO      = fsPoke

instance Storable WordPtr where
   sizeOf      = fromIntegral . FS.sizeOf
   alignment   = fromIntegral . FS.alignment
   peekIO      = fsPeek
   pokeIO      = fsPoke
