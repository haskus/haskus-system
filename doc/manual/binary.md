# Haskus: Binary

Haskus has a set of modules dedicated to the manipulation of binary data. They
provide data type mapping those of other languages such as C and even more.

All these modules are in [Haskus.Format.Binary](../../src/lib/Haskus/Format/Binary).

Haskus does not rely on external tools such as C2HS to provide bindings to C
libraries. There are several reasons for that:

* We don't want to depend on .h files;
* .h files often contain pecularities that are difficult to handle
  automatically;
* Documentation and code of the resulting Haskell files are often very bad:
    * No haddock
    * Very low-level (e.g. #define are not transformed into datatypes with Enum
      instances)

Instead Haskus lets you write bindings in pure Haskell code and provides many
useful things to make this process easy.

## Word, Int

The [Word module](../../src/lib/Haskus/Format/Binary/Word.hs) contains data
types representing unsigned words (Word8, Word16, Word32, etc.) and signed
integers (Int8, Int16, Int32, etc.). It also contains some C types such as
CSize, CShort, CUShort, CLong, CULong, etc.

### Endianness

Words and Ints are stored (i.e., read and written) using host endianness (byte
ordering). `AsBigEndian` and `AsLittleEndian` data types in the
[Endianness module](../../src/lib/Haskus/Format/Binary/Endianness.hs)
allow you to force a different endianness.

The following example shows a data type containing a field for each endianness
variant. We explain how to use this kind of data type as a C structure later in
this document.

```haskell
data Dummy = Dummy
   { fieldX :: Word32                -- ^ 32-byte unsigned word (host endianness)
   , fieldY :: AsBigEndian Word32    -- ^ 32-byte unsigned word (big-endian)
   , fieldZ :: AsLittleEndian Word32 -- ^ 32-byte unsigned word (little-endian)
   } deriving (Generic,Storable)
```

We can also explicitly change the endianness with the following methods:
* hostToBigEndian
* hostToLittleEndian
* bigEndianToHost
* littleEndianToHost
* reverseBytes

Each of these methods is either equivalent to `id` or to `reverseBytes`
depending on the host endianness.

## Bits

The [Bits module](../../src/lib/Haskus/Format/Binary/Bits.hs) allows you to
perform bitwise operations on data types supporting them.

## Buffer

A [Buffer](../../src/lib/Haskus/Format/Binary/Buffer.hs) is basically a strict
ByteString with a better name and a better integration with Storable type class.

## Structures

You map C data structures with Haskell data type as follows:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import Haskus.Format.Binary.Storable
import Haskus.Utils.Types.Generics (Generic)

data StructX = StructX
   { xField0 :: Word8
   , xField1 :: Word64
   } deriving (Show,Generic,Storable)
```

The Storable instance handles the alignment of the field as a C non-packed
structure would (i.e. there are 7 padding bytes between xField0 and xField1).

`peek` and `poke` can be used to read and write the data structure in memory.

### Nesting

Data structures can be nested:

```haskell
data StructY = StructY
   { yField0 :: StructX
   , yField1 :: Word64
   } deriving (Show,Generic,Storable)
```

### Arrays (or Vectors)

Haskus supports vectors: a fixed amount of Storable data correctly aligned. You
can define a vector as follows:
```haskell
{-# LANGUAGE DataKinds #-}

import Haskus.Format.Binary.Vector as V

v :: Vector 5 Word16
```

Vectors are storable, so you can `peek` and `poke` them from memory.
Alternatively, you can create them from a list:
```haskell
Just v = fromList [1,2,3,4,5]
Just v = fromList [1,2,3,4,5,6] -- this fails dynamically
Just v = fromList [1,2,3,4]     -- this fails dynamically

-- take at most 5 elements then fill with 0: v = [1,2,3,4,5]
v = fromFilledList 0 [1,2,3,4,5,6]

-- take at most 5 elements then fill with 7: v = [1,2,3,7,7]
v = fromFilledList 7 [1,2,3]

-- take at most 4 (!) elements then fill with 0: v = [1,2,3,0,0]
v = fromFilledListZ 0 [1,2,3]

-- useful for zero-terminal strings: s = "too long \NUL"
s :: Vector 10 CChar
s = fromFilledListZ 0 (fmap castCharToCChar "too long string")
```

You can concatenate several vectors into a single one:
```haskell
import Haskus.Utils.HList

x = fromFilledList 0 [1,2,3,4] :: Vector 4 Int
y = fromFilledList 0 [5,6]     :: Vector 2 Int
z = fromFilledList 0 [7,8,9]   :: Vector 3 Int

v = V.concat (x `HCons` y `HCons` z `HCons` HNil)

>:t v
v :: Vector 9 Int

> v
fromList [1,2,3,4,5,6,7,8,9]
```

You can also safely `drop` or `take` elements in a vector. You can also `index` into a vector:
```haskell
import Haskus.Format.Binary.Vector as V

v :: Vector 5 Int
v = fromFilledList 0 [1,2,3,4,5,6]

-- v2 = [1,2]
v2 = V.take (Proxy :: Proxy 2) v

-- won't compile (8 > 5)
v2 = V.take (Proxy :: Proxy 8) v

-- v2 = [3,4,5]
v2 = V.drop (Proxy :: Proxy 2) v

-- x = 3
x = V.index (Proxy :: Proxy 2) v
```

Finally, you can obtain a list of the values
```haskell
> V.toList v
[1,2,3,4,5]
```

### Enums

If you have a C enum (or a set of #define's) with consecutive values and
starting from 0, you can do:

```haskell
{-# LANGUAGE DeriveAnyClass #-}

import Haskus.Format.Binary.Enum

data MyEnum
   = MyEnumX
   | MyEnumY
   | MyEnumZ
   deriving (Show,Eq,Enum,CEnum)
```

If the values are not consecutive or don't start from 0, you can write your own
CEnum instance:

```haskell
-- Add 1 to the enum number to get the valid value
instance CEnum MyEnum where
   fromCEnum = (+1) . fromIntegral . fromEnum
   toCEnum   = toEnum . (\x -> x-1) . fromIntegral
```

To use an Enum as a field in a structure, use EnumField:
```haskell
data StructZ = StructZ
   { zField0 :: StructX
   , zField1 :: EnumField Word32 MyEnum
   } deriving (Show,Generic,Storable)
```

The first type parameter of EnumField indicates the backing word type (i.e. the
size of the field in the structure). For instance, you can use Word8, Word16,
Word32 and Word64.

To create or extract an EnumField, use the methods:

```haskell
fromEnumField :: CEnum a => EnumField b a -> a
toEnumField   :: CEnum a => a -> EnumField b a
```

We use a CEnum class that is very similar to Enum because Enum is a special
class that has access to data constructor tags. If we redefine Enum, we cannot
use `fromEnum` to get the data constructor tag.

### Bit sets (or "flags")

We often use flags that are combined in a single word. Each flag is associated
to a bit of the word: if the bit is set the flag is active, otherwise the flag
isn't active.

Haskus uses the CBitSet class to get the bit offset of each flag. By default,
it uses the Enum instance to get the bit offsets as in the following example:

```haskell
{-# LANGUAGE DeriveAnyClass #-}

import Haskus.Format.Binary.BitSet

data Flag
   = FlagX  -- bit 0
   | FlagY  -- bit 1
   | FlagZ  -- bit 2
   deriving (Show,Eq,Enum,CBitSet)
```

If you want to use different bit offsets, you can define your own CBitSet
instance:

```haskell
-- Add 1 to the enum number to get the valid bit offset
instance CBitSet Flag where
   toBitOffset   = (+1) . fromEnum
   fromBitOffset = toEnum . (\x -> x-1)
```

To use a bit set as a field in a structure, use BitSet:
```haskell
data StructZ = StructZ
   { zField0 :: ...
   , zField1 :: BitSet Word32 Flag
   } deriving (Show,Generic,Storable)
```

The first type parameter of BitSet indicates the backing word type (i.e. the
size of the field in the structure). For instance, you can use Word8, Word16,
Word32 and Word64.

Use the following methods to manipulate the BitSet:

```haskell
fromBits     :: (CBitSet a, FiniteBits b) => b -> BitSet b a
toBits       :: (CBitSet a, FiniteBits b) => BitSet b a -> b
member       :: (CBitSet a, FiniteBits b) => BitSet b a -> a -> Bool
notMember    :: (CBitSet a, FiniteBits b) => BitSet b a -> a -> Bool
toList       :: (CBitSet a, FiniteBits b) => BitSet b a -> [a]
fromList     :: (CBitSet a, FiniteBits b, Foldable m) => m a -> BitSet b a
intersection :: FiniteBits b => BitSet b a -> BitSet b a -> BitSet b a
union        :: FiniteBits b => BitSet b a -> BitSet b a -> BitSet b a
```
 
Note that we don't check if bit offsets are outside of the backing word. You
have to choose a backing word that is large enough.

### Unions

An union provides several ways to access the same buffer of memory. To use them
with Haskus, you need to give the list of available representations in a type
as follows:
```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

import Haskus.Format.Binary.Union

u :: Union '[Word8, Word64, Vector 5 Word16]
```

Unions are storable so you can use them as fields in storable structures or
you can directly `peek`/`poke` them.

You can retrieve a member of the union with `fromUnion`.  The extracted type
must be a member of the union otherwise it won't compile.
```haskell
fromUnion u :: Word64
fromUnion u :: Word8
fromUnion u :: Vector 5 Word16
fromUnion u :: Word32 -- won't compile!
```

To create a new union from one of its member, use `toUnion` or `toUnionZero`.
The latter sets the remaining bytes of the buffer to 0. In the example, the union
uses 10 bytes (5 * 2 for Vector 5 Word16) and we write 8 bytes (sizeOf Word64)
hence there are two bytes that can be left uninitialized (toUnion) or set to 0
(toUnionZero).
```haskell
u :: Union '[Word8,Word64,Vector 5 Word16]
u = toUnion (0x1122334455667788 :: Word64)

> print (fromUnion u :: Vector 5 Word16)
fromList [30600,21862,13124,4386,49850]

-- or
u = toUnionZero (0x1122334455667788 :: Word64)
> print (fromUnion u :: Vector 5 Word16)
fromList [30600,21862,13124,4386,0]
```


### Bit fields

You may need to define bit fields over words. For instance, you can
have a Word16 split into 3 fields X, Y and Z composed of 5, 9 and 2 bits
respectively.

                     X             Y          Z
    w :: Word16 |0 0 0 0 0|0 0 0 0 0 0 0 0 0|0 0|

You define it as follows:
```haskell
{-# LANGUAGE DataKinds #-}

import Haskus.Format.Binary.BitField
import Data.Proxy

w :: BitFields Word16 '[ BitField 5 "X" Word8 
                       , BitField 9 "Y" Word16
                       , BitField 2 "Z" Word8
                       ]
w = BitFields 0x0102
```

Note that each field has its own associated type (e.g. Word8 for X and Z)
that must be large enough to hold the number of bits for the field.

Operations on BitFields expect that the cumulated size of the fields is equal
to the whole word size: use a padding field if necessary.

You can extract and update the value of a field by its name:

```haskell
x = extractField (Proxy :: Proxy "X") w
z = extractField (Proxy :: Proxy "Z") w
w' = updateField (Proxy :: Proxy "Y") 0x100 w
-- w' = 0x402

z = extractField (Proxy :: Proxy "XXX") w -- won't compile

w'' = withField (Proxy :: Proxy "Y") (+2) w
```

Fields can also be 'BitSet' or 'EnumField':
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

import Haskus.Format.Binary.BitField
import Haskus.Format.Binary.Enum
import Haskus.Format.Binary.BitSet

data A = A0 | A1 | A2 | A3 deriving (Show,Enum,CEnum)

data B = B0 | B1 deriving (Show,Enum,CBitSet)

w :: BitFields Word16 '[ BitField 5 "X" (EnumField Word8 A)
                       , BitField 9 "Y" Word16
                       , BitField 2 "Z" (BitSet Word8 B)
                       ]
w = BitFields 0x1503
```

BitFields are storable and can be used in storable structures.

You can easily pattern-match on all the fields at the same time with
`matchFields` and `matchNamedFields`. It creates a tuple containing one value
(and its name with `matchNamedFields`) per field.
```haskell
> matchFields w
(EnumField A2,320,fromList [B0,B1])

> matchNamedFields  w
(("X",EnumField A2),("Y",320),("Z",fromList [B0,B1]))
```
