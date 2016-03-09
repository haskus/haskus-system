# Writing Bindings with ViperVM

ViperVM does not rely on external tools such as C2HS to provide bindings. There
are several reasons for that:

* We don't want to depend on .h files;
* .h files often contain pecularities that are difficult to handle
  automatically;
* Documentation and code of the resulting Haskell files are often very bad:
    * No haddock
    * Very low-level (e.g. #define are not transformed into datatypes with Enum
      instances)

Instead with ViperVM you write your bindings in pure Haskell code. ViperVM
provides many useful things to make this process easy.

## Structures

You map C data structures with Haskell data type as follows:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import Foreign.Storable
import Foreign.CStorable
import GHC.Generics (Generic)

data StructX = StructX
   { xField0 :: Word8
   , xField1 :: Word64
   } deriving (Show,Generic,CStorable)

instance Storable StructX where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf
```

The CStorable instance handles the alignment of the field as a C non-packed
structure would (i.e. there are 3 bytes between xField0 and xField1).

The boilerplate Storable instance cannot be derived automatically for now. We
use CStorable members to define it.

`peek` and `poke` can be used to read and write the data structure in memory.

### Nesting

Data structures can be nested:

```haskell
data StructY = StructY
   { yField0 :: StructX
   , yField1 :: Word64
   } deriving (Show,Generic,CStorable)
```

### Enums

If you have a C enum (or a set of #define's) with consecutive values and
starting at 0, you can do:

```haskell
{-# LANGUAGE DeriveAnyClass #-}

import ViperVM.Format.Binary.Enum

data MyEnum
   = MyEnumX
   | MyEnumY
   | MyEnumZ
   deriving (Show,Eq,Enum,CEnum)
```

If the values are not consecutive or don't start a 0, you can write your own
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
   } deriving (Show,Generic,CStorable)
```

The first type parameter of EnumField indicates the backing word type (i.e. the
size of field in the structure). For instance, you can use Word8, Word16, Word32
and Word64.

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

ViperVM uses CEnum to get the bit position of flags as in the following example:

```haskell
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
   } deriving (Show,Generic,CStorable)
```

The first type parameter of BitSet indicates the backing word type (i.e. the
size of field in the structure). For instance, you can use Word8, Word16, Word32
and Word64.

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
 
Note that we don't check if bit offsets are outside of b.

### Unions

An union provides several ways to access the same buffer of memory. To use them
with ViperVM, you need to give the list of available representations in a type
as follows:

```haskell
u :: Union '[Word8,Word64,Vector 5 Word16]
```

Unions are storable so you can use them as fields in storable structures or
directly peek/poke them.

You can retrieve a member of the union with `fromUnion`:
```haskell
fromUnion u :: Word64
```
The extracted type must be a member of the union otherwise it won't compile.

To create a new union from one of its member, use `toUnion` or `toUnionZero`:
```haskell
u :: Union '[Word8,Word64,Vector 5 Word16]
u = toUnion (0x1122334455667788 :: Word64)
-- or
u = toUnionZero (0x1122334455667788 :: Word64)
```

The latter set the remaining bytes of the buffer to 0. In the example, the union
uses 10 bytes (5 * 2 for Vector 5 Word16) and we write 8 bytes (sizeOf Word64)
hence there are two bytes that can be left uninitialized (toUnion) or set to 0
(toUnionZero).


### Bit fields

You may need to define bit fields over words. For instance, you can
have a Word16 split into 3 fields X, Y and Z composed of 5, 9 and 2 bits
respectively.

                     X             Y          Z
    w :: Word16 |0 0 0 0 0|0 0 0 0 0 0 0 0 0|0 0|

You define it as follows:
```haskell
{-# LANGUAGE DataKinds #-}

w :: BitFields Word16 '[ BitField 5 "X" Word8 
                       , BitField 9 "Y" Word16
                       , BitField 2 "Z" Word8
                       ]
w = BitFields 0x01020304
```

Note that each field has its own associated type (e.g. Word8 for X and Z)
that must be large enough to hold the number of bits for the field.

Operations on BitFields expect that the cumulated size of the fields is equal
to the whole word size: use a padding field if necessary.

You can extract and update the value of a field by its name:

```haskell
x = extractField (Proxy :: Proxy "X") w
z = extractField (Proxy :: Proxy "Z") w
w' = updateField (Proxy :: Proxy "Y") 0x5566 w
```

Fields can also be 'BitSet' or 'EnumField':
```haskell
{-# LANGUAGE DataKinds #-}

data A = A0 | A1 | A2 | A3 deriving (Enum,CEnum)

data B = B0 | B1 deriving (Enum,CBitSet)

w :: BitFields Word16 '[ BitField 5 "X" (EnumField Word8 A)
                       , BitField 9 "Y" Word16
                       , BitField 2 "Z" (BitSet Word8 B)
                       ]
w = BitFields 0x01020304
```
