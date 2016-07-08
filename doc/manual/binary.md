# ViperVM: Binary

ViperVM has a set of modules dedicated to the manipulation of binary data. They
provide data type mapping those of other languages such as C and even more.

All these modules are in [ViperVM.Format.Binary](../../src/lib/ViperVM/Format/Binary).

## Word, Int

The [Word module](../../src/lib/ViperVM/Format/Binary/Word.hs) contains unsigned
words (Word8, Word16, Word32, etc.) and signed integers (Int8, Int16, Int32,
etc.).

### Endianness

Words and Ints are stored (i.e., read and written) using host endianness (byte
ordering). `AsBigEndian` and `AsLittleEndian` data types in the
[Endianness module](../../src/lib/ViperVM/Format/Binary/Endianness.hs)
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

You can also explicitly change the endianness with the following methods:
* hostToBigEndian
* hostToLittleEndian
* bigEndianToHost
* littleEndianToHost
* reverseBytes

## Bits

The [Bits module](../../src/lib/ViperVM/Format/Binary/Bits.hs) allows you to
perform bitwise operations on data types supporting them.
