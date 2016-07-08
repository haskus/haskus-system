# ViperVM: Binary

ViperVM has a set of modules dedicated to the manipulation of binary data. They
provide data type mapping those of other languages such as C and even more.

All these modules are in [ViperVM.Format.Binary](src/lib/ViperVM/Format/Binary).

## Word

This module contains unsigned words (Word8, Word16, Word32, etc.) and signed
integers (Int8, Int16, Int32, etc.).

### Endianness

Words and Ints are stored (i.e., read and written) using host endianness (byte
ordering). `AsBigEndian` and `AsLittleEndian` data types in the `Endianness`
module allow you to force a different endianness.

You can also explicitly change the endianness with the following methods:

* hostToBigEndian
* hostToLittleEndian
* bigEndianToHost
* littleEndianToHost
* reverseBytes
