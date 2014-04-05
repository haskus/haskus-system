module ViperVM.Arch.Common.Endianness (Endianness(..)) where


-- | Memory endianness
data Endianness = 
     LittleEndian 
   | BigEndian 
   deriving (Eq,Show)
