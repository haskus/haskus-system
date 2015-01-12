-- | Memory endianness
--
-- Indicate in which order bytes are stored in memory for multi-bytes types.
-- Big-endian means that most-significant bytes come first. Little-endian means
-- that least-significant bytes come first.
module ViperVM.Arch.Common.Endianness
   ( Endianness(..)
   )
where


-- | Memory endianness
data Endianness 
   = LittleEndian    -- ^ Less significant bytes first
   | BigEndian       -- ^ Most significant bytes first
   deriving (Eq,Show)
