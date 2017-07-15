-- | Register
module Haskus.Arch.Common.Register
   ( Reg (..)
   , TypedReg (..)
   )
where

-- | Register
data Reg banks = Reg
   { registerBank   :: banks                -- ^ Register bank
   , registerId     :: {-# UNPACK #-} !Word -- ^ Register ID
   , registerSize   :: {-# UNPACK #-} !Word -- ^ Register size in bits
   , registerOffset :: {-# UNPACK #-} !Word -- ^ Register offset in bits (alias register)
   }
   deriving (Show,Eq,Ord)


-- | Typed register
data TypedReg typ bank = TypedReg
   { typedReg     :: Reg bank   -- ^ Register
   , typedRegType :: typ        -- ^ Register type
   }
   deriving (Show,Eq,Ord)
