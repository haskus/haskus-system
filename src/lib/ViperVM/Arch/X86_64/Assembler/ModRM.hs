{-# LANGUAGE DataKinds #-}

module ViperVM.Arch.X86_64.Assembler.ModRM
   ( ModRM(..)
   , SIB(..)
   , Scale(..)
   , RMMode(..)
   , Mode(..)
   , newModRM
   , rmField
   , regField
   , modField
   , modeField
   , modRMFields
   , rmMode
   , useDisplacement
   , useSIB
   , scaleField
   , indexField
   , baseField
   , rmRegMode
   )
where

import Data.Word
import Data.Bits
import Data.Proxy

import ViperVM.Arch.X86_64.Assembler.Size
import ViperVM.Format.Binary.BitField

-- | ModRM byte
newtype ModRM = ModRM (BitFields Word8
  '[ BitField 2 "mode" Word8
   , BitField 3 "reg"  Word8
   , BitField 3 "rm"   Word8
   ])
   deriving (Show,Eq)

-- | SIB byte
newtype SIB = SIB Word8 deriving (Show,Eq)

-- | SIB scale factor
data Scale
   = Scale1 
   | Scale2 
   | Scale4 
   | Scale8 
   deriving (Show,Eq)

-- | Mode for the R/M field
data RMMode
   = RMRegister   -- ^ Direct register addressing
   | RMBaseIndex  -- ^ Memory addressing with only base/index register
   | RMSIB        -- ^ Memory addressing with SIB byte
   deriving (Show, Eq)

-- | Create a ModRM byte (check inputs)
newModRM :: Word8 -> Word8 -> Word8 -> ModRM
newModRM md rm reg
   | md  > 3 = error "Invalid value for mod field (> 3)"
   | rm  > 7 = error "Invalid value for rm field (> 7)"
   | reg > 7 = error "Invalid value for reg field (> 7)"
   | otherwise = ModRM
         $ updateField (Proxy :: Proxy "mode") md
         $ updateField (Proxy :: Proxy "reg")  reg
         $ updateField (Proxy :: Proxy "rm")   rm
         $ BitFields 0


-- | Get r/m field in ModRM
rmField :: ModRM -> Word8
rmField (ModRM x) = extractField (Proxy :: Proxy "rm") x

-- | Get reg field in ModRM
regField :: ModRM -> Word8
regField (ModRM x) = extractField (Proxy :: Proxy "reg") x

-- | Get mod field in ModRM
modField :: ModRM -> Word8
modField (ModRM x) = extractField (Proxy :: Proxy "mode") x


-- | Mode for pattern matching
data Mode
   = Mode00
   | Mode01
   | Mode10
   | Mode11
   deriving (Show,Eq,Enum)

-- | Get mod field in ModRM
modeField :: ModRM -> Mode
modeField = toEnum . fromIntegral . modField

-- | Get the tree fields (mod,reg,rm)
modRMFields :: ModRM -> (Word8,Word8,Word8)
modRMFields (ModRM x) = matchFields x

-- | Indicate R/M field mode
rmMode :: AddressSize -> ModRM -> RMMode
rmMode sz rm = case (sz, modField rm, rmField rm) of
   (_,3,_)          -> RMRegister
   (AddrSize16,_,_) -> RMBaseIndex
   (_,_,4)          -> RMSIB
   _                -> RMBaseIndex

-- | Indicate if the r/m field contains a register
rmRegMode :: ModRM -> Bool
rmRegMode rm = modField rm == 3

-- | Indicate if displacement bytes follow
useDisplacement :: AddressSize -> ModRM -> Maybe Size
useDisplacement sz modrm = case (sz,modField modrm,rmField modrm) of
   (AddrSize16, 0, 6) -> Just Size16
   (AddrSize16, 1, _) -> Just Size8
   (AddrSize16, 2, _) -> Just Size16
   (AddrSize16, _, _) -> Nothing

   -- 64 bit uses 32 bit addressing
   (_, 0, 5)          -> Just Size32
   (_, 1, _)          -> Just Size8
   (_, 2, _)          -> Just Size32
   _                  -> Nothing

-- | Indicate if a SIB byte follows
useSIB :: AddressSize -> ModRM -> Bool
useSIB sz modrm = case (sz,modField modrm,rmField modrm) of
   (AddrSize16, _, _) -> False -- no SIB in 16 bit addressing
   (_, 3, _)          -> False -- direct register addressing
   (_, _, 4)          -> True
   _                  -> False


-- | Get SIB scale field
scaleField :: SIB -> Scale
scaleField (SIB x) = case x `shiftR` 6 of
   0 -> Scale1
   1 -> Scale2
   2 -> Scale4
   3 -> Scale8
   _ -> error "Invalid scaling factor"

-- | Get SIB index field
indexField :: SIB -> Word8
indexField (SIB x) = (x `shiftR` 3) .&. 0x07

-- | Get SIB base field
baseField :: SIB -> Word8
baseField (SIB x) = x .&. 0x07

