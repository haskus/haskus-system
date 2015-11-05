-- | Module handling registers
module ViperVM.Arch.X86_64.Assembler.RegisterFile
   ( Register(..)
   , RegisterFile(..)
   , makeRegisterFile
   , makeRegSequence
   , mergeRegisterFiles
   , aliasedRegistersWithOffset
   , aliasedRegisters
   )
where

import Data.Word (Word32)
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map

-- | A register
data Register = Register {
   regName :: String,   -- ^ Name of the register
   regSize :: Word32,   -- ^ Size of the register in bytes
   regOffset :: Word32  -- ^ Offset in the register file
} deriving (Eq,Ord,Show)

-- | Register file
data RegisterFile = RegisterFile {
   regFileRegs :: Map String Register, -- ^ Registers by name
   regAliasing :: Map Register [(Register,Int)] -- ^ Aliased registers with offset
} deriving (Show)

-- | Create register file from a list of registers
makeRegisterFile :: [Register] -> RegisterFile
makeRegisterFile regs = RegisterFile fileRegs aliasing
   where
      fileRegs = Map.fromList [(regName reg,reg) | reg <- regs]
      aliasing = Map.fromList [(reg,aliased reg) | reg <- regs]
      aliased r = mapMaybe (f r) regs
      f r1 r2
         | r1 == r2 = Nothing
         | regOffset r2 + regSize r2 <= regOffset r1 = Nothing
         | regOffset r2 >= regOffset r1 + regSize r1 = Nothing
         | otherwise = Just (r2, fromIntegral (regOffset r1) - fromIntegral (regOffset r1))

-- | Get registers overlapping a given register and their offsets in the register file
aliasedRegistersWithOffset :: RegisterFile -> String -> [(Register,Int)]
aliasedRegistersWithOffset file name = regAliasing file Map.! reg
   where
      reg = regFileRegs file Map.! name

-- | Get registers overlapping a given register
aliasedRegisters :: RegisterFile -> String -> [Register]
aliasedRegisters file = fmap fst . aliasedRegistersWithOffset file

-- | Create a sequence of registers of the same size with "pitch" offset between each
makeRegSequence :: Word32 -> Word32 -> Word32 -> [String] -> [Register]
makeRegSequence size pitch = go []
   where
      go xs _ [] = xs
      go xs off (a:as) = go (x:xs) (off + pitch) as
         where x = Register a size off

-- | Merge several register files into a big one (non overlapping)
mergeRegisterFiles :: [RegisterFile] -> RegisterFile
mergeRegisterFiles [] = makeRegisterFile []
mergeRegisterFiles [x] = x
mergeRegisterFiles (x:xs) = foldr merge x xs
   where 
      merge f1 f2 = makeRegisterFile regs
         where
            regs = r1 ++ r2
            r1 = Map.elems $ regFileRegs f1
            r2 = shiftRegs (maxoffset r1) . Map.elems $ regFileRegs f2
      maxoffset = maximum . fmap (\y -> regOffset y + regSize y)
      shiftRegs off = fmap (\r -> r { regOffset = regOffset r + off })
