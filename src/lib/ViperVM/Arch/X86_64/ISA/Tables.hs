{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | Opcode tables
module ViperVM.Arch.X86_64.ISA.Tables
   ( opcodeMaps
   , buildOpcodeMaps
   , buildOpcodeMap
   , MapEntry (..)
   )
where

import ViperVM.Arch.X86_64.ISA.Insns
import ViperVM.Arch.X86_64.ISA.Encoding

import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Vector as V

-- | Entry in the opcode table
data MapEntry = MapEntry
   { entryInsn     :: X86Insn  -- ^ Instruction
   , entryEncoding :: Encoding -- ^ Encoding
   }
   deriving (Show)

-- | Build an opcode map
buildOpcodeMap :: [MapEntry] -> V.Vector [MapEntry]
buildOpcodeMap entries = as
   where
      -- all pairs (opcode, MapEntry)
      es = [(oc,[e]) | e  <- entries
                     , oc <- encGenerateOpcodes (entryEncoding e)
                     ]

      -- Map opcode [MapEntry]
      ks = Map.fromListWith (++) es

      -- Vector
      as = V.generate 256 (fromMaybe [] . (`Map.lookup` ks) . fromIntegral)

-- | Build opcode maps
buildOpcodeMaps :: [X86Insn] -> Map OpcodeMap (V.Vector [MapEntry])
buildOpcodeMaps is = buildOpcodeMap <$> Map.fromListWith (++) es
   where
      -- all map entries
      es = [ (encOpcodeMap e,[MapEntry i e]) | i <- is
                                             , e <- insnEncodings i
                                             ]
-- | Opcode maps
opcodeMaps :: Map OpcodeMap (V.Vector [MapEntry])
opcodeMaps = buildOpcodeMaps instructions
