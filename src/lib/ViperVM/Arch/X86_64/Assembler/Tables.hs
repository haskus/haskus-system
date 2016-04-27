{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | Opcode tables
module ViperVM.Arch.X86_64.Assembler.Tables
   ( FlaggedOpcode(..)
   , opcodeMapPrimary
   , opcodeMap0F
   , opcodeMap0F38
   , opcodeMap0F3A
   , opcodeMap3DNow
   , opcodeMapVex1
   , opcodeMapVex2
   , opcodeMapVex3
   , buildLegacyOpcodeMap
   , buildVexOpcodeMap
   , getLegacyOpcodes
   )
where

import ViperVM.Arch.X86_64.Assembler.Operand
import ViperVM.Arch.X86_64.Assembler.Insns
import ViperVM.Arch.X86_64.Assembler.Size

import Data.Bits
import Data.Maybe
import Data.Word
import qualified Data.Map as Map
import qualified Data.Vector as V

data MapEntry = MapEntry
   { entryInsn :: X86Insn        -- ^ Instruction
   , entryEncoding :: Encoding   -- ^ Encoding
   }

data MatchedEntry = MatchedEntry
   { matchedInsn        :: X86Insn
   , matchedEncoding    :: Encoding
   , matchedVariant     :: EncodingVariant
   , matchedOperandSize :: OperandSize
   , matchedParams      :: [Operand]
   }

data FlaggedOpcode = FlaggedOpcode
   { fgOpcode        :: Word8
   , fgReversed      :: Bool
   , fgSized         :: Bool
   , fgSignExtended  :: Bool
   } deriving (Show)

-- | Return the different opcodes for a legacy encoding
getLegacyOpcodes :: Encoding -> [FlaggedOpcode]
getLegacyOpcodes e = os
   where
      sz = legacySizable e
      rv = legacyReversable e
      se = legacySignExtendable e
      oc = legacyOpcode e
   
      os' = orig : szb ++ opb
      -- with reversable bit set
      os = case rv of
         Nothing -> os'
         Just x  -> os' ++ fmap rev os'
            where
               rev o = o { fgOpcode   = setBit (fgOpcode o) x
                         , fgReversed = True
                         }

      -- original opcode
      orig = FlaggedOpcode oc False False False
      -- with sizable and sign-extendable bits
      szb = case (sz,se) of
         (Nothing,Nothing) -> []
         (Nothing,Just _)  -> error "Invalid opcode fields"
         (Just x,Nothing)  -> [FlaggedOpcode (setBit oc x) False True False]
         (Just x,Just y)   -> 
            [ FlaggedOpcode (setBit oc x)            False True False
            , FlaggedOpcode (setBit (setBit oc x) y) False True True
            ]
      -- with operand in the last 3 bits of the opcode
      opb = case OpcodeLow3 `elem` fmap opEnc (legacyParams e) of
         False -> []
         True  ->  fmap (\x -> FlaggedOpcode (oc+x) False False False) [1..7]

getEncodings :: [X86Insn] -> [(Encoding,X86Insn)]
getEncodings = concatMap f
   where
      f x = fmap (,x) (insnEncodings x)

getVexOpcodes :: Encoding -> [FlaggedOpcode]
getVexOpcodes e = [FlaggedOpcode (vexOpcode e) False False False]


            
-- | Build the opcode maps
buildOpcodeMap :: [(Encoding,X86Insn)] -> V.Vector [(Encoding,X86Insn)]
buildOpcodeMap encs = go encs Map.empty
   where
      go [] rs     = V.generate 256 (fromMaybe [] . (`Map.lookup` rs))
      go ((e,x):xs) rs = let
            os = fmap (fromIntegral . fgOpcode) (getOpcodes e)
         in go xs (insertAll os (e,x) rs)
      
      getOpcodes = \case
         x@LegacyEncoding {} -> getLegacyOpcodes x
         x@VexEncoding    {} -> getVexOpcodes x

      insertAll [] _ rs     = rs
      insertAll (o:os) x rs = insertAll os x (Map.insertWith (++) o [x] rs)


opcodeMapPrimary :: V.Vector [(Encoding,X86Insn)]
opcodeMapPrimary = buildLegacyOpcodeMap MapPrimary instructions

opcodeMap0F :: V.Vector [(Encoding,X86Insn)]
opcodeMap0F = buildLegacyOpcodeMap Map0F instructions

opcodeMap0F38 :: V.Vector [(Encoding,X86Insn)]
opcodeMap0F38 = buildLegacyOpcodeMap Map0F38 instructions

opcodeMap0F3A :: V.Vector [(Encoding,X86Insn)]
opcodeMap0F3A = buildLegacyOpcodeMap Map0F3A instructions

opcodeMap3DNow :: V.Vector [(Encoding,X86Insn)]
opcodeMap3DNow = buildLegacyOpcodeMap Map3DNow instructions

opcodeMapVex1 :: V.Vector [(Encoding,X86Insn)]
opcodeMapVex1 = buildVexOpcodeMap (MapVex 1) instructions

opcodeMapVex2 :: V.Vector [(Encoding,X86Insn)]
opcodeMapVex2 = buildVexOpcodeMap (MapVex 2) instructions

opcodeMapVex3 :: V.Vector [(Encoding,X86Insn)]
opcodeMapVex3 = buildVexOpcodeMap (MapVex 3) instructions


-- | Build a legacy opcode map
buildLegacyOpcodeMap :: LegacyMap -> [X86Insn] -> V.Vector [(Encoding,X86Insn)]
buildLegacyOpcodeMap omap insns = buildOpcodeMap encs
   where
      encs = filter (ff . fst) (getEncodings insns)
      ff = \case
         x@LegacyEncoding {} -> legacyOpcodeMap x == omap 
         _                   -> False

-- | Build a VEX opcode map
buildVexOpcodeMap :: OpcodeMap -> [X86Insn] -> V.Vector [(Encoding,X86Insn)]
buildVexOpcodeMap omap insns = buildOpcodeMap encs
   where
      encs = filter (ff . fst) (getEncodings insns)
      ff = \case
         x@VexEncoding {} -> vexOpcodeMap x == omap 
         _                -> False
            
