{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | Opcode tables
module ViperVM.Arch.X86_64.Assembler.Tables
   ( opcodeMaps
   , buildOpcodeMaps
   , buildOpcodeMap
   , genEncodingOpcodeVariants
   , MapEntry (..)
   )
where

import ViperVM.Arch.X86_64.Assembler.Operand
import ViperVM.Arch.X86_64.Assembler.Insns

import Data.Bits
import Data.Maybe
import Data.Word
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Vector as V

data MapEntry = MapEntry
   { entryInsn     :: X86Insn  -- ^ Instruction
   , entryEncoding :: Encoding -- ^ Encoding
   }

-- | Some instructions store flags and values into the opcode byte. This method
-- returns the list of potential opcodes for an encoding
genEncodingOpcodeVariants :: Encoding -> [Word8]
genEncodingOpcodeVariants e = ocs
   where
      -- the original opcode
      oc = encOpcode e

      -- reversed (check: can we have reversed + operand in opcode (or something
      -- else)?)
      (roc,rsoc) = case (encReversableBit e, encNoForce8Bit e) of
               (Just i, Nothing) -> (Just (setBit oc i), Nothing)
               (Just i, Just i2) -> (Just (setBit oc i), Just (setBit (setBit oc i2) i))
               _                 -> (Nothing,Nothing)
      -- sizable, sign-extended
      (szoc,seoc) = case (encNoForce8Bit e, encSignExtendImmBit e) of
               (Nothing,Nothing) -> (Nothing,Nothing)
               (Just i, Nothing) -> (Just (setBit oc i),Nothing)
               (Just i, Just i2) -> (Just (setBit oc i), Just (setBit (setBit oc i2) i))
               (Nothing, Just i) ->  (Nothing,Just (setBit oc i))
      -- FPU dest
      fdoc = setBit oc <$> encFPUDestBit e
      -- FPU pop
      fpoc = setBit oc <$> encFPUPopBit e
      -- FPU sizable
      fsoc = setBit oc <$> encFPUSizableBit e

      -- opcodes with differetnt flags
      ocs' = oc : catMaybes [roc,rsoc,szoc,seoc,fdoc,fpoc,fsoc]

      -- operand stored in the opcode
      ocs = if OpcodeLow3 `elem` fmap opEnc (encParams e)
               then [o + i | o <- ocs', i <- [0..7]]
               else ocs'


-- | Build an opcode map
buildOpcodeMap :: [MapEntry] -> V.Vector [MapEntry]
buildOpcodeMap entries = as
   where
      -- all pairs (opcode, MapEntry)
      es = [(oc,[e]) | e  <- entries
                     , oc <- genEncodingOpcodeVariants (entryEncoding e)
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
