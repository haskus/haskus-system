module ViperVM.Arch.X86_64.Assembler.VexPrefix
   ( Vex(..)
   , vexW
   , vexR
   , vexX
   , vexB
   , vexL
   , vexVVVV
   , vexMMMMM
   , vexPP
   , decodeVEX
   , decodeXOP
   ) where

import Data.Word
import Data.Bits
import Control.Monad.State

import ViperVM.Arch.X86_64.Assembler.X86Dec
import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.Size

data Vex
   = Vex2 !Word8
   | Vex3 !Word8 !Word8
   deriving (Show)

vexW :: Vex -> Maybe Bool
vexW (Vex2 _) = Nothing
vexW (Vex3 _ x) = Just (testBit x 7)

vexR :: Vex -> Bool
vexR (Vex2 x) = not $ testBit x 7
vexR (Vex3 x _) = not $ testBit x 7

vexX :: Vex -> Maybe Bool
vexX (Vex2 _) = Nothing
vexX (Vex3 x _) = Just (not $ testBit x 6)

vexB :: Vex -> Maybe Bool
vexB (Vex2 _) = Nothing
vexB (Vex3 x _) = Just (not $ testBit x 5)

vexL :: Vex -> Bool
vexL (Vex2 x) = testBit x 2
vexL (Vex3 _ x) = testBit x 2

vexVVVV :: Vex -> Word8
vexVVVV (Vex2 x) = (x `shiftR` 3) .&. 0x0F
vexVVVV (Vex3 _ x) = (x `shiftR` 3) .&. 0x0F

vexPP :: Vex -> Word8
vexPP (Vex2 x) = x .&. 0x03
vexPP (Vex3 _ x) = x .&. 0x03

vexMMMMM :: Vex -> Maybe Word8
vexMMMMM (Vex2 _) = Nothing
vexMMMMM (Vex3 x _) = Just $ x .&. 0x1F

vexMapSelect :: Vex -> [Word8]
vexMapSelect v = case (v, vexMMMMM v) of
   (Vex2 _, _) -> [0x0F]
   (_, Just 1) -> [0x0F]
   (_, Just 2) -> [0x0F,0x38]
   (_, Just 3) -> [0x0F,0x3A]
   _           -> error "Reserved map select in VEX/XOP prefix"
   

-- Try to decode VEX prefixes
decodeVEX :: X86Dec ()
decodeVEX = do
   mode        <- getMode
   allowedSets <- getAllowedSets

   when (SetVEX `elem` allowedSets) $ lookWord8 >>= \x -> do
      when (x .&. 0xFE == 0xC4) $ do
         when (is64bitMode mode) $ decodeVex' x

         -- VEX prefixes are supported in 32-bit and 16-bit modes
         -- They overload LES and LDS opcodes so that the first two bits
         -- of what would be ModRM are invalid (11b) for LES/LDS
         when (not $ is64bitMode mode) $ lookWord16 >>= \y ->
            when (y `shiftR` 14 == 0x3) $ decodeVex' x

decodeVex' :: Word8 -> X86Dec ()
decodeVex' x = do
  assertNoRex ErrRexPrefixBeforeVex
  assertNoLegacyPrefix ErrLegacyPrefixBeforeVex [0xF0,0x66,0xF3,0xF2]
  assertNoXop ErrXopPrefixBeforeVex
  skipWord8
  vex <- case x of
     0xC4 -> Vex3 <$> nextWord8 <*> nextWord8
     0xC5 -> Vex2 <$> nextWord8
     _    -> error "Invalid VEX prefix"
  modify (\s -> s { stateHasVexPrefix = True})
  decodeVexXop vex

-- | Decode a XOP prefix
--
-- XOP is just like Vex3 except that the first byte is 0x8F instead of 0xC4
decodeXOP :: X86Dec ()
decodeXOP = do
   allowedSets <- getAllowedSets

   -- Try to decode XOP prefix
   when (SetXOP `elem` allowedSets) $ lookWord8 >>= \x -> do
      when (x == 0x8F) $ do
         assertNoRex ErrRexPrefixBeforeXop
         assertNoVex ErrRexPrefixBeforeXop
         assertNoLegacyPrefix ErrLegacyPrefixBeforeXop [0xF0,0x66,0xF3,0xF2]
         skipWord8
         vex <- Vex3 <$> nextWord8 <*> nextWord8
         modify (\y -> y { stateHasXopPrefix = True})
         decodeVexXop vex


-- | Decode a VEX or XOP prefix
decodeVexXop :: Vex -> X86Dec ()
decodeVexXop vex = do
   modify (\s -> s
      { stateBaseRegExt       = case vexB vex of
                                 Nothing    -> stateBaseRegExt s
                                 Just True  -> 1
                                 Just False -> 0
      , stateIndexRegExt      = case vexX vex of
                                 Nothing    -> stateIndexRegExt s
                                 Just True  -> 1
                                 Just False -> 0
      , stateRegExt           = case vexR vex of
                                 True  -> 1
                                 False -> 0
      , stateOperandSize      = case vexW vex of
                                 Just True -> OpSize64
                                 _         -> stateOperandSize s
      , stateOpcodeExtE       = vexW vex
      , stateMapSelect        = vexMapSelect vex
      , stateAdditionalOp     = Just (vexVVVV vex)
      , stateLegacyPrefixes   = stateLegacyPrefixes s ++ case vexPP vex of
                                 0 -> []
                                 1 -> [0x66]
                                 2 -> [0xF3]
                                 3 -> [0xF2]
                                 _ -> error "Invalid PP in VEX prefix"
      })

