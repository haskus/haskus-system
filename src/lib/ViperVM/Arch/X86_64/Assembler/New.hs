{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}

module ViperVM.Arch.X86_64.Assembler.New
   (
   )
where

import ViperVM.Arch.X86_64.Assembler.Opcode
import ViperVM.Arch.X86_64.Assembler.LegacyPrefix
import ViperVM.Arch.X86_64.Assembler.RexPrefix
import ViperVM.Arch.X86_64.Assembler.VexPrefix
import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.ModRM
import ViperVM.Arch.X86_64.Assembler.Operand
import ViperVM.Arch.X86_64.Assembler.Registers
import ViperVM.Arch.X86_64.Assembler.Size

import ViperVM.Utils.MultiState
import ViperVM.Utils.HArray
import ViperVM.Format.Binary.Reader
import ViperVM.Format.Binary.Buffer
import ViperVM.Format.Binary.BitField

import Data.List (nub)
import Data.Bits
import Data.Word
import Data.Maybe
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Identity

-- ===========================================================================
-- Legacy encoding
-- ===========================================================================

---------------------------------------------------------------------------
-- Legacy prefixes
-- ~~~~~~~~~~~~~~~
-- 
-- An instruction optionally begins with up to five legacy prefixes, in any
-- order. These prefixes can:
--    1) modify the instruction's default address size
--    2) modify the instruction's operand size
--    3) modify the instruction's address segment
--    4) be used as an opcode extension
--    5) provide atomic bus locking
--    6) repeat the instruction until a condition is met
--
-- The operand size and the default address size are not necessarily the
-- same: the pointed address-sized memory is sign-extended or zero-extended
-- to have the operand size.
--
-- Legacy prefixes that are used as opcode extensions are mandatory...
---------------------------------------------------------------------------

-- | Read legacy prefixes (up to 5)
readLegacyPrefixes :: 
   (ReaderM () s
   ) => MState s LegacyPrefixes
readLegacyPrefixes = do
   ws <- forM [0..4] $ \(_ :: Int) -> do
      w <- binPeek
      if isLegacyPrefix w
         then binSkip 1 >> return (Just w)
         else return Nothing

   return $ LegacyPrefixes (checkLegacyPrefixes (catMaybes ws))

---------------------------------------------------------------------------
-- Legacy prefix groups
-- ~~~~~~~~~~~~~~~~~~~~
--
-- Legacy prefixes are organized in five groups. An instruction may include
-- at most one prefix from each group. The result of using multiple prefixes
-- from a single group is undefined.
--
-- We give the original meaning in parentheses, but prefixes can be used with
-- other meanings.
--
-- G1: 0x66 (Operand-size override)
-- G2: 0x67 (Address-size override)
-- G3: 0x2E (CS segment override)
--     0x3E (DS segment override)
--     0x26 (ES segment override)
--     0x64 (FS segment override)
--     0x65 (GS segment override)
--     0x36 (SS segment override)
-- G4: 0xF0 (atomic memory access (lock))
-- G5: 0xF3 (repeat while zero)
--     0xF2 (repeat while non-zero)
---------------------------------------------------------------------------

-- | Check that legacy prefixes belong to different groups
checkLegacyPrefixes :: [Word8] -> [Word8]
checkLegacyPrefixes ps = if test
      then ps
      else error "checkLegacyPrefixes: instruction using several prefixes from the same group"

   where
      prefixGroup x = case x of
         0x66 -> 1
         0x67 -> 2
         0x2E -> 3
         0x3E -> 3
         0x26 -> 3
         0x64 -> 3
         0x65 -> 3
         0x36 -> 3
         0xF0 -> 4
         0xF3 -> 5
         0xF2 -> 5
         _    -> error "checkLegacyPrefixes: invalid legacy prefix"

      test  = length ps /= length (nub (map prefixGroup ps))
   

---------------------------------------------------------------------------
-- REX prefix
-- ~~~~~~~~~~
--
-- In 64-bit mode, a REX prefix can be used after the legacy prefixes. A REX
-- prefix contains several fields, hence it ranges from 0x40 to 0x4F. In
-- non-64-bit mode, this range is used by the short variants of the INC/DEC
-- instructions, hence these forms are not usable in 64-bit mode.
--
-- There are several things to consider:
--    1) whether a REX prefix is present (whatever it contains)
--    2) the contents of the REX prefix fields.
--
-- Its presence implies:
--    - the use of the uniform byte registers (SIL, DIL, etc. instead of AH,
--    BH, etc.)
--
-- Its fields indicate:
--    - the use of the extended registers (one additional bit per register)
--    - the use of a 64-bit operand size (ignoring the operand-size overriding
--    legacy prefix)
--
-- Some instructions have default or fixed operand size set to 64bits in 64-bit
-- mode, hence they don't require the REX prefix.
--
-- The prefix has the following format:
--
-- |    4    | W | R | X | B |
--                         ^-- base register or ModRM.rm extension
--                     ^------ SIB.index register extension
--                 ^---------- ModRM.reg register extension
--             ^-------------- set to 1 for 64-bit operand size
--      ^--------------------- 4 bits set to 0xD
--
-- 
-- If more than one REX prefix is present, the behavior is undefined (however it
-- seems that the last one is used).
-- 
---------------------------------------------------------------------------

-- | Read optional REX prefix
readRexPrefix ::
   ( ReaderM () s
   , HArrayIndexT X86Mode s
   ) => MState s (Maybe Rex)
readRexPrefix = do
   
   mode <- mGet
   w <- binPeek

   -- REX is only supported in 64-bit mode
   if is64bitMode mode && isRexPrefix w
      then binSkip 1 >> return (Just (Rex w))
      else return Nothing

---------------------------------------------------------------------------
-- Legacy opcodes
-- ~~~~~~~~~~~~~~
--
-- Legacy opcode can belong to one of the following opcode maps:
--    - Primary
--    - Secondary (escaped with 0x0F)
--    - 0x0F38
--    - 0x0F3A
--    - X87
--    - 3DNow! (escaped with 0x0F0F, opcode byte in last instruction byte)
---------------------------------------------------------------------------

-- | Read legacy opcode
readLegacyOpcode ::
   ( ReaderM () s
   , HArrayIndexT X86Mode s
   ) => MState s (Maybe Opcode)
readLegacyOpcode = do
   ps  <- readLegacyPrefixes
   rex <- readRexPrefix
   w   <- binPeek

   let
      -- TODO: use mode or sets...
      is3DNowAllowed    = True
      isX87Allowed      = True
      ret m x = return (Just (OpLegacy ps rex m x))

   case w of
      _ | isX87Allowed && w .&. 0xF8 == 0xD8 -> do
            binSkip 1
            ret MapX87 w

      0x0F -> binSkip 1 >> binRead >>= \case
         
         0x0F | is3DNowAllowed ->
            -- the real 3DNow! opcode is stored in the last byte and will be set
            -- later
            ret Map3DNow 0

         0x3A -> ret Map0F3A =<< binRead
         0x38 -> ret Map0F38 =<< binRead
         w2   -> ret Map0F w2

      w1 -> ret MapPrimary w1

-- ===========================================================================
-- VEX/XOP encodings
-- ===========================================================================

---------------------------------------------------------------------------
-- VEX/XOP prefixes
-- ~~~~~~~~~~~~~~~~
--
-- VEX/XOP prefixes are different from the REX prefix: they don't extend
-- existing instructions but add new ones (new opcode maps). Moreover they are
-- mutually exclusive with the REX prefix as they subsume it.
--
---------------------------------------------------------------------------

-- | Read VEX/XOP encoded opcode
readVexXopOpcode ::
   ( ReaderM () s
   , HArrayIndexT X86Mode s
   ) => MState s (Maybe Opcode)
readVexXopOpcode = do
   w    <- binPeek
   w16  <- binPeek
   mode <- mGet

   let
      -- TODO: use mode or sets...
      isXOPAllowed      = True
      isVEXAllowed      = True

      -- VEX prefixes are supported in 32-bit and 16-bit modes
      -- They overload LES and LDS opcodes so that the first two bits
      -- of what would be ModRM are invalid (11b) for LES/LDS
      modrmMode         = (w16 :: Word16) `unsafeShiftR` 14
      isVexMode         = is64bitMode mode || modrmMode == 0x03

   case (w :: Word8) of
      0x8F  |  isXOPAllowed -> do
                  binSkip 1
                  o <- OpXop <$> (Vex3 <$> binRead <*> binRead) <*> binRead
                  return (Just o)

      0xC4  |  isVEXAllowed && isVexMode -> do
                  binSkip 1
                  o <- OpVex <$> (Vex3 <$> binRead <*> binRead) <*> binRead
                  return (Just o)

      0xC5  |  isVEXAllowed && isVexMode -> do
                  binSkip 1
                  o <- OpVex <$> (Vex2 <$> binRead) <*> binRead
                  return (Just o)

      _  -> return Nothing

      
-- ===========================================================================
-- Generic opcode reading
-- ===========================================================================


-- | Read the opcode encoding
readOpcode ::
   ( ReaderM () s
   , HArrayIndexT X86Mode s
   ) => MState s (Maybe Opcode)
readOpcode = firstJust
      -- check for overloaded prefixes/opcodes first!
      [ readVexXopOpcode
      , readLegacyOpcode
      ]
   where
      -- return the first returned Just
      firstJust :: [MState s (Maybe a)] -> MState s (Maybe a)
      firstJust []     = return Nothing
      firstJust (x:xs) = do
         r <- x
         case r of
            Just _  -> return r
            Nothing -> firstJust xs

-- ===========================================================================
-- Operands
-- ===========================================================================

----------------------------------------
-- Read operands
----------------------------------------

data Insn
   = InsnInvalid
   | InsnReserved
   | InsnX87 X87Op [X87Operand]

readOperands ::
   ( ReaderM () s
   , HArrayIndexT Opcode s
   , HArrayIndexT X86Mode s
   ) => MState s Insn
readOperands = do

   opcode <- mGet

   case opcode of
      OpLegacy _ _ MapX87 x -> readX87Operands x


-- | Extended ModRM.reg (with REX.R, VEX.R, etc.)
getExtReg ::
   ( HArrayIndexT Opcode s
   ) => ModRM -> MState s Word8
getExtReg m = do
   let
      f x = x `unsafeShiftL` 3 .|. regField m
      g x = if x then f 1 else f 0

   mGet >>= \case
      OpVex v _                 -> return $ g (vexR v)
      OpXop v _                 -> return $ g (vexR v)
      OpLegacy _ (Just rex) _ _ -> return $ f (rexR rex)
      OpLegacy _ Nothing    _ _ -> return $ regField m
      
-- | Extended ModRM.rm (with REX.B, VEX.B, etc.)
getExtRM ::
   ( HArrayIndexT Opcode s
   ) => ModRM -> MState s Word8
getExtRM m = do
   let
      f x = x `unsafeShiftL` 3 .|. rmField m
      g x = if x then f 1 else f 0
      h x = g (fromMaybe False x)

   mGet >>= \case
      OpVex v _                 -> return $ h (vexB v)
      OpXop v _                 -> return $ h (vexB v)
      OpLegacy _ (Just rex) _ _ -> return $ f (rexB rex)
      OpLegacy _ Nothing    _ _ -> return $ rmField m


data VectorLength
   = VL128
   | VL256
   deriving (Show,Eq)

-- | Get vector length (stored in VEX.L, XOP.L, etc.)
getVectorLength ::
   ( HArrayIndexT Opcode s
   ) => MState s (Maybe VectorLength)
getVectorLength = do
   op <- mGet
   case op of
      OpVex v _ -> return . Just $ if vexL v
         then VL256
         else VL128
      OpXop v _ -> return . Just $ if vexL v
         then VL256
         else VL128
      _         -> return Nothing

-- | Get the opcode map
getOpcodeMap ::
   (HArrayIndexT Opcode s
   ) => MState s OpcodeMap
getOpcodeMap = mGet >>= \case
   OpLegacy _ _ t _ -> return $ MapLegacy t
   OpVex  v    _    -> return $ vexMapSelect v
   OpXop  v    _    -> return $ vexMapSelect v


getAddr :: ModRM -> MState s Addr
getAddr m = undefined

-- getAddr :: AddressSize -> ModRM -> Addr
-- getAddr asize m = undefined
-- do
--    case asize of
--       -- if we are in 16-bit addressing mode, we don't care about the base
--       -- register extension
--       AddrSize16 -> case (modField m, rmField modrm) of
--          (_,0) -> Addr (Just R_BX) (Just R_SI) disp Nothing
--          (_,1) -> Addr (Just R_BX) (Just R_DI) disp Nothing
--          (_,2) -> Addr (Just R_BP) (Just R_SI) disp Nothing
--          (_,3) -> Addr (Just R_BP) (Just R_DI) disp Nothing
--          (_,4) -> Addr (Just R_SI) Nothing     disp Nothing
--          (_,5) -> Addr (Just R_DI) Nothing     disp Nothing
--          (0,6) -> Addr Nothing     Nothing     disp Nothing
--          (_,6) -> Addr (Just R_BP) Nothing     disp Nothing
--          (_,7) -> Addr (Just R_BX) Nothing     disp Nothing
--          _     -> error "Invalid 16-bit addressing"
-- 


----------------------------------------
-- X87
----------------------------------------

data X87Op
   = FADD
   | FMUL
   | FCOM
   | FCOMP
   | FSUB
   | FSUBR
   | FDIV
   | FDIVR
   | FLD
   | FXCH
   | FST
   | FNOP
   | FSTP
   | FLDENV
   | FCHS
   | FABS
   | FTST
   | FXAM
   | FLDCW
   | FLD1
   | FLDL2T
   | FLDL2E
   | FLDPI
   | FLDLG2
   | FLDLN2
   | FLDZ
   | FNSTENV
   | F2XM1
   | FYL2X
   | FPTAN
   | FPATAN
   | FXTACT
   | FPREM1
   | FDECSTP
   | FINCSTP
   | FNSTCW
   | FPREM
   | FYL2XP1
   | FSQRT
   | FSINCOS
   | FRNDINT
   | FSCALE
   | FSIN
   | FCOS
   | FIADD
   | FIMUL
   | FICOM
   | FICOMP
   | FISUB
   | FISUBR
   | FIDIV
   | FIDIVR
   | FCMOVB
   | FCMOVE
   | FCMOVBE
   | FCMOVU
   | FUCOMPP
   | FILD
   | FISTTP
   | FIST
   | FISTP
   | FCMOVNB
   | FCMOVNE
   | FCMOVNBE
   | FCMOVNU
   | FNCLEX
   | FNINIT
   | FUCOMI
   | FCOMI
   | FFREE
   | FNSAVE
   | FRSTOR
   | FUCOM
   | FUCOMP
   | FNSTSW
   | FADDP
   | FMULP
   | FCOMPP
   | FSUBRP
   | FSUBP
   | FDIVRP
   | FDIVP
   | FUCOMIP
   | FCOMIP
   | FBLD
   | FBSTP
   deriving (Show,Eq,Enum)


data X87Operand
   = ST0
   | ST !Word8
   | Mem32Real Addr
   | Mem64Real Addr
   | Mem80Real Addr
   | Mem16Int  Addr
   | Mem32Int  Addr
   | Mem64Int  Addr
   | MemEnv    Addr
   | MemState  Addr
   | Mem16     Addr
   | Mem80Dec  Addr
   deriving (Show,Eq)

readX87Operands ::
   ( ReaderM () s
   , HArrayIndexT X86Mode s
   ) => Word8 -> MState s Insn
readX87Operands x = do
   y <- binRead 

   let
      m    = ModRM (BitFields y)
      mode = modeField m
      sti  = ST (rmField m)
      reg  = regField m
      ext  = y .&. 0x3F
      mkList x = [x]
      getMem32Real = (mkList . Mem32Real) <$> getAddr m
      getMem64Real = (mkList . Mem64Real) <$> getAddr m
      getMem16Int  = (mkList . Mem16Int ) <$> getAddr m
      getMem32Int  = (mkList . Mem32Int ) <$> getAddr m
      getMem64Int  = (mkList . Mem64Int ) <$> getAddr m
      getMem80Real = (mkList . Mem80Real) <$> getAddr m
      getMemEnv    = (mkList . MemEnv   ) <$> getAddr m
      getMemState  = (mkList . MemState ) <$> getAddr m
      getMem16     = (mkList . Mem16    ) <$> getAddr m
      getMem80Dec  = (mkList . Mem80Dec ) <$> getAddr m


   case x of
      0xD8 -> InsnX87 op <$> ops
         where
            vs  = V.fromList [FADD,FMUL,FCOM,FCOMP,FSUB,FSUBR,FDIV,FDIVR]
            op  = vs V.! fromIntegral reg
            ops = if mode == Mode11
               then return [ST0, sti]
               else getMem32Real
         
      0xD9 -> case (reg,mode) of
         (0,Mode11) -> return (InsnX87 FLD [ST0, sti])
         (0,_)      -> InsnX87 FLD <$> getMem32Real
         (1,Mode11) -> return (InsnX87 FXCH [ST0, sti])
         (1,_)      -> return InsnInvalid
         (2,Mode11) -> return $ if ext == 0xD0
                           then InsnX87 FNOP []
                           else InsnInvalid
         (2,_)      -> InsnX87 FST <$> getMem32Real
         (3,Mode11) -> return InsnReserved
         (3,_)      -> InsnX87 FSTP <$> getMem32Real
         (_,Mode11) -> return $ case ext of
                           0xE0 -> InsnX87 FCHS    []
                           0xE1 -> InsnX87 FABS    []
                           0xE4 -> InsnX87 FTST    []
                           0xE5 -> InsnX87 FXAM    []
                           0xE8 -> InsnX87 FLD1    []
                           0xE9 -> InsnX87 FLDL2T  []
                           0xEA -> InsnX87 FLDL2E  []
                           0xEB -> InsnX87 FLDPI   []
                           0xEC -> InsnX87 FLDLG2  []
                           0xED -> InsnX87 FLDLN2  []
                           0xEE -> InsnX87 FLDZ    []
                           0xF0 -> InsnX87 F2XM1   []
                           0xF1 -> InsnX87 FYL2X   []
                           0xF2 -> InsnX87 FPTAN   []
                           0xF3 -> InsnX87 FPATAN  []
                           0xF4 -> InsnX87 FXTACT  []
                           0xF5 -> InsnX87 FPREM1  []
                           0xF6 -> InsnX87 FDECSTP []
                           0xF7 -> InsnX87 FINCSTP []
                           0xF8 -> InsnX87 FPREM   []
                           0xF9 -> InsnX87 FYL2XP1 []
                           0xFA -> InsnX87 FSQRT   []
                           0xFB -> InsnX87 FSINCOS []
                           0xFC -> InsnX87 FRNDINT []
                           0xFD -> InsnX87 FSCALE  []
                           0xFE -> InsnX87 FSIN    []
                           0xFF -> InsnX87 FCOS    []
                           _    -> InsnInvalid
         (4,_)      -> InsnX87 FLDENV  <$> getMemEnv
         (5,_)      -> InsnX87 FLDCW   <$> getMem16
         (6,_)      -> InsnX87 FNSTENV <$> getMemEnv
         (7,_)      -> InsnX87 FNSTCW  <$> getMem16
         _          -> error "Invalid X87 opcode"

      0xDA -> case (reg,mode) of
         (0,Mode11) -> return (InsnX87 FCMOVB  [ST0, sti])
         (1,Mode11) -> return (InsnX87 FCMOVE  [ST0, sti])
         (2,Mode11) -> return (InsnX87 FCMOVBE [ST0, sti])
         (3,Mode11) -> return (InsnX87 FCMOVU  [ST0, sti])
         (0,_) -> InsnX87 op <$> getMem32Int
            where
               vs  = V.fromList [FIADD,FIMUL,FICOM,FICOMP,FISUB,FISUBR,FIDIV,FIDIVR]
               op  = vs V.! fromIntegral reg
         (5,Mode11) | ext == 0xE9 -> return (InsnX87 FUCOMPP [])
         _  -> return InsnInvalid

      0xDB -> case (reg,mode) of
         (0,Mode11) -> return (InsnX87 FCMOVNB  [ST0, sti])
         (1,Mode11) -> return (InsnX87 FCMOVNE  [ST0, sti])
         (2,Mode11) -> return (InsnX87 FCMOVNBE [ST0, sti])
         (3,Mode11) -> return (InsnX87 FCMOVNU  [ST0, sti])
         (5,Mode11) -> return (InsnX87 FUCOMI   [ST0, sti])
         (4,Mode11) -> case ext of
                           0xE2 -> return (InsnX87 FNCLEX [])
                           0xE3 -> return (InsnX87 FNINIT [])
                           _ | ext < 0xE5 -> return InsnReserved
                           _ -> return InsnInvalid
         (6,Mode11) -> return (InsnX87 FCOMI    [ST0, sti])
         (7,Mode11) -> return InsnInvalid
         (0,_)      -> InsnX87 FILD   <$> getMem32Int
         (1,_)      -> InsnX87 FISTTP <$> getMem32Int
         (2,_)      -> InsnX87 FIST   <$> getMem32Int
         (3,_)      -> InsnX87 FISTP  <$> getMem32Int
         (4,_)      -> return InsnInvalid
         (5,_)      -> InsnX87 FLD    <$> getMem80Real
         (6,_)      -> return InsnInvalid
         (7,_)      -> InsnX87 FSTP   <$> getMem80Real
         _          -> error "Invalid X87 opcode"

      0xDC -> case (reg,mode) of
         (2,Mode11) -> return InsnReserved
         (3,Mode11) -> return InsnReserved
         _ -> InsnX87 op <$> ops
            where
               vs  = V.fromList [FADD,FMUL,FCOM,FCOMP,FSUB,FSUBR,FDIV,FDIVR]
               op  = vs V.! fromIntegral reg
               ops = if mode == Mode11
                  then return [sti,ST0]
                  else getMem64Real

      0xDD -> case (reg,mode) of
         (0,Mode11) -> return (InsnX87 FFREE  [sti])
         (1,Mode11) -> return InsnReserved
         (2,Mode11) -> return (InsnX87 FST    [sti])
         (3,Mode11) -> return (InsnX87 FSTP   [sti])
         (4,Mode11) -> return (InsnX87 FUCOM  [sti,ST0])
         (5,Mode11) -> return (InsnX87 FUCOMP [sti])
         (_,Mode11) -> return InsnInvalid
         (0,_)      -> InsnX87 FLD    <$> getMem64Real
         (1,_)      -> InsnX87 FISTTP <$> getMem64Int
         (2,_)      -> InsnX87 FST    <$> getMem64Real
         (3,_)      -> InsnX87 FSTP   <$> getMem64Real
         (4,_)      -> InsnX87 FRSTOR <$> getMemState
         (5,_)      -> return InsnInvalid
         (6,_)      -> InsnX87 FNSAVE <$> getMemState
         (7,_)      -> InsnX87 FNSTSW <$> getMem16
         _          -> error "Invalid X87 opcode"

      0xDE -> case (reg,mode) of
         (0,Mode11) -> return (InsnX87 FADDP [sti,ST0])
         (1,Mode11) -> return (InsnX87 FMULP [sti,ST0])
         (2,Mode11) -> return InsnReserved
         (3,Mode11) -> if ext == 0xD9
                        then return (InsnX87 FCOMPP [])
                        else return InsnInvalid
         (4,Mode11) -> return (InsnX87 FSUBRP [sti,ST0])
         (5,Mode11) -> return (InsnX87 FSUBP  [sti,ST0])
         (6,Mode11) -> return (InsnX87 FDIVRP [sti,ST0])
         (7,Mode11) -> return (InsnX87 FDIVP  [sti,ST0])
         _ -> InsnX87 op <$> getMem16Int
            where
               vs  = V.fromList [FIADD,FIMUL,FICOM,FICOMP,FISUB,FISUBR,FIDIV,FIDIVR]
               op  = vs V.! fromIntegral reg

      0xDF -> case (reg,mode) of
         (4,Mode11) -> if ext == 0xE0
                        then return (InsnX87 FNSTSW [])
                        else return InsnInvalid
         (5,Mode11) -> return (InsnX87 FUCOMIP [ST0,sti])
         (6,Mode11) -> return (InsnX87 FCOMIP  [ST0,sti])
         (7,Mode11) -> return InsnInvalid
         (_,Mode11) -> return InsnReserved
         (0,_)      -> InsnX87 FILD   <$> getMem16Int
         (1,_)      -> InsnX87 FISTTP <$> getMem16Int
         (2,_)      -> InsnX87 FIST   <$> getMem16Int
         (3,_)      -> InsnX87 FISTP  <$> getMem16Int
         (4,_)      -> InsnX87 FBLD   <$> getMem80Dec
         (5,_)      -> InsnX87 FILD   <$> getMem64Int
         (6,_)      -> InsnX87 FBSTP  <$> getMem80Dec
         (7,_)      -> InsnX87 FISTP  <$> getMem64Int
         _          -> error "Invalid X87 opcode"
         
      _ -> error "Invalid X87 opcode"
         
