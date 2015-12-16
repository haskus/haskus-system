module ViperVM.Arch.X86_64.Assembler.X87
   ( X87Instruction (..)
   , X87Insn(..)
   , X87Flag(..)
   , M16INT(..)
   , M32INT(..)
   , M64INT(..)
   , M32FP(..)
   , M64FP(..)
   , M80FP(..)
   , M80DEC(..)
   , M80BCD(..)
   , MSTATE(..)
   , MCW(..)
   , MSW(..)
   , MENV(..)
   , decodeX87
--   , getX87Info
   )
where

import Data.Word

import ViperVM.Arch.X86_64.Assembler.ModRM
import ViperVM.Arch.X86_64.Assembler.Insns
import ViperVM.Arch.X86_64.Assembler.Registers
import ViperVM.Arch.X86_64.Assembler.X86Dec
import ViperVM.Arch.X86_64.Assembler.Encoding
import ViperVM.Arch.X86_64.Assembler.Operand
import Control.Monad.Trans.Either

{- Note [FPU (x87)]
   ~~~~~~~~~~~~~~~~

 x87 floating-point instructions are encoded with two bytes. The first 5 bits
 are fixed: 11011 xxx xxxxxxxx

 The second byte is a kind of ModRM but which can be used differently to
 extend the first opcode byte. 
    
-}

newtype M32FP  = M32FP Addr deriving (Show,Eq)
newtype M64FP  = M64FP Addr deriving (Show,Eq)
newtype M16INT = M16INT Addr deriving (Show,Eq)
newtype M32INT = M32INT Addr deriving (Show,Eq)
newtype M64INT = M64INT Addr deriving (Show,Eq)
newtype M80DEC = M80DEC Addr deriving (Show,Eq)
newtype M80BCD = M80BCD Addr deriving (Show,Eq)
newtype M80FP  = M80FP Addr deriving (Show,Eq)
newtype MCW    = MCW Addr deriving (Show,Eq)
newtype MSW    = MSW Addr deriving (Show,Eq)
newtype MENV   = MENV Addr deriving (Show,Eq)
newtype MSTATE = MSTATE Addr deriving (Show,Eq)

-- we don't enforce the register to be of the X87 family while it must be.
-- Perhaps we could do something with type families.
type ST     = Register

-- | X87 instructions
--
-- Some instruction are polymorphic (e.g. FADD m32fp/m64fp). Here we mangle the
-- data constructor to avoid this (e.g. FADD_m32 and FADD_m64).
data X87Instruction
   = F2XM1
   | FABS
   | FADD_m32 M32FP
   | FADD_m64 M64FP
   | FADD_st0_sti ST
   | FADD_sti_st0 ST
   | FADDP_sti_st0 ST
   | FIADD_m32 M32INT
   | FIADD_m16 M16INT
   | FBLD M80DEC
   | FBSTP M80BCD
   | FCHS
   | FNCLEX
   | FCMOVB ST
   | FCMOVE ST
   | FCMOVBE ST
   | FCMOVU ST
   | FCMOVNB ST
   | FCMOVNE ST
   | FCMOVNBE ST
   | FCMOVNU ST
   | FCOM_m32 M32FP
   | FCOM_m64 M64FP
   | FCOM_st  ST
   | FCOMP_m32 M32FP
   | FCOMP_m64 M64FP
   | FCOMP_st  ST
   | FCOMPP
   | FCOMI ST
   | FCOMIP ST
   | FUCOMI ST
   | FUCOMIP ST
   | FCOS
   | FDECSTP
   | FDIV_m32 M32FP
   | FDIV_m64 M64FP
   | FDIV_st0_st0_sti ST
   | FDIV_sti_st0_sti ST
   | FDIV_st0_sti_st0 ST
   | FDIV_sti_sti_st0 ST
   | FDIVP ST
   | FIDIV_m32 M32INT
   | FIDIV_m16 M16INT
   | FDIVR_m32 M32FP
   | FDIVR_m64 M64FP
   | FDIVRP ST
   | FIDIVR_m32 M32INT
   | FIDIVR_m16 M16INT
   | FFREE ST
   | FICOM_m16 M16INT
   | FICOM_m32 M32INT
   | FICOMP_m16 M16INT
   | FICOMP_m32 M32INT
   | FILD_m16 M16INT
   | FILD_m32 M32INT
   | FILD_m64 M64INT
   | FINCSTP
   | FNINIT
   | FIST_m16 M16INT
   | FIST_m32 M32INT
   | FISTP_m16 M16INT
   | FISTP_m32 M32INT
   | FISTP_m64 M64INT
   | FISTTP_m16 M16INT
   | FISTTP_m32 M32INT
   | FISTTP_m64 M64INT
   | FLD_m32 M32FP
   | FLD_m64 M64FP
   | FLD_m80 M80FP
   | FLD_st  ST
   | FLD1
   | FLDL2T
   | FLDL2E
   | FLDPI
   | FLDLG2
   | FLDLN2
   | FLDZ
   | FLDCW MCW
   | FLDENV MENV
   | FMUL_m32 M32FP
   | FMUL_m64 M64FP
   | FMUL_st0_sti ST
   | FMUL_sti_st0 ST
   | FMULP_sti_st0 ST
   | FIMUL_m32 M32INT
   | FIMUL_m16 M16INT
   | FNOP
   | FPATAN
   | FPREM
   | FPREM1
   | FPTAN
   | FRNDINT
   | FRSTOR MSTATE
   | FNSAVE MSTATE
   | FSCALE
   | FSIN
   | FSINCOS
   | FSQRT
   | FST_m32 M32FP
   | FST_m64 M64FP
   | FST_st  ST
   | FSTP_m32 M32FP
   | FSTP_m64 M64FP
   | FSTP_m80 M80FP
   | FSTP_st  ST
   | FNSTCW MCW
   | FNSTENV MENV
   | FNSTSW MSW
   | FNSTSW_ax
   | FSUB_st0_st0_m32 M32FP
   | FSUB_st0_st0_m64 M64FP
   | FSUB_st0_m32_st0 M32FP
   | FSUB_st0_m64_st0 M64FP
   | FSUB_st0_st0_sti ST
   | FSUB_st0_sti_st0 ST
   | FSUB_sti_st0_sti ST
   | FSUB_sti_sti_st0 ST
   | FSUBP_st0_st0_sti ST
   | FSUBP_sti_sti_st0 ST
   | FISUB_m32 M32INT
   | FISUB_m16 M16INT
   | FISUBR_m32 M32INT
   | FISUBR_m16 M16INT
   | FTST
   | FUCOM ST
   | FUCOMP ST
   | FUCOMPP
   | FXAM
   | FXCH ST
   | FXTRACT
   | FYL2X
   | FYL2XP1
   deriving (Show,Eq)

decodeX87 :: Word8 -> X86Dec X87Instruction
decodeX87 x = do
   y <- lookWord8
   case (x,y) of
      -- instructions without parameters
      (0xD9, 0xD0) -> skipWord8 >> right FNOP
      (0xD9, 0xE1) -> skipWord8 >> right FABS
      (0xD9, 0xE0) -> skipWord8 >> right FCHS
      (0xD9, 0xE5) -> skipWord8 >> right FXAM
      (0xD9, 0xE8) -> skipWord8 >> right FLD1
      (0xD9, 0xE9) -> skipWord8 >> right FLDL2T
      (0xD9, 0xEA) -> skipWord8 >> right FLDL2E
      (0xD9, 0xEB) -> skipWord8 >> right FLDPI
      (0xD9, 0xEC) -> skipWord8 >> right FLDLG2
      (0xD9, 0xED) -> skipWord8 >> right FLDLN2
      (0xD9, 0xEE) -> skipWord8 >> right FLDZ
      (0xD9, 0xF0) -> skipWord8 >> right F2XM1
      (0xD9, 0xF1) -> skipWord8 >> right FYL2X
      (0xD9, 0xF2) -> skipWord8 >> right FPTAN
      (0xD9, 0xF3) -> skipWord8 >> right FPATAN
      (0xD9, 0xF4) -> skipWord8 >> right FXTRACT
      (0xD9, 0xF5) -> skipWord8 >> right FPREM1
      (0xD9, 0xF6) -> skipWord8 >> right FDECSTP
      (0xD9, 0xF7) -> skipWord8 >> right FINCSTP
      (0xD9, 0xF8) -> skipWord8 >> right FPREM
      (0xD9, 0xF9) -> skipWord8 >> right FYL2XP1
      (0xD9, 0xFA) -> skipWord8 >> right FSQRT
      (0xD9, 0xFB) -> skipWord8 >> right FSINCOS
      (0xD9, 0xFC) -> skipWord8 >> right FRNDINT
      (0xD9, 0xFD) -> skipWord8 >> right FSCALE
      (0xD9, 0xFE) -> skipWord8 >> right FSIN
      (0xD9, 0xFF) -> skipWord8 >> right FCOS
      (0xDB, 0xE2) -> skipWord8 >> right FNCLEX
      (0xDA, 0xE9) -> skipWord8 >> right FUCOMPP
      (0xDD, 0xE4) -> skipWord8 >> right FTST
      (0xDE, 0xD9) -> skipWord8 >> right FCOMPP
      (0xDF, 0xE0) -> skipWord8 >> right FNSTSW_ax
      -- instructions with ModRM
      _ -> do
         let 
            m         = ModRM y
            getReg    = getRMRegister RF_X87 Nothing m
            getM32FP  = M32FP  <$> getAddr m
            getM64FP  = M64FP  <$> getAddr m
            getM80FP  = M80FP  <$> getAddr m
            getM16INT = M16INT <$> getAddr m
            getM32INT = M32INT <$> getAddr m
            getM64INT = M64INT <$> getAddr m
            getM80DEC = M80DEC <$> getAddr m
            getM80BCD = M80BCD <$> getAddr m
            getMCW    = MCW    <$> getAddr m
            getMSW    = MSW    <$> getAddr m
            getMENV   = MENV   <$> getAddr m
            getMSTATE = MSTATE <$> getAddr m
         case (x,regField m,rmRegMode m) of
            (0xD8, 0, True ) -> skipWord8 >> FADD_st0_sti      <$> getReg
            (0xD8, 0, False) -> skipWord8 >> FADD_m32          <$> getM32FP
            (0xD9, 0, True ) -> skipWord8 >> FLD_st            <$> getReg
            (0xD9, 0, False) -> skipWord8 >> FLD_m32           <$> getM32FP
            (0xDA, 0, False) -> skipWord8 >> FIADD_m32         <$> getM32INT
            (0xDB, 0, False) -> skipWord8 >> FILD_m32          <$> getM32INT
            (0xDC, 0, True ) -> skipWord8 >> FADD_sti_st0      <$> getReg
            (0xDC, 0, False) -> skipWord8 >> FADD_m64          <$> getM64FP
            (0xDD, 0, False) -> skipWord8 >> FLD_m64           <$> getM64FP
            (0xDD, 0, True ) -> skipWord8 >> FFREE             <$> getReg
            (0xDE, 0, True ) -> skipWord8 >> FADDP_sti_st0     <$> getReg
            (0xDE, 0, False) -> skipWord8 >> FIADD_m16         <$> getM16INT
            (0xDF, 0, False) -> skipWord8 >> FILD_m16          <$> getM16INT

            (0xD8, 1, False) -> skipWord8 >> FMUL_m32          <$> getM32FP
            (0xD8, 1, True ) -> skipWord8 >> FMUL_st0_sti      <$> getReg
            (0xD9, 1, True ) -> skipWord8 >> FXCH              <$> getReg
            (0xDA, 1, False) -> skipWord8 >> FIMUL_m32         <$> getM32INT
            (0xDC, 1, True ) -> skipWord8 >> FMUL_sti_st0      <$> getReg
            (0xDC, 1, False) -> skipWord8 >> FMUL_m64          <$> getM64FP
            (0xDE, 1, True ) -> skipWord8 >> FMULP_sti_st0     <$> getReg
            (0xDE, 1, False) -> skipWord8 >> FIMUL_m16         <$> getM16INT

            (0xDF, 2, False) -> skipWord8 >> FIST_m16          <$> getM16INT
            (0xDB, 2, False) -> skipWord8 >> FIST_m32          <$> getM32INT
            (0xD8, 2, True ) -> skipWord8 >> FCOM_st           <$> getReg
            (0xD8, 2, False) -> skipWord8 >> FCOM_m32          <$> getM32FP
            (0xD9, 2, False) -> skipWord8 >> FST_m32           <$> getM32FP
            (0xDA, 2, False) -> skipWord8 >> FICOM_m32         <$> getM32INT
            (0xDC, 2, False) -> skipWord8 >> FCOM_m64          <$> getM64FP
            (0xDD, 2, False) -> skipWord8 >> FST_m64           <$> getM64FP
            (0xDD, 2, True ) -> skipWord8 >> FST_st            <$> getReg
            (0xDE, 2, False) -> skipWord8 >> FICOM_m16         <$> getM16INT

            (0xD8, 3, True ) -> skipWord8 >> FCOMP_st          <$> getReg
            (0xD8, 3, False) -> skipWord8 >> FCOMP_m32         <$> getM32FP
            (0xD9, 3, False) -> skipWord8 >> FSTP_m32          <$> getM32FP
            (0xDA, 3, False) -> skipWord8 >> FICOMP_m32        <$> getM32INT
            (0xDB, 3, False) -> skipWord8 >> FISTP_m32         <$> getM32INT
            (0xDC, 3, False) -> skipWord8 >> FCOMP_m64         <$> getM64FP
            (0xDD, 3, True ) -> skipWord8 >> FSTP_st           <$> getReg
            (0xDD, 3, False) -> skipWord8 >> FSTP_m64          <$> getM64FP
            (0xDE, 3, False) -> skipWord8 >> FICOMP_m16        <$> getM16INT
            (0xDF, 3, False) -> skipWord8 >> FISTP_m16         <$> getM16INT

            (0xD8, 4, True ) -> skipWord8 >> FSUB_st0_st0_sti  <$> getReg
            (0xD8, 4, False) -> skipWord8 >> FSUB_st0_st0_m32  <$> getM32FP
            (0xD9, 4, False) -> skipWord8 >> FLDENV            <$> getMENV
            (0xDA, 4, False) -> skipWord8 >> FISUB_m32         <$> getM32INT
            (0xDC, 4, False) -> skipWord8 >> FSUB_st0_st0_m64  <$> getM64FP
            (0xDC, 4, True ) -> skipWord8 >> FSUB_sti_st0_sti  <$> getReg
            (0xDD, 4, True ) -> skipWord8 >> FUCOM             <$> getReg
            (0xDD, 4, False) -> skipWord8 >> FRSTOR            <$> getMSTATE
            (0xDE, 4, True ) -> skipWord8 >> FSUBP_sti_sti_st0 <$> getReg
            (0xDE, 4, False) -> skipWord8 >> FISUB_m16         <$> getM16INT
            (0xDF, 4, False) -> skipWord8 >> FBLD              <$> getM80DEC

            (0xD8, 5, True ) -> skipWord8 >> FSUB_st0_sti_st0  <$> getReg
            (0xD8, 5, False) -> skipWord8 >> FSUB_st0_m32_st0  <$> getM32FP
            (0xD9, 5, False) -> skipWord8 >> FLDCW             <$> getMCW
            (0xDA, 5, False) -> skipWord8 >> FISUBR_m32        <$> getM32INT
            (0xDB, 5, False) -> skipWord8 >> FLD_m80           <$> getM80FP
            (0xDB, 5, True ) -> skipWord8 >> FUCOMI            <$> getReg
            (0xDC, 5, False) -> skipWord8 >> FSUB_st0_m64_st0  <$> getM64FP
            (0xDC, 5, True ) -> skipWord8 >> FSUB_sti_sti_st0  <$> getReg
            (0xDD, 5, True ) -> skipWord8 >> FUCOMP            <$> getReg
            (0xDE, 5, True ) -> skipWord8 >> FSUBP_st0_st0_sti <$> getReg
            (0xDE, 5, False) -> skipWord8 >> FISUBR_m16        <$> getM16INT
            (0xDF, 5, True ) -> skipWord8 >> FUCOMIP           <$> getReg
            (0xDF, 5, False) -> skipWord8 >> FILD_m64          <$> getM64INT

            (0xD8, 6, True ) -> skipWord8 >> FDIV_st0_st0_sti  <$> getReg
            (0xD8, 6, False) -> skipWord8 >> FDIV_m32          <$> getM32FP
            (0xD9, 6, False) -> skipWord8 >> FNSTENV           <$> getMENV
            (0xDA, 6, False) -> skipWord8 >> FIDIV_m32         <$> getM32INT
            (0xDC, 6, True ) -> skipWord8 >> FDIV_sti_st0_sti  <$> getReg
            (0xDC, 6, False) -> skipWord8 >> FDIV_m64          <$> getM64FP
            (0xDD, 6, False) -> skipWord8 >> FNSAVE            <$> getMSTATE
            (0xDE, 6, True ) -> skipWord8 >> FDIVRP            <$> getReg
            (0xDE, 6, False) -> skipWord8 >> FIDIV_m16         <$> getM16INT
            (0xDF, 6, False) -> skipWord8 >> FBSTP             <$> getM80BCD
            (0xDF, 6, True ) -> skipWord8 >> FCOMIP            <$> getReg

            (0xD8, 7, True ) -> skipWord8 >> FDIV_st0_sti_st0  <$> getReg
            (0xD8, 7, False) -> skipWord8 >> FDIVR_m32         <$> getM32FP
            (0xD9, 7, False) -> skipWord8 >> FNSTCW            <$> getMCW
            (0xDA, 7, False) -> skipWord8 >> FIDIVR_m32        <$> getM32INT
            (0xDB, 7, False) -> skipWord8 >> FSTP_m80          <$> getM80FP
            (0xDC, 7, True ) -> skipWord8 >> FDIV_sti_sti_st0  <$> getReg
            (0xDC, 7, False) -> skipWord8 >> FDIVR_m64         <$> getM64FP
            (0xDD, 7, False) -> skipWord8 >> FNSTSW            <$> getMSW
            (0xDE, 7, True ) -> skipWord8 >> FDIVP             <$> getReg
            (0xDE, 7, False) -> skipWord8 >> FIDIVR_m16        <$> getM16INT
            (0xDF, 7, False) -> skipWord8 >> FISTP_m64         <$> getM64INT

            _                -> left $ ErrUnknownOpcode MapX87 x

-- getX87Info :: X87Instruction -> X87Insn
-- getX87Info x = case x of
--    F2XM1                   -> x87_f2xm1
--    FABS                    -> x87_fabs
--    FADD_m32 _              -> x87_fadd
--    FADD_m64 _              -> x87_fadd
--    FADD_st0_sti _          -> x87_fadd
--    FADD_sti_st0 _          -> x87_fadd
--    FADDP_sti_sto _         -> x87_faddp
--    FADDP                   -> x87_faddp
--    FIADD_m32 _             -> x87_fiadd
--    FIADD_m16 _             -> x87_fiadd
--    FBLD _                  -> x87_fbld
--    FBSTP _                 -> x87_fbstp
--    FCHS                    -> x87_fchs
--    FNCLEX                  -> x87_fnclex
--    FCMOVB _                -> x87_fcmovb
--    FCMOVE _                -> x87_fcmove
--    FCMOVBE _               -> x87_fcmovbe
--    FCMOVU _                -> x87_fcmovu
--    FCMOVNB _               -> x87_fcmovnb
--    FCMOVNE _               -> x87_fcmovne
--    FCMOVNBE _              -> x87_fcmovnbe
--    FCMOVNU _               -> x87_fcmovnu
--    FCOM_m32 _              -> x87_fcom
--    FCOM_m64 _              -> x87_fcom
--    FCOM_st _               -> x87_fcom
--    FCOMP_m32 _             -> x87_fcomp
--    FCOMP_m64 _             -> x87_fcomp
--    FCOMP_st _              -> x87_fcomp
--    FCOMPP                  -> x87_fcompp
--    FCOMI _                 -> x87_fcomi
--    FCOMIP _                -> x87_fcomip
--    FUCOMI _                -> x87_fucomi
--    FUCOMIP _               -> x87_fucomip
--    FCOS                    -> x87_fcos
--    FDECSTP                 -> x87_fdecstp
--    FDIV_m32 _              -> x87_fdiv
--    FDIV_m64 _              -> x87_fdiv
--    FDIV_st0_sti _          -> x87_fdiv
--    FDIV_sti_st0 _          -> x87_fdiv
--    FDIVP _                 -> x87_fdivp
--    FIDIV_m32 _             -> x87_fidiv
--    FIDIV_m16 _             -> x87_fidiv
--    FDIVR_m32 _             -> x87_fdivr
--    FDIVR_m64 _             -> x87_fdivr
--    FDIVR_st0_sti _         -> x87_fdivr
--    FDIVR_sti_st0 _         -> x87_fdivr
--    FDIVRP _                -> x87_fdivrp
--    FIDIVR_m32 _            -> x87_fidivr
--    FIDIVR_m16 _            -> x87_fidivr
--    FFREE _                 -> x87_ffree
--    FICOM_m16 _             -> x87_ficom
--    FICOM_m32 _             -> x87_ficom
--    FICOMP_m16 _            -> x87_ficomp
--    FICOMP_m32 _            -> x87_ficomp
--    FILD_m16 _              -> x87_fild
--    FILD_m32 _              -> x87_fild
--    FILD_m64 _              -> x87_fild
--    FINCSTP                 -> x87_fincstp
--    FNINIT                  -> x87_fninit
--    FIST_m16 _              -> x87_fist
--    FIST_m32 _              -> x87_fist
--    FISTP_m16 _             -> x87_fistp
--    FISTP_m32 _             -> x87_fistp
--    FISTP_m64 _             -> x87_fistp
--    FISTTP_m16 _            -> x87_fistpp
--    FISTTP_m32 _            -> x87_fistpp
--    FISTTP_m64 _            -> x87_fistpp
--    FLD_m32 _               -> x87_fld
--    FLD_m64 _               -> x87_fld
--    FLD_m80 _               -> x87_fld
--    FLD_st _                -> x87_fld
--    FLD1                    -> x87_fld1
--    FLDL2T                  -> x87_fldl2t
--    FLDL2E                  -> x87_fldl2e
--    FLDPI                   -> x87_fldpi
--    FLDLG2                  -> x87_fldlg2
--    FLDLN2                  -> x87_fldln2
--    FLDZ                    -> x87_fldz
--    FLDCW _                 -> x87_fldcw
--    FLDENV _                -> x87_fldenv
--    FMUL_m32 _              -> x87_fmul
--    FMUL_m64 _              -> x87_fmul
--    FMUL_st0_sti _          -> x87_fmul
--    FMUL_sti_st0 _          -> x87_fmul
--    FMULP_sti_st0 _         -> x87_fmulp
--    FIMUL_m32 _             -> x87_fimul
--    FIMUL_m16 _             -> x87_fimul
--    FNOP                    -> x87_fnop
--    FPATAN                  -> x87_fpatan
--    FPREM                   -> x87_fprem
--    FPREM1                  -> x87_fprem1
--    FPTAN                   -> x87_fptan
--    FRNDINT                 -> x87_frndint
--    FRSTOR _                -> x87_frstor
--    FNSAVE _                -> x87_fnsave
--    FSCALE                  -> x87_fscale
--    FSIN                    -> x87_fsin
--    FSINCOS                 -> x87_fsincos
--    FSQRT                   -> x87_fsqrt
--    FST_m32 _               -> x87_fst
--    FST_m64 _               -> x87_fst
--    FST_st _                -> x87_fst
--    FSTP_m32 _              -> x87_fstp
--    FSTP_m64 _              -> x87_fstp
--    FSTP_m80 _              -> x87_fstp
--    FSTP_st _               -> x87_fstp
--    FNSTCW _                -> x87_fnstcw
--    FNSTENV _               -> x87_fnstenv
--    FNSTSW _                -> x87_fnstsw
--    FNSTSW_ax               -> x87_fnstsw
--    FSUB_m32 _              -> x87_fsub
--    FSUB_m64 _              -> x87_fsub
--    FSUB_st0_sti _          -> x87_fsub
--    FSUB_sti_st0 _          -> x87_fsub
--    FSUBP_st0_sti _         -> x87_fsubp
--    FISUB_m32 _             -> x87_fisub
--    FISUB_m64 _             -> x87_fisub
--    FSUBR_m32 _             -> x87_fsubr
--    FSUBR_m64 _             -> x87_fsubr
--    FSUBRP_sti_st0 _        -> x87_fsubrp
--    FISUBR_m32 _            -> x87_fisubr
--    FISUBR_m64 _            -> x87_fisubr
--    FTST                    -> x87_ftst
--    FUCOM _                 -> x87_fucom
--    FUCOMP _                -> x87_fucomp
--    FUCOMPP                 -> x87_fucompp
--    FXAM                    -> x87_fxam
--    FXCH _                  -> x87_fxch
--    FXTRACT                 -> x87_fxtract
--    FYL2X                   -> x87_fyl2x
--    FYL2XP1                 -> x87_fyl2xp1

data X87Insn = X87Insn
   { x87Desc        :: String
   , x87Mnemonic    :: String
   , x87Properties  :: [Properties]
   , x87FPUFlags    :: [FlagOp X87Flag]
   , x87Flags       :: [FlagOp Flag]
   , x87Encoding    :: [Encoding]
   } deriving (Show)

data X87Flag
   = C0 | C1 | C2 | C3
   deriving (Show,Eq)


--op :: AccessMode -> OperandType -> OperandEnc -> Operand
--op = Operand


--x87 :: String -> String -> [Properties] -> [FlagOp X87Flag] -> [FlagOp Flag] -> [Encoding] -> X87Insn
--x87 = X87Insn
--
--x87_f2xm1 :: X87Insn
--x87_f2xm1 = x87
--   "FPU compute 2^x - 1" "F2XM1"
--   [Legacy, LongMode, Extension FPU]
--   [ Set [C1], Undef [C0,C2,C3]]
--   []
--   [LegacyEncoding [ op    RW    T_ST0    E_Implicit ]]
--
--x87_fabs :: X87Insn
--x87_fabs = x87
--   "FPU absolute value" "FABS"
--   [Legacy, LongMode, Extension FPU]
--   [ Zero [C1], Undef [C0,C2,C3]]
--   []
--   [LegacyEncoding [ op    RW    T_ST0    E_Implicit ]]
--
--x87_fadd :: X87Insn
--x87_fadd = x87
--   "FPU add" "FADD"
--   [Legacy, LongMode, Reversable 2, FPUPop 1, FPUMemFormat, OpExt 0x0, Extension FPU]
--   [ Set [C1], Undef [C0,C2,C3]]
--   []
--   [LegacyEncoding [ op    RW    T_ST0    E_Implicit 
--                   , op    RO    T_STMem  E_ModRM
--                   ]]
--
--x87_fbld :: X87Insn
--x87_fbld = x87
--   "FPU load binary coded decimal" "FBLD"
--   [Legacy, LongMode, Extension FPU, OpExt 0x4]
--   [ Set [C1], Undef [C0,C2,C3]]
--   []
--   [LegacyEncoding [ op    RO    T_M80dec E_ModRM ]]
--
--x87_fbstp :: X87Insn
--x87_fbstp = x87
--   "Store BCD integer and pop" "FBSTP"
--   [Legacy, LongMode, Extension FPU, OpExt 0x6]
--   [ Set [C1], Undef [C0,C2,C3]]
--   []
--   [LegacyEncoding [ op    RO    T_ST0    E_Implicit
--                   , op    WO    T_M80dec E_ModRM
--                   ]]
--
--x87_fchs :: X87Insn
--x87_fchs = x87
--   "FPU change sign" "FCHS"
--   [Legacy, LongMode, Extension FPU]
--   [ Zero [C1], Undef [C0,C2,C3]]
--   []
--   [LegacyEncoding [ op    RW    T_ST0    E_Implicit ]]
--
--x87_fnclex :: X87Insn
--x87_fnclex = x87
--   "FPU clear exceptions" "FNCLEX"
--   [Legacy, LongMode, Extension FPU]
--   [ Undef [C0,C1,C2,C3]]
--   []
--   [LegacyEncoding []]
--
--x87_fcmovb :: X87Insn
--x87_fcmovb = x87
--   "FPU conditional move if below" "FCMOVB"
--   [Legacy, LongMode, Extension FPU, Arch IntelP6]
--   [ Set [C1], Undef [C0,C2,C3]]
--   [Read [CF]]
--   [LegacyEncoding [ op    WO    T_ST0    E_Implicit 
--                   , op    RO    T_ST     E_OpReg
--                   ]]
--
--x87_fcmove :: X87Insn
--x87_fcmove = x87
--   "FPU conditional move if equal" "FCMOVE"
--   [Legacy, LongMode, Extension FPU, Arch IntelP6]
--   [ Set [C1], Undef [C0,C2,C3]]
--   [ Read [ZF]]
--   [LegacyEncoding [ op    WO    T_ST0    E_Implicit 
--                   , op    RO    T_ST     E_OpReg
--                   ]]
--
--x87_fcmovbe :: X87Insn
--x87_fcmovbe = x87
--   "FPU conditional move if below or equal" "FCMOVBE"
--   [Legacy, LongMode, Extension FPU, Arch IntelP6]
--   [ Set [C1], Undef [C0,C2,C3]]
--   [ Read [CF,ZF]]
--   [LegacyEncoding [ op    WO    T_ST0    E_Implicit 
--                   , op    RO    T_ST     E_OpReg
--                   ]]
--
--x87_fcmovu :: X87Insn
--x87_fcmovu = x87
--   "FPU conditional move if unordered" "FCMOVU"
--   [Legacy, LongMode, Extension FPU, Arch IntelP6]
--   [ Set [C1], Undef [C0,C2,C3]]
--   [ Read [PF]]
--   [LegacyEncoding [ op    WO    T_ST0    E_Implicit 
--                   , op    RO    T_ST     E_OpReg
--                   ]]
--
--x87_fcmovnb :: X87Insn
--x87_fcmovnb = x87
--   "FPU conditional move if not below" "FCMOVNB"
--   [Legacy, LongMode, Extension FPU, Arch IntelP6]
--   [ Set [C1], Undef [C0,C2,C3]]
--   [ Read [CF]]
--   [LegacyEncoding [ op    WO    T_ST0    E_Implicit 
--                   , op    RO    T_ST     E_OpReg
--                   ]]
--
--x87_fcmovne :: X87Insn
--x87_fcmovne = x87
--   "FPU conditional move if not equal" "FCMOVNE"
--   [Legacy, LongMode, Extension FPU, Arch IntelP6]
--   [ Set [C1], Undef [C0,C2,C3]]
--   [ Read [ZF]]
--   [LegacyEncoding [ op    WO    T_ST0    E_Implicit 
--                   , op    RO    T_ST     E_OpReg
--                   ]]
--
--x87_fcmovnbe :: X87Insn
--x87_fcmovnbe = x87
--   "FPU conditional move if not below or equal" "FCMOVNBE"
--   [Legacy, LongMode, Extension FPU, Arch IntelP6]
--   [ Set [C1], Undef [C0,C2,C3]]
--   [ Read [CF,ZF]]
--   [LegacyEncoding [ op    WO    T_ST0    E_Implicit 
--                   , op    RO    T_ST     E_OpReg
--                   ]]
--
--x87_fcmovnu :: X87Insn
--x87_fcmovnu = x87
--   "FPU conditional move if not unordered" "FCMOVNU"
--   [Legacy, LongMode, Extension FPU, Arch IntelP6]
--   [ Set [C1], Undef [C0,C2,C3]]
--   [ Read [PF]]
--   [LegacyEncoding [ op    WO    T_ST0    E_Implicit 
--                   , op    RO    T_ST     E_OpReg
--                   ]]
--
--x87_fcom :: X87Insn
--x87_fcom = x87
--   "FPU compare" "FCOM"
--   [Legacy, LongMode, FPUPop 8, FPUMemFormat, OpExt 0x2, Extension FPU]
--   [ Set [C1], Undef [C0,C2,C3]]
--   []
--   [LegacyEncoding [ op    RO    T_ST0    E_Implicit 
--                   , op    RO    T_STMem  E_ModRM
--                   ]]
