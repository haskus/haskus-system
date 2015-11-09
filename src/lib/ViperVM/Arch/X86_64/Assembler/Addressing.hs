module ViperVM.Arch.X86_64.Assembler.Addressing
   ( Addr(..)
   , AddrParams(..)
   , getAddr
   )
where

import Data.Bits
import Data.Word

import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.ModRM
import ViperVM.Arch.X86_64.Assembler.Size
import ViperVM.Arch.X86_64.Assembler.Registers
import ViperVM.Arch.X86_64.Assembler.X86Dec

-- The X86 architecture supports different kinds of memory addressing. The
-- available addressing modes depend on the execution mode.
-- The most complicated addressing has:
--    - a base register
--    - an index register with a scaling factor (1, 2, 4 or 8)
--    - an offset (displacement)
--
-- Base and index registers can be extended in 64-bit mode to access new registers.
-- Offset size depends on the address size and on the execution mode.

data Addr = Addr
   { addrBase  :: Maybe Register
   , addrIndex :: Maybe Register
   , addrDisp  :: Maybe SizedValue
   , addrScale :: Maybe Scale
   }
   deriving (Show,Eq)

-- | Parameters for an addressing
data AddrParams = AddrParams
   { addrParamBaseExt         :: Word8       -- ^ Extension for the base register
   , addrParamIndexExt        :: Word8       -- ^ Extension for the index register
   , addrParamUseExtRegisters :: Bool        -- ^ Indicate if extended regsiters have to be used
   , addrParamAddressSize     :: AddressSize -- ^ Address size
   }
   deriving (Show,Eq)

-- | Read the memory addressing in r/m field
getAddr :: AddrParams -> ModRM -> X86Dec Addr
getAddr (AddrParams baseExt indexExt useExtendedRegisters asize) modrm = do
   mode <- getMode

   -- depending on the r/m field in ModRM and on the address size, we know if
   -- we must read a SIB byte
   sib   <- case useSIB asize modrm of
      True  -> Just . SIB <$> nextWord8
      False -> return Nothing

   -- depending on the mod field and the r/m in ModRM and on the address size,
   -- we know if we must read a displacement and its size
   disp <- case useDisplacement asize modrm of
      Nothing     -> return Nothing
      Just Size8  -> Just . SizedValue8  <$> nextWord8
      Just Size16 -> Just . SizedValue16 <$> nextWord16
      Just Size32 -> Just . SizedValue32 <$> nextWord32
      Just _      -> error "Invalid displacement size"

   return $ case asize of
      -- if we are in 16-bit addressing mode, we don't care about the base
      -- register extension
      AddrSize16 -> case (modField modrm, rmField modrm) of
         (_,0) -> Addr (Just R_BX) (Just R_SI) disp Nothing
         (_,1) -> Addr (Just R_BX) (Just R_DI) disp Nothing
         (_,2) -> Addr (Just R_BP) (Just R_SI) disp Nothing
         (_,3) -> Addr (Just R_BP) (Just R_DI) disp Nothing
         (_,4) -> Addr (Just R_SI) Nothing     disp Nothing
         (_,5) -> Addr (Just R_DI) Nothing     disp Nothing
         (0,6) -> Addr Nothing     Nothing     disp Nothing
         (_,6) -> Addr (Just R_BP) Nothing     disp Nothing
         (_,7) -> Addr (Just R_BX) Nothing     disp Nothing
         _     -> error "Invalid 16-bit addressing"

      
      -- 32-bit and 64-bit addressing
      _ -> Addr baseReg indexReg disp scale
         where
            -- size of the operand
            sz   = case asize of
               AddrSize16 -> error "Invalid address size"
               AddrSize32 -> Size32
               AddrSize64 -> Size64

            -- associate base/index register
            makeReg =  regFromCode RF_GPR (Just sz) useExtendedRegisters

            -- the extended base register is either in SIB or in r/m. In some
            -- cases, there is no base register or it is implicitly RIP/EIP
            baseReg = case (modField modrm, rmField modrm) of
               (0,5)
                  | isLongMode mode -> case asize of
                     AddrSize32 -> Just R_EIP
                     AddrSize64 -> Just R_RIP
                     AddrSize16 -> error "Invalid address size"
                  | otherwise -> Nothing
               _ -> Just . makeReg $ (baseExt `shiftL` 3) .|. br
                  where
                     br = case sib of
                        Nothing -> rmField modrm
                        Just s  -> baseField s

            -- if there is an index field, it is in sib
            indexReg = makeReg . ((indexExt `shiftL` 3) .|.) . indexField <$> sib

            -- if there is a scale, it is in sib
            scale = scaleField <$> sib
