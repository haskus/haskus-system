{-# LANGUAGE LambdaCase #-}
module ViperVM.Arch.X86_64.Assembler.Encoder
   ( encode
   , EncodeError (..)
   , EncodeState (..)
   )
where

import ViperVM.Arch.X86_64.Assembler.Insn
import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.Encoding
import ViperVM.Arch.X86_64.Assembler.Operand
import ViperVM.Arch.X86_64.Assembler.Registers
import ViperVM.Arch.X86_64.Assembler.ModRM
import ViperVM.Format.Binary.Put
import ViperVM.Format.Binary.BitField

import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Trans.Either
import Data.Maybe (maybeToList)
import Data.Foldable (traverse_)
import Data.Word
import Data.Bits

type X86EncStateT = StateT EncodeState PutM
type X86Enc a     = EitherT EncodeError X86EncStateT a

data EncodeError
   = EncodeErrorInvalidOperandCount
   | EncodeErrorREXCannotEncodeRegister

data EncodeState = EncodeState
   { encStateMode          :: X86Mode
   , encStateRequireREX    :: Maybe Bool
   , encStateBaseRegExt    :: Maybe Word8    -- ^ REX.B
   , encStateIndexRegExt   :: Maybe Word8    -- ^ REX.X
   , encStateRegExt        :: Maybe Word8    -- ^ REX.R
   }

emitWord8 :: Word8 -> X86Enc ()
emitWord8 x = lift (lift (putWord8 x))

emitWord16 :: Word16 -> X86Enc ()
emitWord16 x = lift (lift (putWord16le x))

emitWord32 :: Word32 -> X86Enc ()
emitWord32 x = lift (lift (putWord32le x))

emitWord64 :: Word64 -> X86Enc ()
emitWord64 x = lift (lift (putWord64le x))

-- | Encode an instruction
encode :: Instruction -> X86Enc ()
encode (InsnX87 _) = undefined --TODO

encode (InsnX86 _ enc opSize variant ops) = do

   -- check that the instruction can be encoded in the current mode
   -- and with the enabled instruction sets
   -- TODO

   -- check the number of operands
   when (length (encOperands enc) /= length ops) $
      left EncodeErrorInvalidOperandCount

   let
      -- Indicate if REX prefix is required or not
      setRequireREX :: Bool -> X86Enc ()
      setRequireREX b = gets encStateRequireREX >>= \case
         Nothing        -> modify (\s -> s { encStateRequireREX = Just b })
         Just b2 
            | b2 /= b   -> left EncodeErrorREXCannotEncodeRegister
            | otherwise -> return ()

   forM_ (encOperands enc `zip` ops) $ \(e,op) -> do

      -- check if we need REX prefix to access extended registers
      case op of
         OpReg r | (opEnc e == E_ModReg || opEnc e == E_ModRM) -> do
            let code = regToCode r
            when (code > 7) $ do
               setRequireREX True
               when (opEnc e == E_ModReg) $ modify (\s -> s { encStateRegExt = Just 1 })
               when (opEnc e == E_ModRM) $ modify (\s -> s { encStateRegExt = Just 1 })
            when (not (regSupportRex r)) $ setRequireREX False

         --TODO
         OpMem addr -> undefined
               
             
      

   case enc of
      LegacyEncoding _ -> do
         -- legacy prefixes
         let 
            -- lock
            vlock = if encLockable enc && Locked `elem` variant then [0xF0] else []
            -- mandatory prefix (put last)
            vmand = maybeToList (encMandatoryPrefix enc)

            ps = vlock ++ vmand

         traverse_ emitWord8 ps

         -- REX prefix
         -- TODO

         -- opcode map
         case encOpcodeMap enc of
            Map0F      -> emitWord8 0x0F
            Map0F01    -> emitWord8 0x0F >> emitWord8 0x01
            Map0F3A    -> emitWord8 0x0F >> emitWord8 0x3A
            Map0F38    -> emitWord8 0x0F >> emitWord8 0x38
            Map3DNow   -> emitWord8 0x0F >> emitWord8 0x0F
            MapPrimary -> return ()
            MapX87     -> return ()
            m          -> error $ "Cannot encode opcode map with legacy encoding: " ++ show m

      VexEncoding _ -> do
         -- VEX prefix
         -- TODO
         undefined
   
   -- opcode
   let
      -- add operand in opcode if any
      isOpReg = (==) E_OpReg . opEnc . fst 
      opExt   = case fmap snd $ filter isOpReg (encOperands enc `zip` ops) of
                  [OpReg r] -> regToCode r
                  [_]       -> error "Operand stored in opcode is not a register"
                  []        -> 0x00
                  _         -> error "Encoding with more than one E_OpReg operand encoding"
      opcode = encOpcode enc .|. opExt
      -- TODO: set sizable, signExtend and reversed bits if relevant
   emitWord8 opcode

   -- operands
   when (encRequireModRM enc) $ do
      let 
         isModRegEnc x = case opEnc (fst x) of
            E_ModReg -> True
            _        -> False
      
         reg = case encOpcodeExt enc of
               Just n  -> n
               Nothing -> case fmap snd $ filter isModRegEnc (encOperands enc `zip` ops) of
                  [OpReg r] -> regToCode r .&. 0x07
                  [op]      -> error $ "Invalid operand for ModRM.reg: "++show op
                  []        -> error "Don't know how to fill ModRM.reg"
                  _         -> error "More than one ModRM.reg operand"

         -- TODO
         md = 0
         rm = 0

      let (ModRM modrm) = newModRM md rm reg
      emitWord8 (bitFieldsBits modrm)

   -- TODO

   -- TODO: check that we don't encode invalid register when REX prefix is used
   -- TODO: return offset and type of memory operand (for linker fixup)

   -- immediate operand

   -- 3DNow opcode
   case encOpcodeMap enc of
      Map3DNow -> emitWord8 (encOpcode enc)
      _        -> return ()
