module ViperVM.Arch.X86_64.Assembler.Encoder
   ( encode
   , EncodeError (..)
   , EncodeState (..)
   )
where

import ViperVM.Arch.X86_64.Assembler.Insn
import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.Encoding
import ViperVM.Format.Binary.Put

import Control.Monad.State
import Control.Monad.Trans.Either
import Data.Maybe (maybeToList)
import Data.Foldable (traverse_)
import Data.Word
import Data.Bits

type X86EncStateT = StateT EncodeState PutM
type X86Enc a     = EitherT EncodeError X86EncStateT a

data EncodeError
   = EncodeError

data EncodeState = EncodeState
   { encStateMode     :: X86Mode
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

   case enc of
      LegacyEncoding _ -> do
         -- legacy prefixes
         let 
            vlock = if encLockable enc && Locked `elem` variant then [0xF0] else []
            vmand = maybeToList (encMandatoryPrefix enc)

            ps = vlock ++ vmand

         traverse_ emitWord8 ps

         -- REX prefix
         -- TODO

         -- opcode map
         -- TODO

      VexEncoding _ -> do
         -- VEX prefix
         -- TODO
         undefined
   
   -- opcode
   let
      opExt  = case filter ((==) E_OpReg . opEnc . fst) (encOperands enc `zip` ops) of
                  []      -> 0x00
                  [(e,x)] -> undefined -- TODO: find reg code
                  _       -> error "Encoding with more than one E_OpReg operand encoding"
      opcode = encOpcode enc .|. opExt
      -- TODO: set sizable, signExtend and reversed bits if relevant
   emitWord8 opcode

   -- operands
   -- TODO

   -- TODO: return offset and type of memory operand (for linker fixup)

   -- 3DNow opcode
   case encOpcodeMap enc of
      Map3DNow -> emitWord8 (encOpcode enc)
      _        -> return ()
