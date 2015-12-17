module ViperVM.Arch.X86_64.Assembler.Encoder
   ( encode
   , EncodeError (..)
   , EncodeState (..)
   )
where

import ViperVM.Arch.X86_64.Assembler.Insn
import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.Encoding
import ViperVM.Format.Binary.Put (PutM)

import Control.Monad.State
import Control.Monad.Trans.Either

type X86EncStateT = StateT EncodeState PutM
type X86Enc a     = EitherT EncodeError X86EncStateT a

data EncodeError
   = EncodeError

data EncodeState = EncodeState
   { encStateMode     :: X86Mode
   }

encode :: Instruction -> X86Enc ()
encode (InsnX87 insn) = undefined --TODO

encode (InsnX86 insn enc opSize variant ops) = do

   -- legacy prefixes
   case enc of
      LegacyEncoding e -> do
         -- determine prefixes
         -- TODO

         -- check prefix number and groups (optional)
         -- TODO

         -- put prefixes
         -- TODO
         undefined
      _ -> return ()
   
   -- TODO

   -- opcode map
   -- TODO

   -- opcode
   -- TODO

   -- operands
   -- TODO

   -- 3DNow opcode
   -- TODO
   undefined
