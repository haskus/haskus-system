-- | X86 Archtiectures and micro-architectures
module Haskus.Arch.X86_64.ISA.MicroArch
   ( X86Arch(..)
   )
where

-- | X86 micro-architecture
data X86Arch
   = Intel486
   | IntelPentium
   | IntelP6
   deriving (Show,Eq)

