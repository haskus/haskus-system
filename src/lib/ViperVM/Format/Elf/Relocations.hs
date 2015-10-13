-- | Relocation types found in ELF binary files
module ViperVM.Format.Elf.Relocations
   ( toRelocType
   , fromRelocType
   , RelocationType (..)
   , X86_64_Relocation (..)
   )
where

import ViperVM.Format.Elf.Header
import Data.Word

toRelocType :: Arch -> Word32 -> RelocationType
toRelocType arch typ =
   case arch of
      ArchX86_64 -> RelocationX86_64 (toEnum (fromIntegral typ))
      _          -> RelocationTypeUnknown typ

fromRelocType :: RelocationType -> Word32
fromRelocType typ =
   case typ of
      RelocationX86_64 t      -> fromIntegral (fromEnum t)
      RelocationTypeUnknown t -> t

-- | Relocation type
data RelocationType
   = RelocationX86_64 X86_64_Relocation
   | RelocationTypeUnknown Word32
   deriving (Show)

-- | Relocations for x86-64 architectures
data X86_64_Relocation
   = X86_64_NONE              -- ^ No reloc
   | X86_64_64                -- ^ Direct 64 bit
   | X86_64_PC32              -- ^ PC relative 32 bit signed
   | X86_64_GOT32             -- ^ 32 bit GOT entry
   | X86_64_PLT32             -- ^ 32 bit PLT address
   | X86_64_COPY              -- ^ Copy symbol at runtime
   | X86_64_GLOB_DAT          -- ^ Create GOT entry
   | X86_64_JUMP_SLOT         -- ^ Create PLT entry
   | X86_64_RELATIVE          -- ^ Adjust by program base
   | X86_64_GOTPCREL          -- ^ 32 bit signed PC relative offset to GOT
   | X86_64_32                -- ^ Direct 32 bit zero extended
   | X86_64_32S               -- ^ Direct 32 bit sign extended
   | X86_64_16                -- ^ Direct 16 bit zero extended
   | X86_64_PC16              -- ^ 16 bit sign extended pc relative
   | X86_64_8                 -- ^ Direct 8 bit sign extended
   | X86_64_PC8               -- ^ 8 bit sign extended pc relative
   | X86_64_DTPMOD64          -- ^ ID of module containing symbol
   | X86_64_DTPOFF64          -- ^ Offset in module's TLS block
   | X86_64_TPOFF64           -- ^ Offset in initial TLS block
   | X86_64_TLSGD             -- ^ 32 bit signed PC relative offset to two GOT entries for GD symbol
   | X86_64_TLSLD             -- ^ 32 bit signed PC relative offset to two GOT entries for LD symbol
   | X86_64_DTPOFF32          -- ^ Offset in TLS block
   | X86_64_GOTTPOFF          -- ^ 32 bit signed PC relative offset to GOT entry for IE symbol
   | X86_64_TPOFF32           -- ^ Offset in initial TLS block
   | X86_64_PC64              -- ^ PC relative 64 bit
   | X86_64_GOTOFF64          -- ^ 64 bit offset to GOT
   | X86_64_GOTPC32           -- ^ 32 bit signed pc relative offset to GOT
   | X86_64_GOT64             -- ^ 64-bit GOT entry offset
   | X86_64_GOTPCREL64        -- ^ 64-bit PC relative offset to GOT entry
   | X86_64_GOTPC64           -- ^ 64-bit PC relative offset to GOT
   | X86_64_GOTPLT64          -- ^ like GOT64, says PLT entry needed
   | X86_64_PLTOFF64          -- ^ 64-bit GOT relative offset to PLT entry
   | X86_64_SIZE32            -- ^ Size of symbol plus 32-bit addend
   | X86_64_SIZE64            -- ^ Size of symbol plus 64-bit addend
   | X86_64_GOTPC32_TLSDESC   -- ^ GOT offset for TLS descriptor
   | X86_64_TLSDESC_CALL      -- ^ Marker for call through TLS descriptor
   | X86_64_TLSDESC           -- ^ TLS descriptor
   | X86_64_IRELATIVE         -- ^ Adjust indirectly by program base
   | X86_64_RELATIVE64        -- ^ 64-bit adjust by program base
   deriving (Show, Enum)
