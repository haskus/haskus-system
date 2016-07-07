-- | Relocation types found in ELF binary files
module ViperVM.Format.Elf.RelocationType
   ( toRelocType
   , fromRelocType
   , RelocationType (..)
   , M68K_Relocation (..)
   , I386_Relocation (..)
   , X86_64_Relocation (..)
   )
where

import ViperVM.Format.Elf.Header
import ViperVM.Format.Binary.Word

-- | Create relocation type
toRelocType :: Arch -> Word32 -> RelocationType
toRelocType arch typ =
   case arch of
      Arch68K    -> RelocationM68K   (toEnum (fromIntegral typ))
      Arch386    -> RelocationI386   (toEnum (fromIntegral typ))
      ArchX86_64 -> RelocationX86_64 (toEnum (fromIntegral typ))
      _          -> RelocationTypeUnknown typ

-- | Get relocation type code
fromRelocType :: RelocationType -> Word32
fromRelocType typ =
   case typ of
      RelocationM68K   t      -> fromIntegral (fromEnum t)
      RelocationI386   t      -> fromIntegral (fromEnum t)
      RelocationX86_64 t      -> fromIntegral (fromEnum t)
      RelocationTypeUnknown t -> t

-- | Relocation type
data RelocationType
   = RelocationM68K        M68K_Relocation         -- ^ Motorola 68k
   | RelocationI386        I386_Relocation         -- ^ Intel 80386
   | RelocationX86_64      X86_64_Relocation       -- ^ x86-64
   | RelocationTypeUnknown Word32
   deriving (Show)

-- | Motorola m68k relocation
data M68K_Relocation
   = M68K_NONE          -- ^ No reloc
   | M68K_32            -- ^ Direct 32 bit
   | M68K_16            -- ^ Direct 16 bit
   | M68K_8             -- ^ Direct 8 bit
   | M68K_PC32          -- ^ PC relative 32 bit
   | M68K_PC16          -- ^ PC relative 16 bit
   | M68K_PC8           -- ^ PC relative 8 bit
   | M68K_GOT32         -- ^ 32 bit PC relative GOT entry
   | M68K_GOT16         -- ^ 16 bit PC relative GOT entry
   | M68K_GOT8          -- ^ 8 bit PC relative GOT entry
   | M68K_GOT32O        -- ^ 32 bit GOT offset
   | M68K_GOT16O        -- ^ 16 bit GOT offset
   | M68K_GOT8O         -- ^ 8 bit GOT offset
   | M68K_PLT32         -- ^ 32 bit PC relative PLT address
   | M68K_PLT16         -- ^ 16 bit PC relative PLT address
   | M68K_PLT8          -- ^ 8 bit PC relative PLT address
   | M68K_PLT32O        -- ^ 32 bit PLT offset
   | M68K_PLT16O        -- ^ 16 bit PLT offset
   | M68K_PLT8O         -- ^ 8 bit PLT offset
   | M68K_COPY          -- ^ Copy symbol at runtime
   | M68K_GLOB_DAT      -- ^ Create GOT entry
   | M68K_JMP_SLOT      -- ^ Create PLT entry
   | M68K_RELATIVE      -- ^ Adjust by program base
   | M68K_TLS_GD32      -- ^ 32 bit GOT offset for GD
   | M68K_TLS_GD16      -- ^ 16 bit GOT offset for GD
   | M68K_TLS_GD8       -- ^ 8 bit GOT offset for GD
   | M68K_TLS_LDM32     -- ^ 32 bit GOT offset for LDM
   | M68K_TLS_LDM16     -- ^ 16 bit GOT offset for LDM
   | M68K_TLS_LDM8      -- ^ 8 bit GOT offset for LDM
   | M68K_TLS_LDO32     -- ^ 32 bit module-relative offset
   | M68K_TLS_LDO16     -- ^ 16 bit module-relative offset
   | M68K_TLS_LDO8      -- ^ 8 bit module-relative offset
   | M68K_TLS_IE32      -- ^ 32 bit GOT offset for IE
   | M68K_TLS_IE16      -- ^ 16 bit GOT offset for IE
   | M68K_TLS_IE8       -- ^ 8 bit GOT offset for IE
   | M68K_TLS_LE32      -- ^ 32 bit offset relative to static TLS block
   | M68K_TLS_LE16      -- ^ 16 bit offset relative to static TLS block
   | M68K_TLS_LE8       -- ^ 8 bit offset relative to static TLS block
   | M68K_TLS_DTPMOD32  -- ^ 32 bit module number
   | M68K_TLS_DTPREL32  -- ^ 32 bit module-relative offset
   | M68K_TLS_TPREL32   -- ^ 32 bit TP-relative offset
   deriving (Show,Eq)

instance Enum M68K_Relocation where
   fromEnum x = case x of
      M68K_NONE          -> 0
      M68K_32            -> 1
      M68K_16            -> 2
      M68K_8             -> 3
      M68K_PC32          -> 4
      M68K_PC16          -> 5
      M68K_PC8           -> 6
      M68K_GOT32         -> 7
      M68K_GOT16         -> 8
      M68K_GOT8          -> 9
      M68K_GOT32O        -> 10
      M68K_GOT16O        -> 11
      M68K_GOT8O         -> 12
      M68K_PLT32         -> 13
      M68K_PLT16         -> 14
      M68K_PLT8          -> 15
      M68K_PLT32O        -> 16
      M68K_PLT16O        -> 17
      M68K_PLT8O         -> 18
      M68K_COPY          -> 19
      M68K_GLOB_DAT      -> 20
      M68K_JMP_SLOT      -> 21
      M68K_RELATIVE      -> 22
      M68K_TLS_GD32      -> 25
      M68K_TLS_GD16      -> 26
      M68K_TLS_GD8       -> 27
      M68K_TLS_LDM32     -> 28
      M68K_TLS_LDM16     -> 29
      M68K_TLS_LDM8      -> 30
      M68K_TLS_LDO32     -> 31
      M68K_TLS_LDO16     -> 32
      M68K_TLS_LDO8      -> 33
      M68K_TLS_IE32      -> 34
      M68K_TLS_IE16      -> 35
      M68K_TLS_IE8       -> 36
      M68K_TLS_LE32      -> 37
      M68K_TLS_LE16      -> 38
      M68K_TLS_LE8       -> 39
      M68K_TLS_DTPMOD32  -> 40
      M68K_TLS_DTPREL32  -> 41
      M68K_TLS_TPREL32   -> 42

   toEnum x = case x of
      0     -> M68K_NONE
      1     -> M68K_32
      2     -> M68K_16
      3     -> M68K_8
      4     -> M68K_PC32
      5     -> M68K_PC16
      6     -> M68K_PC8
      7     -> M68K_GOT32
      8     -> M68K_GOT16
      9     -> M68K_GOT8
      10    -> M68K_GOT32O
      11    -> M68K_GOT16O
      12    -> M68K_GOT8O
      13    -> M68K_PLT32
      14    -> M68K_PLT16
      15    -> M68K_PLT8
      16    -> M68K_PLT32O
      17    -> M68K_PLT16O
      18    -> M68K_PLT8O
      19    -> M68K_COPY
      20    -> M68K_GLOB_DAT
      21    -> M68K_JMP_SLOT
      22    -> M68K_RELATIVE
      25    -> M68K_TLS_GD32
      26    -> M68K_TLS_GD16
      27    -> M68K_TLS_GD8
      28    -> M68K_TLS_LDM32
      29    -> M68K_TLS_LDM16
      30    -> M68K_TLS_LDM8
      31    -> M68K_TLS_LDO32
      32    -> M68K_TLS_LDO16
      33    -> M68K_TLS_LDO8
      34    -> M68K_TLS_IE32
      35    -> M68K_TLS_IE16
      36    -> M68K_TLS_IE8
      37    -> M68K_TLS_LE32
      38    -> M68K_TLS_LE16
      39    -> M68K_TLS_LE8
      40    -> M68K_TLS_DTPMOD32
      41    -> M68K_TLS_DTPREL32
      42    -> M68K_TLS_TPREL32
      _     -> error $ "Unknown m68K relocation (" ++ show x ++ ")"


-- | I386 relocations
data I386_Relocation
   = I386_NONE            -- ^ No reloc
   | I386_32              -- ^ Direct 32 bit
   | I386_PC32            -- ^ PC relative 32 bit
   | I386_GOT32           -- ^ 32 bit GOT entry
   | I386_PLT32           -- ^ 32 bit PLT address
   | I386_COPY            -- ^ Copy symbol at runtime
   | I386_GLOB_DAT        -- ^ Create GOT entry
   | I386_JMP_SLOT        -- ^ Create PLT entry
   | I386_RELATIVE        -- ^ Adjust by program base
   | I386_GOTOFF          -- ^ 32 bit offset to GOT
   | I386_GOTPC           -- ^ 32 bit PC relative offset to GOT
   | I386_32PLT           -- ^ 
   | I386_TLS_TPOFF       -- ^ Offset in static TLS block
   | I386_TLS_IE          -- ^ Address of GOT entry for static TLS block offset
   | I386_TLS_GOTIE       -- ^ GOT entry for static TLS block offset
   | I386_TLS_LE          -- ^ Offset relative to static TLS block
   | I386_TLS_GD          -- ^ Direct 32 bit for GNU version of general dynamic thread local data
   | I386_TLS_LDM         -- ^ Direct 32 bit for GNU version of local dynamic thread local data in LE code
   | I386_16
   | I386_PC16
   | I386_8
   | I386_PC8
   | I386_TLS_GD_32       -- ^ Direct 32 bit for general dynamic thread local data
   | I386_TLS_GD_PUSH     -- ^ Tag for pushl in GD TLS code
   | I386_TLS_GD_CALL     -- ^ Relocation for call to __tls_get_addr()
   | I386_TLS_GD_POP      -- ^ Tag for popl in GD TLS code
   | I386_TLS_LDM_32      -- ^ Direct 32 bit for local dynamic thread local data in LE code
   | I386_TLS_LDM_PUSH    -- ^ Tag for pushl in LDM TLS code
   | I386_TLS_LDM_CALL    -- ^ Relocation for call to __tls_get_addr() in LDM code
   | I386_TLS_LDM_POP     -- ^ Tag for popl in LDM TLS code
   | I386_TLS_LDO_32      -- ^ Offset relative to TLS block
   | I386_TLS_IE_32       -- ^ GOT entry for negated static TLS block offset
   | I386_TLS_LE_32       -- ^ Negated offset relative to static TLS block
   | I386_TLS_DTPMOD32    -- ^ ID of module containing symbol
   | I386_TLS_DTPOFF32    -- ^ Offset in TLS block
   | I386_TLS_TPOFF32     -- ^ Negated offset in static TLS block
   | I386_SIZE32          -- ^ 32-bit symbol size
   | I386_TLS_GOTDESC     -- ^ GOT offset for TLS descriptor.
   | I386_TLS_DESC_CALL   -- ^ Marker of call through TLS descriptor for relaxation.
   | I386_TLS_DESC        -- ^ TLS descriptor containing pointer to code and to argument, returning the TLS offset for the symbol.
   | I386_IRELATIVE       -- ^ Adjust indirectly by program base
   deriving (Show,Eq)

instance Enum I386_Relocation where
   fromEnum x = case x of
      I386_NONE            -> 0
      I386_32              -> 1
      I386_PC32            -> 2
      I386_GOT32           -> 3
      I386_PLT32           -> 4
      I386_COPY            -> 5
      I386_GLOB_DAT        -> 6
      I386_JMP_SLOT        -> 7
      I386_RELATIVE        -> 8
      I386_GOTOFF          -> 9
      I386_GOTPC           -> 10
      I386_32PLT           -> 11
      I386_TLS_TPOFF       -> 14
      I386_TLS_IE          -> 15
      I386_TLS_GOTIE       -> 16
      I386_TLS_LE          -> 17
      I386_TLS_GD          -> 18
      I386_TLS_LDM         -> 19
      I386_16              -> 20
      I386_PC16            -> 21
      I386_8               -> 22
      I386_PC8             -> 23
      I386_TLS_GD_32       -> 24
      I386_TLS_GD_PUSH     -> 25
      I386_TLS_GD_CALL     -> 26
      I386_TLS_GD_POP      -> 27
      I386_TLS_LDM_32      -> 28
      I386_TLS_LDM_PUSH    -> 29
      I386_TLS_LDM_CALL    -> 30
      I386_TLS_LDM_POP     -> 31
      I386_TLS_LDO_32      -> 32
      I386_TLS_IE_32       -> 33
      I386_TLS_LE_32       -> 34
      I386_TLS_DTPMOD32    -> 35
      I386_TLS_DTPOFF32    -> 36
      I386_TLS_TPOFF32     -> 37
      I386_SIZE32          -> 38
      I386_TLS_GOTDESC     -> 39
      I386_TLS_DESC_CALL   -> 40
      I386_TLS_DESC        -> 41
      I386_IRELATIVE       -> 42

   toEnum x = case x of
      0    -> I386_NONE
      1    -> I386_32
      2    -> I386_PC32
      3    -> I386_GOT32
      4    -> I386_PLT32
      5    -> I386_COPY
      6    -> I386_GLOB_DAT
      7    -> I386_JMP_SLOT
      8    -> I386_RELATIVE
      9    -> I386_GOTOFF
      10   -> I386_GOTPC
      11   -> I386_32PLT
      14   -> I386_TLS_TPOFF
      15   -> I386_TLS_IE
      16   -> I386_TLS_GOTIE
      17   -> I386_TLS_LE
      18   -> I386_TLS_GD
      19   -> I386_TLS_LDM
      20   -> I386_16
      21   -> I386_PC16
      22   -> I386_8
      23   -> I386_PC8
      24   -> I386_TLS_GD_32
      25   -> I386_TLS_GD_PUSH
      26   -> I386_TLS_GD_CALL
      27   -> I386_TLS_GD_POP
      28   -> I386_TLS_LDM_32
      29   -> I386_TLS_LDM_PUSH
      30   -> I386_TLS_LDM_CALL
      31   -> I386_TLS_LDM_POP
      32   -> I386_TLS_LDO_32
      33   -> I386_TLS_IE_32
      34   -> I386_TLS_LE_32
      35   -> I386_TLS_DTPMOD32
      36   -> I386_TLS_DTPOFF32
      37   -> I386_TLS_TPOFF32
      38   -> I386_SIZE32
      39   -> I386_TLS_GOTDESC
      40   -> I386_TLS_DESC_CALL
      41   -> I386_TLS_DESC
      42   -> I386_IRELATIVE
      _     -> error $ "Unknown i386 relocation (" ++ show x ++ ")"


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
   deriving (Show, Eq, Enum)
