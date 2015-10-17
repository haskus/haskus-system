module ViperVM.Format.Elf.Dynamic
   ( RawDynamicEntry (..)
   , getRawDynamicEntry
   , putRawDynamicEntry
   , DynamicEntryFlag (..)
   , DynamicEntryFlags
   , DynamicEntryType (..)
   , DynamicStateFlag (..)
   , DynamicStateFlags
   , DynamicFeature (..)
   , DynamicFeatures
   , DynamicPositionalFlag (..)
   , DynamicPositionalFlags
   )
where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put

import ViperVM.Utils.BitSet (EnumBitSet,BitSet)

import ViperVM.Format.Elf.PreHeader

data RawDynamicEntry = RawDynamicEntry
   { rawDynType   :: DynamicEntryType
   , rawDynValue  :: Word64
   }
   deriving (Show,Eq)

data DynamicEntryType
   = DynTypeNone                     -- ^ Marks end of dynamic section
   | DynTypeNeededLibraryName        -- ^ Name of needed library
   | DynTypePLTRelocSize             -- ^ Size in bytes of PLT relocs
   | DynTypePLTGOTAddress            -- ^ Processor defined value
   | DynTypeSymbolHashTableAddress   -- ^ Address of symbol hash table
   | DynTypeStringTableAddress       -- ^ Address of string table
   | DynTypeSymbolTableAddress       -- ^ Address of symbol table
   | DynTypeRelocaAddress            -- ^ Address of Rela relocs 
   | DynTypeRelocaSize               -- ^ Total size of Rela relocs 
   | DynTypeRelocaEntrySize          -- ^ Size of one Rela reloc 
   | DynTypeStringTableSize          -- ^ Size of string table 
   | DynTypeSymbolEntrySize          -- ^ Size of one symbol table entry 
   | DynTypeInitFunctionAddress      -- ^ Address of init function 
   | DynTypeFiniFunctionAddress      -- ^ Address of termination function 
   | DynTypeSharedObjectName         -- ^ Name of shared object 
   | DynTypeLibrarySearchPathOld     -- ^ Library search path (deprecated) 
   | DynTypeSymbolic                 -- ^ Start symbol search here 
   | DynTypeRelocAddress             -- ^ Address of Rel relocs 
   | DynTypeRelocSize                -- ^ Total size of Rel relocs 
   | DynTypeRelocEntrySize           -- ^ Size of one Rel reloc 
   | DynTypePLTRelocType             -- ^ Type of reloc in PLT 
   | DynTypeDebug                    -- ^ For debugging; unspecified 
   | DynTypeRelocatableText          -- ^ Reloc might modify .text 
   | DynTypePLTRelocAddress          -- ^ Address of PLT relocs 
   | DynTypeBindNow                  -- ^ Process relocations of object 
   | DynTypeInitFunctionArrayAddress -- ^ Array with addresses of init fct 
   | DynTypeFiniFunctionArrayAddress -- ^ Array with addresses of fini fct 
   | DynTypeInitFunctionArraySize    -- ^ Size in bytes of InitFunctionArray
   | DynTypeFiniFunctionArraySize    -- ^ Size in bytes of FinFunctionArray
   | DynTypeLibrarySearchPath        -- ^ Library search path 
   | DynTypeFlags                    -- ^ Flags for the object being loaded 
   -- | DynTypeEncoding              -- ^ Start of encoded range 
   | DynTypePreInitFunctionArrayAddress -- ^ Array with addresses of preinit fct
   | DynTypePreInitFunctionArraySize -- ^ Size in bytes of PreInitFunctionArray

   | DynTypeGNUPrelinkedTimestamp    -- ^ Prelinking timestamp 
   | DynTypeGNUConflictSize          -- ^ Size of conflict section 
   | DynTypeGNULibraryListSize       -- ^ Size of library list 
   | DynTypeChecksum
   | DynTypePLTPaddingSize
   | DynTypeMoveEntrySize
   | DynTypeMoveSize
   | DynTypeFeatureSelection         -- ^ Feature selection (DTF_*).  
   | DynTypePositionalFlags          -- ^ Flags effecting the following dynamic entry
   | DynTypeSymbolInfoSize           -- ^ Size of syminfo table (in bytes) 
   | DynTypeSymbolInfoEntrySize      -- ^ Sizeo of syminfo entry

   | DynTypeGNUHashTableAddress      -- ^ GNU-style hash table.  
   | DynTypeTLSDescPLT
   | DynTypeTLSDescGOT
   | DynTypeGNUConflictSection       -- ^ Start of conflict section 
   | DynTypeGNULibraryList           -- ^ Library list 
   | DynTypeConfigInfo               -- ^ Configuration information
   | DynTypeDependencyAuditing       -- ^ Dependency auditing
   | DynTypeObjectAuditing           -- ^ Object auditing
   | DynTypePLTPadding               -- ^ PLT padding
   | DynTypeMoveTable                -- ^ Move table
   | DynTypeSymbolInfoTable          -- ^ Syminfo table

   | DynTypeSymbolVersion
   | DynTypeRelocaCount
   | DynTypeRelocCount

   | DynTypeStateFlags               -- ^ State flags
   | DynTypeVersionDefinitionTable   -- ^ Address of version definition table 
   | DynTypeVersionDefinitionCount   -- ^ Number of version definitions 
   | DynTypeVersionNeededTable       -- ^ Address of table with needed versions 
   | DynTypeVersionNeededCount       -- ^ Number of needed versions

   | DynTypeLoadBefore               -- ^ Shared object to load before self 
   | DynTypeGetValuesFrom            -- ^ Shared object to get values from 

   | DynTypeUnknown Word64           -- ^ Unknown dynamic type
   deriving (Show,Eq)

getRawDynamicEntry :: PreHeader -> Get RawDynamicEntry
getRawDynamicEntry pre = do
   let (_,_,_,gwN) = getGetters pre
   RawDynamicEntry
      <$> (toDynamicEntryType <$> gwN)
      <*> gwN

putRawDynamicEntry :: PreHeader -> RawDynamicEntry -> Put
putRawDynamicEntry pre de = do
   let (_,_,_,pwN) = getPutters pre
   pwN (fromDynamicEntryType $ rawDynType de)
   pwN (rawDynValue de)

fromDynamicEntryType :: DynamicEntryType -> Word64
fromDynamicEntryType x = case x of
   DynTypeNone                     -> 0
   DynTypeNeededLibraryName        -> 1
   DynTypePLTRelocSize             -> 2
   DynTypePLTGOTAddress            -> 3
   DynTypeSymbolHashTableAddress   -> 4
   DynTypeStringTableAddress       -> 5
   DynTypeSymbolTableAddress       -> 6
   DynTypeRelocaAddress            -> 7
   DynTypeRelocaSize               -> 8
   DynTypeRelocaEntrySize          -> 9
   DynTypeStringTableSize          -> 10
   DynTypeSymbolEntrySize          -> 11
   DynTypeInitFunctionAddress      -> 12
   DynTypeFiniFunctionAddress      -> 13
   DynTypeSharedObjectName         -> 14
   DynTypeLibrarySearchPathOld     -> 15
   DynTypeSymbolic                 -> 16
   DynTypeRelocAddress             -> 17
   DynTypeRelocSize                -> 18
   DynTypeRelocEntrySize           -> 19
   DynTypePLTRelocType             -> 20
   DynTypeDebug                    -> 21
   DynTypeRelocatableText          -> 22
   DynTypePLTRelocAddress          -> 23
   DynTypeBindNow                  -> 24
   DynTypeInitFunctionArrayAddress -> 25
   DynTypeFiniFunctionArrayAddress -> 26
   DynTypeInitFunctionArraySize    -> 27
   DynTypeFiniFunctionArraySize    -> 28
   DynTypeLibrarySearchPath        -> 29
   DynTypeFlags                    -> 30
   --DynTypeEncoding               -> 32
   DynTypePreInitFunctionArrayAddress -> 32
   DynTypePreInitFunctionArraySize -> 33
   
   DynTypeGNUPrelinkedTimestamp    -> 0x6ffffdf5
   DynTypeGNUConflictSize          -> 0x6ffffdf6
   DynTypeGNULibraryListSize       -> 0x6ffffdf7
   DynTypeChecksum                 -> 0x6ffffdf8
   DynTypePLTPaddingSize           -> 0x6ffffdf9 
   DynTypeMoveEntrySize            -> 0x6ffffdfa 
   DynTypeMoveSize                 -> 0x6ffffdfb
   DynTypeFeatureSelection         -> 0x6ffffdfc
   DynTypePositionalFlags          -> 0x6ffffdfd
   DynTypeSymbolInfoSize           -> 0x6ffffdfe
   DynTypeSymbolInfoEntrySize      -> 0x6ffffdff
   
   DynTypeGNUHashTableAddress      -> 0x6ffffef5
   DynTypeTLSDescPLT               -> 0x6ffffef6
   DynTypeTLSDescGOT               -> 0x6ffffef7
   DynTypeGNUConflictSection       -> 0x6ffffef8
   DynTypeGNULibraryList           -> 0x6ffffef9
   DynTypeConfigInfo               -> 0x6ffffefa
   DynTypeDependencyAuditing       -> 0x6ffffefb
   DynTypeObjectAuditing           -> 0x6ffffefc
   DynTypePLTPadding               -> 0x6ffffefd
   DynTypeMoveTable                -> 0x6ffffefe
   DynTypeSymbolInfoTable          -> 0x6ffffeff
   
   DynTypeSymbolVersion            -> 0x6ffffff0
   DynTypeRelocaCount              -> 0x6ffffff9
   DynTypeRelocCount               -> 0x6ffffffa
   DynTypeStateFlags               -> 0x6ffffffb
   DynTypeVersionDefinitionTable   -> 0x6ffffffc
   DynTypeVersionDefinitionCount   -> 0x6ffffffd
   DynTypeVersionNeededTable       -> 0x6ffffffe
   DynTypeVersionNeededCount       -> 0x6fffffff
   
   DynTypeLoadBefore               -> 0x7ffffffd
   DynTypeGetValuesFrom            -> 0x7fffffff

   DynTypeUnknown v                -> v

toDynamicEntryType :: Word64 -> DynamicEntryType
toDynamicEntryType x = case x of
   0               -> DynTypeNone
   1               -> DynTypeNeededLibraryName
   2               -> DynTypePLTRelocSize
   3               -> DynTypePLTGOTAddress
   4               -> DynTypeSymbolHashTableAddress
   5               -> DynTypeStringTableAddress
   6               -> DynTypeSymbolTableAddress
   7               -> DynTypeRelocaAddress
   8               -> DynTypeRelocaSize
   9               -> DynTypeRelocaEntrySize
   10              -> DynTypeStringTableSize
   11              -> DynTypeSymbolEntrySize
   12              -> DynTypeInitFunctionAddress
   13              -> DynTypeFiniFunctionAddress
   14              -> DynTypeSharedObjectName
   15              -> DynTypeLibrarySearchPathOld
   16              -> DynTypeSymbolic
   17              -> DynTypeRelocAddress
   18              -> DynTypeRelocSize
   19              -> DynTypeRelocEntrySize
   20              -> DynTypePLTRelocType
   21              -> DynTypeDebug
   22              -> DynTypeRelocatableText
   23              -> DynTypePLTRelocAddress
   24              -> DynTypeBindNow
   25              -> DynTypeInitFunctionArrayAddress
   26              -> DynTypeFiniFunctionArrayAddress
   27              -> DynTypeInitFunctionArraySize
   28              -> DynTypeFiniFunctionArraySize
   29              -> DynTypeLibrarySearchPath
   30              -> DynTypeFlags
   --32            -> DynTypeEncoding
   32              -> DynTypePreInitFunctionArrayAddress
   33              -> DynTypePreInitFunctionArraySize

   0x6ffffdf5      -> DynTypeGNUPrelinkedTimestamp
   0x6ffffdf6      -> DynTypeGNUConflictSize
   0x6ffffdf7      -> DynTypeGNULibraryListSize
   0x6ffffdf8      -> DynTypeChecksum
   0x6ffffdf9      -> DynTypePLTPaddingSize
   0x6ffffdfa      -> DynTypeMoveEntrySize
   0x6ffffdfb      -> DynTypeMoveSize
   0x6ffffdfc      -> DynTypeFeatureSelection
   0x6ffffdfd      -> DynTypePositionalFlags
   0x6ffffdfe      -> DynTypeSymbolInfoSize
   0x6ffffdff      -> DynTypeSymbolInfoEntrySize

   0x6ffffef5      -> DynTypeGNUHashTableAddress
   0x6ffffef6      -> DynTypeTLSDescPLT
   0x6ffffef7      -> DynTypeTLSDescGOT
   0x6ffffef8      -> DynTypeGNUConflictSection
   0x6ffffef9      -> DynTypeGNULibraryList
   0x6ffffefa      -> DynTypeConfigInfo
   0x6ffffefb      -> DynTypeDependencyAuditing
   0x6ffffefc      -> DynTypeObjectAuditing
   0x6ffffefd      -> DynTypePLTPadding
   0x6ffffefe      -> DynTypeMoveTable
   0x6ffffeff      -> DynTypeSymbolInfoTable

   0x6ffffff0      -> DynTypeSymbolVersion
   0x6ffffff9      -> DynTypeRelocaCount
   0x6ffffffa      -> DynTypeRelocCount
   0x6ffffffb      -> DynTypeStateFlags
   0x6ffffffc      -> DynTypeVersionDefinitionTable
   0x6ffffffd      -> DynTypeVersionDefinitionCount
   0x6ffffffe      -> DynTypeVersionNeededTable
   0x6fffffff      -> DynTypeVersionNeededCount

   0x7ffffffd      -> DynTypeLoadBefore
   0x7fffffff      -> DynTypeGetValuesFrom

   v               -> DynTypeUnknown v


-- | Dynamic entry flags (DynTypeFlags)
data DynamicEntryFlag
   = DynFlagOrigin            -- ^ Object may use DF_ORIGIN
   | DynFlagSymbolic          -- ^ Symbol resolutions starts here
   | DynFlagHasTextRelocation -- ^ Object contains text relocations
   | DynFlagBindNow           -- ^ No lazy binding for this object
   | DynFlagStaticTLS         -- ^ Module uses the static TLS model
   deriving (Show,Eq,Enum)

instance EnumBitSet DynamicEntryFlag

type DynamicEntryFlags = BitSet Word64 DynamicEntryFlag

-- | Dynamic state flags (DynTypeStateFlags)
data DynamicStateFlag
   = DynStateFlagNow                        -- ^ Set RTLD_NOW for this object.  
   | DynStateFlagGlobal                     -- ^ Set RTLD_GLOBAL for this object.  
   | DynStateFlagGroup                      -- ^ Set RTLD_GROUP for this object.  
   | DynStateFlagNoDelete                   -- ^ Set RTLD_NODELETE for this object.
   | DynStateFlagLoadFilter                 -- ^ Trigger filtee loading at runtime.
   | DynStateFlagInitFirst                  -- ^ Set RTLD_INITFIRST for this object
   | DynStateFlagNoOpen                     -- ^ Set RTLD_NOOPEN for this object.  
   | DynStateFlagOrigin                     -- ^ $ORIGIN must be handled.  
   | DynStateFlagDirect                     -- ^ Direct binding enabled.  
   | DynStateFlagTrans
   | DynStateFlagInterpose                  -- ^ Object is used to interpose.  
   | DynStateFlagIgnoreDefaultLibrarySearch -- ^ Ignore default lib search path.  
   | DynStateFlagNoDump                     -- ^ Object can't be dldump'ed.  
   | DynStateFlagAlternativeConfig          -- ^ Configuration alternative created.
   | DynStateFlagEndFiltee                  -- ^ Filtee terminates filters search. 
   | DynStateFlagDispRelocDNE               -- ^ Disp reloc applied at build time. 
   | DynStateFlagDispRelocPND               -- ^ Disp reloc applied at run-time.  
   | DynStateFlagNoDirect                   -- ^ Object has no-direct binding. 
   | DynStateFlagIgnoreMultipleDef
   | DynStateFlagNoKSymbols
   | DynStateFlagNoHeader
   | DynStateFlagEdited                     -- ^ Object is modified after built.  
   | DynStateFlagNoReloc                    -- ^ 
   | DynStateFlagSymbolInterposers          -- ^ Object has individual interposers.  
   | DynStateFlagGlobalAudit                -- ^ Global auditing required.  
   | DynStateFlagSingletonSymbols           -- ^ Singleton symbols are used.  
   deriving (Show,Eq,Enum)

instance EnumBitSet DynamicStateFlag

type DynamicStateFlags = BitSet Word64 DynamicStateFlag

-- | Features
data DynamicFeature
   = DynFeatureParInit
   | DynFeatureConfExp
   deriving (Show,Eq,Enum)

instance EnumBitSet DynamicFeature

type DynamicFeatures = BitSet Word64 DynamicFeature

-- | Dynamic positional flags affecting only the next entry
data DynamicPositionalFlag
   = DynPositionalFlagLazyLoad   -- ^ Lazyload following object
   | DynPositionalFlagGroupPerm  -- ^ Symbols from next object are not generally available
   deriving (Show,Eq,Enum)

instance EnumBitSet DynamicPositionalFlag

type DynamicPositionalFlags = BitSet Word64 DynamicPositionalFlag
