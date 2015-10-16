module ViperVM.Format.Elf.Dynamic
   ( DynamicEntry (..)
   , getDynamicEntry
   , putDynamicEntry
   )
where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Word
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Binary.Get
import Data.Binary.Put

import ViperVM.Utils.BitSet (EnumBitSet,BitSet)
import qualified ViperVM.Utils.BitSet as BitSet

import ViperVM.Format.Elf.PreHeader
import ViperVM.Format.Elf.Header

data DynamicEntry = DynamicEntry
   { dynType   :: DynamicEntryType
   , dynValue  :: Word64
   }
   deriving (Show)

data DynamicEntryType
   = DynTypeNone                     -- ^ Marks end of dynamic section
   | DynTypeNeededLibraryName        -- ^ Name of needed library
   | DynTypePLTRelocSize             -- ^ Size in bytes of PLT relocs
   | DynTypePLTGOT                   -- ^ Processor defined value
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
   | DynTypeInitFunctionArray        -- ^ Array with addresses of init fct 
   | DynTypeFiniFunctionArray        -- ^ Array with addresses of fini fct 
   | DynTypeInitFunctionArraySize    -- ^ Size in bytes of InitFunctionArray
   | DynTypeFiniFunctionArraySize    -- ^ Size in bytes of FinFunctionArray
   | DynTypeLibrarySearchPath        -- ^ Library search path 
   | DynTypeFlags                    -- ^ Flags for the object being loaded 
   | DynTypeEncoding                 -- ^ Start of encoded range 
   | DynTypePreInitFunctionArray     -- ^ Array with addresses of preinit fct
   | DynTypePreInitFunctionArraySize -- ^ Size in bytes of PreInitFunctionArray

   | DynTypeGNUPrelinkedTimestamp    -- ^ Prelinking timestamp 
   | DynTypeGNUConflictSize          -- ^ Size of conflict section 
   | DynTypeGNULibraryListSize       -- ^ Size of library list 
   | DynTypeChecksum
   | DynTypePLTPaddingSize
   | DynTypeMoveEntrySize
   | DynTypeMoveSize
   | DynTypeFeatureSelection         -- ^ Feature selection (DTF_*).  
   | DynTypePositionalFlag           -- ^ Flags effecting the following dynamic entry
   | DynTypeSymbolInfoSize           -- ^ Size of syminfo table (in bytes) 
   | DynTypeSymbolInfoEntrySize      -- ^ Sizeo of syminfo entry

   | DynTypeGNUHashTable             -- ^ GNU-style hash table.  
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
   deriving (Show)

getDynamicEntry :: PreHeader -> Get DynamicEntry
getDynamicEntry pre = do
   let (_,_,_,gwN) = getGetters pre
   DynamicEntry
      <$> (toDynamicEntryType <$> gwN)
      <*> gwN

putDynamicEntry :: PreHeader -> DynamicEntry -> Put
putDynamicEntry pre de = do
   let (_,_,_,pwN) = getPutters pre
   pwN (fromDynamicEntryType $ dynType de)
   pwN (dynValue de)

fromDynamicEntryType :: DynamicEntryType -> Word64
fromDynamicEntryType x = case x of
   DynTypeNone                     -> 0
   DynTypeNeededLibraryName        -> 1
   DynTypePLTRelocSize             -> 2
   DynTypePLTGOT                   -> 3
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
   DynTypeInitFunctionArray        -> 25
   DynTypeFiniFunctionArray        -> 26
   DynTypeInitFunctionArraySize    -> 27
   DynTypeFiniFunctionArraySize    -> 28
   DynTypeLibrarySearchPath        -> 29
   DynTypeFlags                    -> 30
   --DynTypeEncoding               -> 32
   DynTypePreInitFunctionArray     -> 32
   DynTypePreInitFunctionArraySize -> 33
   
   DynTypeGNUPrelinkedTimestamp    -> 0x6ffffdf5
   DynTypeGNUConflictSize          -> 0x6ffffdf6
   DynTypeGNULibraryListSize       -> 0x6ffffdf7
   DynTypeChecksum                 -> 0x6ffffdf8
   DynTypePLTPaddingSize           -> 0x6ffffdf9 
   DynTypeMoveEntrySize            -> 0x6ffffdfa 
   DynTypeMoveSize                 -> 0x6ffffdfb
   DynTypeFeatureSelection         -> 0x6ffffdfc
   DynTypePositionalFlag           -> 0x6ffffdfd
   DynTypeSymbolInfoSize           -> 0x6ffffdfe
   DynTypeSymbolInfoEntrySize      -> 0x6ffffdff
   
   DynTypeGNUHashTable             -> 0x6ffffef5
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
   3               -> DynTypePLTGOT                   
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
   25              -> DynTypeInitFunctionArray        
   26              -> DynTypeFiniFunctionArray        
   27              -> DynTypeInitFunctionArraySize    
   28              -> DynTypeFiniFunctionArraySize    
   29              -> DynTypeLibrarySearchPath        
   30              -> DynTypeFlags                    
   --32            -> DynTypeEncoding               
   32              -> DynTypePreInitFunctionArray     
   33              -> DynTypePreInitFunctionArraySize 
               
   0x6ffffdf5      -> DynTypeGNUPrelinkedTimestamp    
   0x6ffffdf6      -> DynTypeGNUConflictSize          
   0x6ffffdf7      -> DynTypeGNULibraryListSize       
   0x6ffffdf8      -> DynTypeChecksum                 
   0x6ffffdf9      -> DynTypePLTPaddingSize           
   0x6ffffdfa      -> DynTypeMoveEntrySize            
   0x6ffffdfb      -> DynTypeMoveSize                 
   0x6ffffdfc      -> DynTypeFeatureSelection         
   0x6ffffdfd      -> DynTypePositionalFlag           
   0x6ffffdfe      -> DynTypeSymbolInfoSize           
   0x6ffffdff      -> DynTypeSymbolInfoEntrySize      
               
   0x6ffffef5      -> DynTypeGNUHashTable             
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
