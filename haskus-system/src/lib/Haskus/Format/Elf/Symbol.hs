-- | ELF symbol tables
module Haskus.Format.Elf.Symbol
   ( SymbolEntry (..)
   , SymbolBinding (..)
   , SymbolType (..)
   , SymbolVisibility (..)
   , SymbolInfo (..)
   , getSymbolEntry
   , putSymbolEntry
   )
where

import Haskus.Format.Binary.Bits
import Haskus.Format.Binary.Get
import Haskus.Format.Binary.Put
import Haskus.Format.Binary.Word
import Haskus.Format.Elf.PreHeader


-- | Symbol entry in a symbol table
data SymbolEntry = SymbolEntry
   { symbolNameIndex    :: Word32
   , symbolBinding      :: SymbolBinding
   , symbolType         :: SymbolType
   , symbolVisibility   :: SymbolVisibility
   , symbolInfo         :: SymbolInfo
   , symbolValue        :: Word64
   , symbolSize         :: Word64
   } deriving (Show)

-- | Symbol binding kind
data SymbolBinding
   = SymbolBindingLocal
   | SymbolBindingGlobal
   | SymbolBindingWeak
   | SymbolBindingUnknown Word8
   deriving (Show)

instance Enum SymbolBinding where
   fromEnum x = case x of
      SymbolBindingLocal      -> 0
      SymbolBindingGlobal     -> 1
      SymbolBindingWeak       -> 2
      SymbolBindingUnknown v  -> fromIntegral v

   toEnum x = case x of
      0 -> SymbolBindingLocal
      1 -> SymbolBindingGlobal
      2 -> SymbolBindingWeak
      v -> SymbolBindingUnknown (fromIntegral v)

-- | Symbol type
data SymbolType
   = SymbolTypeNone
   | SymbolTypeData
   | SymbolTypeCode
   | SymbolTypeSection
   | SymbolTypeFile
   | SymbolTypeCommonData
   | SymbolTypeTLSData
   | SymbolTypeUnknown Word8
   deriving (Show)

instance Enum SymbolType where
   fromEnum x = case x of
      SymbolTypeNone          -> 0
      SymbolTypeData          -> 1
      SymbolTypeCode          -> 2
      SymbolTypeSection       -> 3
      SymbolTypeFile          -> 4
      SymbolTypeCommonData    -> 5
      SymbolTypeTLSData       -> 6
      SymbolTypeUnknown v     -> fromIntegral v

   toEnum x = case x of
      0 -> SymbolTypeNone
      1 -> SymbolTypeData
      2 -> SymbolTypeCode
      3 -> SymbolTypeSection
      4 -> SymbolTypeFile
      5 -> SymbolTypeCommonData
      6 -> SymbolTypeTLSData
      v -> SymbolTypeUnknown (fromIntegral v)

-- | Symbol visibility
data SymbolVisibility
   = SymbolVisibilityDefault
   | SymbolVisibilityInternal
   | SymbolVisibilityHidden
   | SymbolVisibilityProtected
   deriving (Show)

instance Enum SymbolVisibility where
   fromEnum x = case x of
      SymbolVisibilityDefault    -> 0
      SymbolVisibilityInternal   -> 1
      SymbolVisibilityHidden     -> 2
      SymbolVisibilityProtected  -> 3

   toEnum x = case x of
      0 -> SymbolVisibilityDefault
      1 -> SymbolVisibilityInternal
      2 -> SymbolVisibilityHidden
      3 -> SymbolVisibilityProtected
      v -> error $ "Invalid symbol visibility: " ++ show v

-- | Symbol information
--
-- In the original semantics, symbol's "section" field can be used to encode
-- other information and "info" field is taken for binding/type... We fix this
-- by using "Info" for "section" and Binding/Type for "info".
data SymbolInfo
   = SymbolInfoUndefined             -- ^ Undefined section
   | SymbolInfoAbsolute              -- ^ Associated symbol is absolute
   | SymbolInfoCommon                -- ^ Associated symbol is common
   | SymbolInfoIndexInExtraTable     -- ^ Index is in extra table
   | SymbolInfoSectionBeforeAll      -- ^ Order section before all others (Solaris)
   | SymbolInfoSectionAfterAll       -- ^ Order section after all others (Solaris)
   | SymbolInfoSectionIndex Word16   -- ^ Section index
   | SymbolInfoUnknown Word16        -- ^ Unknown information
   deriving (Show)

instance Enum SymbolInfo where
   fromEnum x = case x of
      SymbolInfoUndefined           -> 0
      SymbolInfoAbsolute            -> 0xfff1
      SymbolInfoCommon              -> 0xfff2
      SymbolInfoIndexInExtraTable   -> 0xffff
      SymbolInfoSectionBeforeAll    -> 0xff00
      SymbolInfoSectionAfterAll     -> 0xff01
      SymbolInfoSectionIndex v      -> fromIntegral v
      SymbolInfoUnknown v           -> fromIntegral v

   toEnum x = case x of
      0      -> SymbolInfoUndefined
      0xfff1 -> SymbolInfoAbsolute
      0xfff2 -> SymbolInfoCommon
      0xffff -> SymbolInfoIndexInExtraTable
      0xff00 -> SymbolInfoSectionBeforeAll
      0xff01 -> SymbolInfoSectionAfterAll
      v 
         | v < 0xff00 -> SymbolInfoSectionIndex (fromIntegral v)
         | otherwise  -> SymbolInfoUnknown (fromIntegral v)

-- | Getter for a symbol entry
getSymbolEntry :: PreHeader -> Get SymbolEntry
getSymbolEntry i = do
   let (gw8,gw16,gw32,_,gwN) = getGetters i
   
   (name,value,size,info,other,sec) <- case preHeaderWordSize i of
      WordSize32 -> do
         name  <- gw32
         value <- gwN
         size  <- gwN
         info  <- gw8
         other <- gw8
         sec   <- gw16
         return (name,value,size,info,other,sec)

      WordSize64 -> do
         name  <- gw32
         info  <- gw8
         other <- gw8
         sec   <- gw16
         value <- gwN
         size  <- gwN
         return (name,value,size,info,other,sec)
   let 
      typ  = toEnum (fromIntegral $ info .&. 0x0f)
      bind = toEnum (fromIntegral $ info `shiftR` 4)
      visi = toEnum (fromIntegral $ other .&. 0x03)
      ifo  = toEnum (fromIntegral sec)
   return (SymbolEntry name bind typ visi ifo value size)

-- | Putter for a symbol entry
putSymbolEntry :: PreHeader -> SymbolEntry -> Put
putSymbolEntry i (SymbolEntry name bind typ visi ifo value size) = do
   let 
      (pw8,pw16,pw32,_,pwN) = getPutters i
      info = (fromIntegral (fromEnum bind) `shiftL` 4) 
         .|. (fromIntegral (fromEnum typ) .&. 0x0f)
      other = fromIntegral (fromEnum visi) .&. 0x03
      sec = fromIntegral (fromEnum ifo)
   
   case preHeaderWordSize i of
      WordSize32 -> do
         pw32 name
         pwN value
         pwN size
         pw8 info
         pw8 other
         pw16 sec

      WordSize64 -> do
         pw32 name
         pw8 info
         pw8 other
         pw16 sec
         pwN value
         pwN size

