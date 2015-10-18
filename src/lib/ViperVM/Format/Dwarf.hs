{-# LANGUAGE LambdaCase, TupleSections #-}
module ViperVM.Format.Dwarf
   ( Entry (..)
   , Tag (..)
   , fromTag
   , toTag
   , Attribute (..)
   , fromAttribute
   , toAttribute
   , Form (..)
   , toForm
   , fromForm
   , DwarfFormat (..)
   , getFormat
   , putFormat
   , getUnitLength
   , putUnitLength
   , CompilationUnitHeader (..)
   , getCompilationUnitHeader
   , putCompilationUnitHeader
   , DebugInfo (..)
   , getDebugInfo
   )
where

import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)

import ViperVM.Format.Binary.Endianness

-- DWARF 4
-- =======
--
-- DWARF uses a series of debugging information entries (DIEs) to define a
-- low-level representation of a source program. Each DIE consists of an
-- identifying tag and a series of attributes.
--
-- The DIEs are contained in the .debug_info and .debug_types sections of an
-- object file.
--
-- Each attribute value is characterized by an attribute name. No more than one
-- attribute with a given name may appear in a DIE.


-- | A debugging information entry (DIE)
data Entry = Entry
   { entryTag        :: Tag           -- ^ Entry tag
   , entryAttributes :: [Attribute]   -- ^ Entry attributes
   }
   deriving (Show,Eq)

-- | Tag names
data Tag
   = TagAccessDeclaration
   | TagArrayType
   | TagBaseType
   | TagCatchBlock
   | TagClassType
   | TagCommonblock
   | TagCommonInclusion
   | TagCompileUnit
   | TagCondition
   | TagConstType
   | TagConstant
   | TagDwarfProcedure
   | TagEntryPoint
   | TagEnumerationType
   | TagEnumerator
   | TagFileType
   | TagFormalParameter
   | TagFriend
   | TagImportedDeclaration
   | TagImportedModule
   | TagImportedUnit
   | TagInheritance
   | TagInlinedSubroutine
   | TagInterfaceType
   | TagLabel
   | TagLexicalBlock
   | TagMember
   | TagModule
   | TagNamelist
   | TagNamelistItem
   | TagNamespace
   | TagPackedType
   | TagPartialUnit
   | TagPointerType
   | TagPointerToMemberType
   | TagReferenceType
   | TagRestrictType
   | TagRvalueReferenceType
   | TagSetType
   | TagSharedType
   | TagStringType
   | TagStructureType
   | TagSubprogram
   | TagSubrangeType
   | TagSubroutineType
   | TagTemplateAlias
   | TagTemplateTypeParameter
   | TagTemplateValueParameter
   | TagThrownType
   | TagTryBlock
   | TagTypedef
   | TagTypeUnit
   | TagUnionType
   | TagUnspecifiedParameters
   | TagUnspecifiedType
   | TagVariable
   | TagVariant
   | TagVariantPart
   | TagVolatileType
   | TagWithStatement
   | TagCustom Word16
   deriving (Show,Eq)

fromTag :: Tag -> Word16
fromTag x = case x of
   TagAccessDeclaration       -> 0x23
   TagArrayType               -> 0x01
   TagBaseType                -> 0x24
   TagCatchBlock              -> 0x25
   TagClassType               -> 0x02
   TagCommonblock             -> 0x1a
   TagCommonInclusion         -> 0x1b
   TagCompileUnit             -> 0x11
   TagCondition               -> 0x3f
   TagConstType               -> 0x26
   TagConstant                -> 0x27
   TagDwarfProcedure          -> 0x36
   TagEntryPoint              -> 0x03
   TagEnumerationType         -> 0x04
   TagEnumerator              -> 0x28
   TagFileType                -> 0x29
   TagFormalParameter         -> 0x05
   TagFriend                  -> 0x2a
   TagImportedDeclaration     -> 0x08
   TagImportedModule          -> 0x3a
   TagImportedUnit            -> 0x3d
   TagInheritance             -> 0x1c
   TagInlinedSubroutine       -> 0x1d
   TagInterfaceType           -> 0x38
   TagLabel                   -> 0x0a
   TagLexicalBlock            -> 0x0b
   TagMember                  -> 0x0d
   TagModule                  -> 0x1e
   TagNamelist                -> 0x2b
   TagNamelistItem            -> 0x2c
   TagNamespace               -> 0x39
   TagPackedType              -> 0x2d
   TagPartialUnit             -> 0x3c
   TagPointerType             -> 0x0f
   TagPointerToMemberType     -> 0x1f
   TagReferenceType           -> 0x10
   TagRestrictType            -> 0x37
   TagRvalueReferenceType     -> 0x42
   TagSetType                 -> 0x20
   TagSharedType              -> 0x40
   TagStringType              -> 0x12
   TagStructureType           -> 0x13
   TagSubprogram              -> 0x2e
   TagSubrangeType            -> 0x21
   TagSubroutineType          -> 0x15
   TagTemplateAlias           -> 0x43
   TagTemplateTypeParameter   -> 0x2f
   TagTemplateValueParameter  -> 0x30
   TagThrownType              -> 0x31
   TagTryBlock                -> 0x32
   TagTypedef                 -> 0x16
   TagTypeUnit                -> 0x41
   TagUnionType               -> 0x17
   TagUnspecifiedParameters   -> 0x18
   TagUnspecifiedType         -> 0x3b
   TagVariable                -> 0x34
   TagVariant                 -> 0x19
   TagVariantPart             -> 0x33
   TagVolatileType            -> 0x35
   TagWithStatement           -> 0x22
   TagCustom v                -> v

toTag :: Word16 -> Tag
toTag x = case x of
   0x23   -> TagAccessDeclaration
   0x01   -> TagArrayType
   0x24   -> TagBaseType
   0x25   -> TagCatchBlock
   0x02   -> TagClassType
   0x1a   -> TagCommonblock
   0x1b   -> TagCommonInclusion
   0x11   -> TagCompileUnit
   0x3f   -> TagCondition
   0x26   -> TagConstType
   0x27   -> TagConstant
   0x36   -> TagDwarfProcedure
   0x03   -> TagEntryPoint
   0x04   -> TagEnumerationType
   0x28   -> TagEnumerator
   0x29   -> TagFileType
   0x05   -> TagFormalParameter
   0x2a   -> TagFriend
   0x08   -> TagImportedDeclaration
   0x3a   -> TagImportedModule
   0x3d   -> TagImportedUnit
   0x1c   -> TagInheritance
   0x1d   -> TagInlinedSubroutine
   0x38   -> TagInterfaceType
   0x0a   -> TagLabel
   0x0b   -> TagLexicalBlock
   0x0d   -> TagMember
   0x1e   -> TagModule
   0x2b   -> TagNamelist
   0x2c   -> TagNamelistItem
   0x39   -> TagNamespace
   0x2d   -> TagPackedType
   0x3c   -> TagPartialUnit
   0x0f   -> TagPointerType
   0x1f   -> TagPointerToMemberType
   0x10   -> TagReferenceType
   0x37   -> TagRestrictType
   0x42   -> TagRvalueReferenceType
   0x20   -> TagSetType
   0x40   -> TagSharedType
   0x12   -> TagStringType
   0x13   -> TagStructureType
   0x2e   -> TagSubprogram
   0x21   -> TagSubrangeType
   0x15   -> TagSubroutineType
   0x43   -> TagTemplateAlias
   0x2f   -> TagTemplateTypeParameter
   0x30   -> TagTemplateValueParameter
   0x31   -> TagThrownType
   0x32   -> TagTryBlock
   0x16   -> TagTypedef
   0x41   -> TagTypeUnit
   0x17   -> TagUnionType
   0x18   -> TagUnspecifiedParameters
   0x3b   -> TagUnspecifiedType
   0x34   -> TagVariable
   0x19   -> TagVariant
   0x33   -> TagVariantPart
   0x35   -> TagVolatileType
   0x22   -> TagWithStatement
   v      -> TagCustom v



data Attribute
   = AttrAbstractOrigin
   | AttrAccessibility
   | AttrAddressClass
   | AttrAllocated
   | AttrArtificial
   | AttrAssociated
   | AttrBaseTypes
   | AttrBinaryScale
   | AttrBitOffset
   | AttrBitSize
   | AttrBitStride
   | AttrByteSize
   | AttrByteStride
   | AttrCallColumn
   | AttrCallFile
   | AttrCallLine
   | AttrCallingConvention
   | AttrCommonReference
   | AttrCompilationDirectory
   | AttrConstantValue
   | AttrConstantExpr
   | AttrContainingType
   | AttrCount
   | AttrDataBitOffset
   | AttrDataLocation
   | AttrDataMemberLocation
   | AttrDecimalScale
   | AttrDecimalSign
   | AttrDeclarationColumn
   | AttrDeclarationFile
   | AttrDeclarationLine
   | AttrDeclaration
   | AttrDefaultValue
   | AttrDescription
   | AttrDigitCount
   | AttrDiscriminant
   | AttrDiscriminantList
   | AttrDiscriminantValue
   | AttrElemental
   | AttrEncoding
   | AttrEndianity
   | AttrEntryPC
   | AttrEnumClass
   | AttrExplicit
   | AttrExtension
   | AttrExternal
   | AttrFrameBase
   | AttrFriend
   | AttrHighPC
   | AttrIdentifierCase
   | AttrImport
   | AttrInline
   | AttrIsOptional
   | AttrLanguage
   | AttrLinkageName
   | AttrLocation
   | AttrLowPC
   | AttrLowerBound
   | AttrMacroInfo
   | AttrMainSubprogram
   | AttrMutable
   | AttrName
   | AttrNamelistItem
   | AttrObjectPointer
   | AttrOrdering
   | AttrPictureString
   | AttrPriority
   | AttrProducer
   | AttrPrototyped
   | AttrPure
   | AttrRanges
   | AttrRecursive
   | AttrReturnAddress
   | AttrSegment
   | AttrSibling
   | AttrSmall
   | AttrSignature
   | AttrSpecification
   | AttrStartScope
   | AttrStaticLink
   | AttrStatementList
   | AttrStringLength
   | AttrThreadsScaled
   | AttrTrampoline
   | AttrType
   | AttrUpperBound
   | AttrUseLocation
   | AttrUseUtf8
   | AttrVariableParameter
   | AttrVirtuality
   | AttrVisibility
   | AttrVTableElemLocation
   | AttrCustom Word16
   deriving (Show,Eq)

fromAttribute :: Attribute -> Word16
fromAttribute x = case x of
   AttrAbstractOrigin            -> 0x31
   AttrAccessibility             -> 0x32
   AttrAddressClass              -> 0x33
   AttrAllocated                 -> 0x4e
   AttrArtificial                -> 0x34
   AttrAssociated                -> 0x4f
   AttrBaseTypes                 -> 0x35
   AttrBinaryScale               -> 0x5b
   AttrBitOffset                 -> 0x0c
   AttrBitSize                   -> 0x0d
   AttrBitStride                 -> 0x2e
   AttrByteSize                  -> 0x0b
   AttrByteStride                -> 0x51
   AttrCallColumn                -> 0x57
   AttrCallFile                  -> 0x58
   AttrCallLine                  -> 0x59
   AttrCallingConvention         -> 0x36
   AttrCommonReference           -> 0x1a
   AttrCompilationDirectory      -> 0x1b
   AttrConstantValue             -> 0x1c
   AttrConstantExpr              -> 0x6c
   AttrContainingType            -> 0x1d
   AttrCount                     -> 0x37
   AttrDataBitOffset             -> 0x6b
   AttrDataLocation              -> 0x50
   AttrDataMemberLocation        -> 0x38
   AttrDecimalScale              -> 0x5c
   AttrDecimalSign               -> 0x5e
   AttrDeclarationColumn         -> 0x39
   AttrDeclarationFile           -> 0x3a
   AttrDeclarationLine           -> 0x3b
   AttrDeclaration               -> 0x3c
   AttrDefaultValue              -> 0x1e
   AttrDescription               -> 0x5a
   AttrDigitCount                -> 0x5f
   AttrDiscriminant              -> 0x15
   AttrDiscriminantList          -> 0x3d
   AttrDiscriminantValue         -> 0x16
   AttrElemental                 -> 0x66
   AttrEncoding                  -> 0x3e
   AttrEndianity                 -> 0x65
   AttrEntryPC                   -> 0x52
   AttrEnumClass                 -> 0x6d
   AttrExplicit                  -> 0x63
   AttrExtension                 -> 0x54
   AttrExternal                  -> 0x3f
   AttrFrameBase                 -> 0x40
   AttrFriend                    -> 0x41
   AttrHighPC                    -> 0x12
   AttrIdentifierCase            -> 0x42
   AttrImport                    -> 0x18
   AttrInline                    -> 0x20
   AttrIsOptional                -> 0x21
   AttrLanguage                  -> 0x13
   AttrLinkageName               -> 0x6e
   AttrLocation                  -> 0x02
   AttrLowPC                     -> 0x11
   AttrLowerBound                -> 0x22
   AttrMacroInfo                 -> 0x43
   AttrMainSubprogram            -> 0x6a
   AttrMutable                   -> 0x61
   AttrName                      -> 0x03
   AttrNamelistItem              -> 0x44
   AttrObjectPointer             -> 0x64
   AttrOrdering                  -> 0x09
   AttrPictureString             -> 0x60
   AttrPriority                  -> 0x45
   AttrProducer                  -> 0x25
   AttrPrototyped                -> 0x27
   AttrPure                      -> 0x67
   AttrRanges                    -> 0x55
   AttrRecursive                 -> 0x68
   AttrReturnAddress             -> 0x2a
   AttrSegment                   -> 0x46
   AttrSibling                   -> 0x01
   AttrSmall                     -> 0x5d
   AttrSignature                 -> 0x69
   AttrSpecification             -> 0x47
   AttrStartScope                -> 0x2c
   AttrStaticLink                -> 0x48
   AttrStatementList             -> 0x10
   AttrStringLength              -> 0x19
   AttrThreadsScaled             -> 0x62
   AttrTrampoline                -> 0x56
   AttrType                      -> 0x49
   AttrUpperBound                -> 0x2f
   AttrUseLocation               -> 0x4a
   AttrUseUtf8                   -> 0x53
   AttrVariableParameter         -> 0x4b
   AttrVirtuality                -> 0x4c
   AttrVisibility                -> 0x17
   AttrVTableElemLocation        -> 0x4d
   AttrCustom v                  -> v

toAttribute :: Word16 -> Attribute
toAttribute x = case x of
   0x31  -> AttrAbstractOrigin
   0x32  -> AttrAccessibility
   0x33  -> AttrAddressClass
   0x4e  -> AttrAllocated
   0x34  -> AttrArtificial
   0x4f  -> AttrAssociated
   0x35  -> AttrBaseTypes
   0x5b  -> AttrBinaryScale
   0x0c  -> AttrBitOffset
   0x0d  -> AttrBitSize
   0x2e  -> AttrBitStride
   0x0b  -> AttrByteSize
   0x51  -> AttrByteStride
   0x57  -> AttrCallColumn
   0x58  -> AttrCallFile
   0x59  -> AttrCallLine
   0x36  -> AttrCallingConvention
   0x1a  -> AttrCommonReference
   0x1b  -> AttrCompilationDirectory
   0x1c  -> AttrConstantValue
   0x6c  -> AttrConstantExpr
   0x1d  -> AttrContainingType
   0x37  -> AttrCount
   0x6b  -> AttrDataBitOffset
   0x50  -> AttrDataLocation
   0x38  -> AttrDataMemberLocation
   0x5c  -> AttrDecimalScale
   0x5e  -> AttrDecimalSign
   0x39  -> AttrDeclarationColumn
   0x3a  -> AttrDeclarationFile
   0x3b  -> AttrDeclarationLine
   0x3c  -> AttrDeclaration
   0x1e  -> AttrDefaultValue
   0x5a  -> AttrDescription
   0x5f  -> AttrDigitCount
   0x15  -> AttrDiscriminant
   0x3d  -> AttrDiscriminantList
   0x16  -> AttrDiscriminantValue
   0x66  -> AttrElemental
   0x3e  -> AttrEncoding
   0x65  -> AttrEndianity
   0x52  -> AttrEntryPC
   0x6d  -> AttrEnumClass
   0x63  -> AttrExplicit
   0x54  -> AttrExtension
   0x3f  -> AttrExternal
   0x40  -> AttrFrameBase
   0x41  -> AttrFriend
   0x12  -> AttrHighPC
   0x42  -> AttrIdentifierCase
   0x18  -> AttrImport
   0x20  -> AttrInline
   0x21  -> AttrIsOptional
   0x13  -> AttrLanguage
   0x6e  -> AttrLinkageName
   0x02  -> AttrLocation
   0x11  -> AttrLowPC
   0x22  -> AttrLowerBound
   0x43  -> AttrMacroInfo
   0x6a  -> AttrMainSubprogram
   0x61  -> AttrMutable
   0x03  -> AttrName
   0x44  -> AttrNamelistItem
   0x64  -> AttrObjectPointer
   0x09  -> AttrOrdering
   0x60  -> AttrPictureString
   0x45  -> AttrPriority
   0x25  -> AttrProducer
   0x27  -> AttrPrototyped
   0x67  -> AttrPure
   0x55  -> AttrRanges
   0x68  -> AttrRecursive
   0x2a  -> AttrReturnAddress
   0x46  -> AttrSegment
   0x01  -> AttrSibling
   0x5d  -> AttrSmall
   0x69  -> AttrSignature
   0x47  -> AttrSpecification
   0x2c  -> AttrStartScope
   0x48  -> AttrStaticLink
   0x10  -> AttrStatementList
   0x19  -> AttrStringLength
   0x62  -> AttrThreadsScaled
   0x56  -> AttrTrampoline
   0x49  -> AttrType
   0x2f  -> AttrUpperBound
   0x4a  -> AttrUseLocation
   0x53  -> AttrUseUtf8
   0x4b  -> AttrVariableParameter
   0x4c  -> AttrVirtuality
   0x17  -> AttrVisibility
   0x4d  -> AttrVTableElemLocation
   v     -> AttrCustom v


data Form
   = FormAddress
   | FormBlock2
   | FormBlock4
   | FormData2
   | FormData4
   | FormData8
   | FormString
   | FormBlock
   | FormBlock1
   | FormData1
   | FormFlag
   | FormSData
   | FormStrp
   | FormUData
   | FormRefAddress
   | FormRef1
   | FormRef2
   | FormRef4
   | FormRef8
   | FormRefUData
   | FormIndirect
   | FormSecOffset
   | FormExprLoc
   | FormFlagPresent
   | FormRefSig8
   deriving (Show,Eq)

fromForm :: Form -> Word16
fromForm x = case x of
   FormAddress       -> 0x01
   FormBlock2        -> 0x03
   FormBlock4        -> 0x04
   FormData2         -> 0x05
   FormData4         -> 0x06
   FormData8         -> 0x07
   FormString        -> 0x08
   FormBlock         -> 0x09
   FormBlock1        -> 0x0a
   FormData1         -> 0x0b
   FormFlag          -> 0x0c
   FormSData         -> 0x0d
   FormStrp          -> 0x0e
   FormUData         -> 0x0f
   FormRefAddress    -> 0x10
   FormRef1          -> 0x11
   FormRef2          -> 0x12
   FormRef4          -> 0x13
   FormRef8          -> 0x14
   FormRefUData      -> 0x15
   FormIndirect      -> 0x16
   FormSecOffset     -> 0x17
   FormExprLoc       -> 0x18
   FormFlagPresent   -> 0x19
   FormRefSig8       -> 0x20

toForm :: Word16 -> Form
toForm x = case x of
   0x01  -> FormAddress
   0x03  -> FormBlock2
   0x04  -> FormBlock4
   0x05  -> FormData2
   0x06  -> FormData4
   0x07  -> FormData8
   0x08  -> FormString
   0x09  -> FormBlock
   0x0a  -> FormBlock1
   0x0b  -> FormData1
   0x0c  -> FormFlag
   0x0d  -> FormSData
   0x0e  -> FormStrp
   0x0f  -> FormUData
   0x10  -> FormRefAddress
   0x11  -> FormRef1
   0x12  -> FormRef2
   0x13  -> FormRef4
   0x14  -> FormRef8
   0x15  -> FormRefUData
   0x16  -> FormIndirect
   0x17  -> FormSecOffset
   0x18  -> FormExprLoc
   0x19  -> FormFlagPresent
   0x20  -> FormRefSig8
   _     -> error "Unknown form entry"


-- Attribute classes
--data ClassAddress          = ClassAddress
--data ClassBlock            = ClassBlock
--data ClassConstant         = ClassConstant
--data ClassExprLoc          = ClassExprLoc
--data ClassFlag             = ClassFlag
--data ClassLinePointer      = ClassLinePointer
--data ClassLocListPointer   = ClassLocListPointer
--data ClassMacroPointer     = ClassMacroPointer
--data ClassRangeListPointer = ClassRangeListPointer
--data ClassReference        = ClassReference
--data ClassString           = ClassString
--
--TODO


-- DWARF expressions
--
-- DWARF expressions describe how to compute a value or name a location during
-- debugging of a program. They are expressed in terms of DWARF operations that
-- operate on a stack of values.  
--
-- All DWARF operations are encoded as a stream of opcodes that are each
-- followed by zero or more literal operands. The number of operands is
-- determined by the opcode.
--
-- TODO


data DwarfFormat
   = Dwarf32 
   | Dwarf64
   deriving (Show,Eq)

-- | Compilation unit header
data CompilationUnitHeader = CompilationUnitHeader
   { cuhEndianness   :: Endianness     -- ^ The endianness is not part of the
                                       -- header (it is given by the ELF header 
                                       -- for instance) but we store it here for
                                       -- convenience
   , cuhDwarfFormat  :: DwarfFormat
   , cuhUnitLength   :: Word64
   , cuhVersion      :: Word16
   , cuhAbbrevOffset :: Word64
   , cuhAddressSize  :: Word8
   }
   deriving (Show,Eq)

-- | Return getters
getGetters :: Endianness -> DwarfFormat -> (Get Word8, Get Word16, Get Word32, Get Word64, Get Word64)
getGetters endian format = (gw8,gw16,gw32,gw64,gwN)
   where
      ExtendedWordGetters gw8 gw16 gw32 gw64 gwN = getExtendedWordGetters endian ws
      ws = case format of
         Dwarf64 -> WordSize64
         Dwarf32 -> WordSize32

-- | Return putters
getPutters :: Endianness -> DwarfFormat -> (Word8 -> Put, Word16 -> Put, Word32 -> Put, Word64 -> Put, Word64 -> Put)
getPutters endian format = (pw8,pw16,pw32,pw64,pwN)
   where
      ExtendedWordPutters pw8 pw16 pw32 pw64 pwN = getExtendedWordPutters endian ws
      ws = case format of
         Dwarf64 -> WordSize64
         Dwarf32 -> WordSize32

getFormat :: Endianness -> Get DwarfFormat
getFormat endian = do
   let WordGetters _ _ gw32 _ = getWordGetters endian
   lookAhead gw32 >>= \case
      0xffffffff         -> skip 4 >> return Dwarf64
      l | l < 0xfffffff0 -> return Dwarf32
        | otherwise      -> error $ "Invalid unit length ("++show l++")"

putFormat :: Endianness -> DwarfFormat -> Put
putFormat endian format = do
   let WordPutters _ _ pw32 _  = getWordPutters endian
   case format of
      Dwarf64 -> pw32 0xffffffff
      Dwarf32 -> return ()

getUnitLength :: Endianness -> Get (DwarfFormat,Word64)
getUnitLength endian = do
   format <- getFormat endian
   let (_,_,_,_,gwN) = getGetters endian format
   len <- gwN
   return (format,len)

putUnitLength :: Endianness -> DwarfFormat -> Word64 -> Put
putUnitLength endian format len = do
   putFormat endian format
   let (_,_,_,_,pwN) = getPutters endian format
   -- check and store unit length
   case format of
      Dwarf32 
         | len > 0xfffffff0 -> error $ "Invalid unit length in 32-bit format ("++show len++")"
      _                     -> pwN len
   

getCompilationUnitHeader :: Endianness -> Get CompilationUnitHeader
getCompilationUnitHeader endian = do
   (format,len) <- getUnitLength endian
   let (gw8,gw16,_,_,gwN) = getGetters endian format
   
   CompilationUnitHeader
      endian
      format
      len
      <$> gw16
      <*> gwN
      <*> gw8

putCompilationUnitHeader :: CompilationUnitHeader -> Put
putCompilationUnitHeader cuh = do
   let (pw8,pw16,_,_,pwN) = getPutters (cuhEndianness cuh) (cuhDwarfFormat cuh)

   putUnitLength (cuhEndianness cuh) (cuhDwarfFormat cuh) (cuhUnitLength cuh)
   pw16 (cuhVersion cuh)
   pwN  (cuhAbbrevOffset cuh)
   pw8  (cuhAddressSize cuh)


data DebugInfo = DebugInfo
   { debugInfoCompilationUnitHeader :: CompilationUnitHeader
   , debugInfoContent               :: ByteString
   }
   deriving (Show)

getDebugInfo :: Endianness -> Get DebugInfo
getDebugInfo endian = do
   cuh <- getCompilationUnitHeader endian
   -- the length in the header excludes only itself
   let len = case cuhDwarfFormat cuh of
            Dwarf32 -> fromIntegral (cuhUnitLength cuh) - 7
            Dwarf64 -> fromIntegral (cuhUnitLength cuh) - 11
   bs <- getByteString len
   return $ DebugInfo cuh bs
