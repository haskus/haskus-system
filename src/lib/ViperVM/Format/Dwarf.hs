{-# LANGUAGE LambdaCase, TupleSections #-}
module ViperVM.Format.Dwarf
   ( Entry (..)
   , DwarfFormat (..)
   , getFormat
   , putFormat
   , getUnitLength
   , putUnitLength
   -- * Attributes
   , Attribute (..)
   , fromAttribute
   , toAttribute
   , Form (..)
   , toForm
   , fromForm
   , Tag (..)
   , fromTag
   , toTag
   , Encoding (..)
   , toEncoding
   , fromEncoding
   , Endianity (..)
   , toEndianity
   , fromEndianity
   , DecimalSign (..)
   , toDecimalSign
   , fromDecimalSign
   , Accessibility (..)
   , toAccessibility
   , fromAccessibility
   , Visibility (..)
   , toVisibility
   , fromVisibility
   , Virtuality (..)
   , toVirtuality
   , fromVirtuality
   , Language (..)
   , toLanguage
   , fromLanguage
   , CaseSensitivity (..)
   , toCaseSensitivity
   , fromCaseSensitivity
   , CallingConvention (..)
   , toCallingConvention
   , fromCallingConvention
   , Inlining (..)
   , toInlining
   , fromInlining
   , ArrayOrdering (..)
   , toArrayOrdering
   , fromArrayOrdering
   -- * Debug entry
   , DebugEntry (..)
   , DebugAttribute (..)
   , getDebugEntry
   , getDebugEntries
   , debugEntryTree
   -- * Debug info
   , CompilationUnitHeader (..)
   , getCompilationUnitHeader
   , putCompilationUnitHeader
   , DebugInfo (..)
   , getDebugInfo
   -- * Debug type
   , TypeUnitHeader (..)
   , getTypeUnitHeader
   , putTypeUnitHeader
   , DebugType (..)
   , getDebugType
   -- * Abbreviations
   , DebugAbbrevEntry (..)
   , DebugAbbrevAttribute (..)
   , getDebugAbbrevEntry
   , getDebugAbbrevEntries
   )
where

import Data.Word
import Data.Int
import Data.Tree (Tree(..))
import ViperVM.Format.Binary.Get
import ViperVM.Format.Binary.Put
import Data.ByteString (ByteString)
import Data.Maybe (fromJust,isJust)

import Control.Monad (forM)

import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as BS

import ViperVM.Format.Binary.Endianness
import ViperVM.Format.Binary.VariableLength

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
   | FormStringPointer
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
   FormStringPointer -> 0x0e
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
   0x0e  -> FormStringPointer
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
data DwarfExpr
   = ExprAddr Word64
   | ExprDeref
   | ExprUnsignedConstant Word64
   | ExprSignedConstant   Int64
   | ExprDup
   | ExprDrop
   | ExprOver
   | ExprPick Word8
   | ExprSwap
   | ExprRot
   | ExprXDeref
   | ExprAbs
   | ExprAnd
   | ExprDiv
   | ExprMinus
   | ExprMod
   | ExprMul
   | ExprNegate
   | ExprNot
   | ExprOr
   | ExprPlus
   | ExprPlusUnsignedConstant Word64
   | ExprShiftLeft
   | ExprShiftRight
   | ExprShiftRightA
   | ExprXor
   | ExprSkip Int16
   | ExprBra Int16
   | ExprEq
   | ExprGE
   | ExprGT
   | ExprLE
   | ExprLT
   | ExprNE
   | ExprLiteral Word8
   | ExprRegister Word8
   | ExprBaseRegister Word64
   | ExprExtReg Word64
   | ExprFBReg Int64
   | ExprExtBaseReg Word64 Int64
   | ExprPiece Word64
   | ExprDerefSize Word8
   | ExprExtDerefSize Word8
   | ExprNop
   | ExprPushObjectAddress
   | ExprCall Word64
   | ExprCallRef Word64
   | ExprFormTLSAddress
   | ExprCallFrameCFA
   | ExprBitPiece Word64 Word64
   | ExprImplicitValue Word64 ByteString
   | ExprStackValue
   | ExprCustom Word8
   deriving (Show)

getDwarfExpr :: Endianness -> DwarfFormat -> Get DwarfExpr
getDwarfExpr endian format = do

   let (gw8,gw16,gw32,gw64,gwN) = getGetters endian format
   code <- gw8
   case code of
      0x03 -> ExprAddr <$> gwN
      0x06 -> return ExprDeref
      0x08 -> ExprUnsignedConstant . fromIntegral <$> gw8
      0x09 -> ExprSignedConstant   . fromIntegral <$> (fromIntegral <$> gw8 :: Get Int8)
      0x0a -> ExprUnsignedConstant . fromIntegral <$> gw16
      0x0b -> ExprSignedConstant   . fromIntegral <$> (fromIntegral <$> gw16 :: Get Int16)
      0x0c -> ExprUnsignedConstant . fromIntegral <$> gw32
      0x0d -> ExprSignedConstant   . fromIntegral <$> (fromIntegral <$> gw32 :: Get Int32)
      0x0e -> ExprUnsignedConstant . fromIntegral <$> gw64
      0x0f -> ExprSignedConstant   . fromIntegral <$> (fromIntegral <$> gw64 :: Get Int64)
      0x10 -> ExprUnsignedConstant                <$> getULEB128
      0x11 -> ExprSignedConstant                  <$> getSLEB128
      0x12 -> return ExprDup
      0x13 -> return ExprDrop
      0x14 -> return ExprOver
      0x15 -> ExprPick <$> gw8
      0x16 -> return ExprSwap
      0x17 -> return ExprRot
      0x18 -> return ExprXDeref
      0x19 -> return ExprAbs
      0x1a -> return ExprAnd
      0x1b -> return ExprDiv
      0x1c -> return ExprMinus
      0x1d -> return ExprMod
      0x1e -> return ExprMul
      0x1f -> return ExprNegate
      0x20 -> return ExprNot
      0x21 -> return ExprOr
      0x22 -> return ExprPlus
      0x23 -> ExprPlusUnsignedConstant <$> getULEB128
      0x24 -> return ExprShiftLeft
      0x25 -> return ExprShiftRight
      0x26 -> return ExprShiftRightA
      0x27 -> return ExprXor
      0x28 -> ExprSkip <$> (fromIntegral <$> gw16 :: Get Int16)
      0x29 -> ExprBra  <$> (fromIntegral <$> gw16 :: Get Int16)
      0x2a -> return ExprEq
      0x2b -> return ExprGE
      0x2c -> return ExprGT
      0x2d -> return ExprLE
      0x2e -> return ExprLT
      0x2f -> return ExprNE
      x
         | x >= 0x30 && x <= 0x4f -> return $ ExprLiteral (fromIntegral x - 0x30)
         | x >= 0x50 && x <= 0x6f -> return $ ExprRegister (fromIntegral x - 0x50)
         | x >= 0x70 && x <= 0x8f -> ExprBaseRegister <$> getSLEB128
      0x90 -> ExprExtReg <$> getULEB128
      0x91 -> ExprFBReg  <$> getSLEB128
      0x92 -> ExprExtBaseReg <$> getULEB128 <*> getSLEB128
      0x93 -> ExprPiece <$> getULEB128
      0x94 -> ExprDerefSize <$> gw8
      0x95 -> ExprExtDerefSize <$> gw8
      0x96 -> return ExprNop
      0x97 -> return ExprPushObjectAddress
      0x98 -> ExprCall . fromIntegral <$> gw16
      0x99 -> ExprCall . fromIntegral <$> gw32
      0x9a -> ExprCallRef <$> gwN
      0x9b -> return ExprFormTLSAddress
      0x9c -> return ExprCallFrameCFA
      0x9d -> ExprBitPiece <$> getULEB128 <*> getULEB128
      0x9e -> do
         sz <- getULEB128 :: Get Word64
         ExprImplicitValue sz <$> getByteString (fromIntegral sz)
      0x9f -> return ExprStackValue
      x    -> return $ ExprCustom (fromIntegral x)


-- | DWARF format (32-bit or 64-bit)
data DwarfFormat
   = Dwarf32 
   | Dwarf64
   deriving (Show,Eq)

-- | Compilation unit header
data CompilationUnitHeader = CompilationUnitHeader
   { cuhDwarfFormat     :: DwarfFormat
   , cuhUnitLength      :: Word64
   , cuhVersion         :: Word16
   , cuhAbbrevOffset    :: Word64
   , cuhAddressSize     :: Word8
   }
   deriving (Show,Eq)

data TypeUnitHeader = TypeUnitHeader
   { tuhDwarfFormat     :: DwarfFormat
   , tuhUnitLength      :: Word64
   , tuhVersion         :: Word16
   , tuhAbbrevOffset    :: Word64
   , tuhAddressSize     :: Word8
   , tuhTypeSignature   :: Word64
   , tuhTypeOffset      :: Word64
   }
   deriving(Show,Eq)

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
      format
      len
      <$> gw16
      <*> gwN
      <*> gw8

putCompilationUnitHeader :: Endianness -> CompilationUnitHeader -> Put
putCompilationUnitHeader endian cuh = do
   let (pw8,pw16,_,_,pwN) = getPutters endian (cuhDwarfFormat cuh)

   putUnitLength endian (cuhDwarfFormat cuh) (cuhUnitLength cuh)
   pw16 (cuhVersion cuh)
   pwN  (cuhAbbrevOffset cuh)
   pw8  (cuhAddressSize cuh)

getTypeUnitHeader :: Endianness -> Get TypeUnitHeader
getTypeUnitHeader endian = do
   (format,len) <- getUnitLength endian
   let (gw8,gw16,_,gw64,gwN) = getGetters endian format
   
   TypeUnitHeader
      format
      len
      <$> gw16
      <*> gwN
      <*> gw8
      <*> gw64
      <*> gwN

putTypeUnitHeader :: Endianness -> TypeUnitHeader -> Put
putTypeUnitHeader endian tuh = do
   let (pw8,pw16,_,pw64,pwN) = getPutters endian (tuhDwarfFormat tuh)

   putUnitLength endian (tuhDwarfFormat tuh) (tuhUnitLength tuh)
   pw16 (tuhVersion tuh)
   pwN  (tuhAbbrevOffset tuh)
   pw8  (tuhAddressSize tuh)
   pw64 (tuhTypeSignature tuh)
   pwN  (tuhTypeOffset tuh)



data DebugInfo = DebugInfo
   { debugInfoCompilationUnitHeader :: CompilationUnitHeader   -- ^ Header
   , debugInfoEntries               :: [Maybe DebugEntry]      -- ^ Entries (NULL entries are used as siblings group delimiter)
   }
   deriving (Show)

data DebugType = DebugType
   { debugTypeUnitHeader            :: TypeUnitHeader
   , debugTypeContent               :: ByteString
   }
   deriving (Show)


data DebugEntry = DebugEntry
   { debugEntryAbbrevCode     :: Word64
   , debugEntryTag            :: Tag
   , debugEntryHasChildren    :: Bool
   , debugEntryAttributes     :: [DebugAttribute]
   }
   deriving (Show)

data DebugAttribute = DebugAttribute
   { debugAttrName   :: Attribute
   , debugAttrValue  :: AttributeValue
   }
   deriving (Show)

-- | Build a tree from a list of DebugEntry (with Nothing = NULL etnries)
--
-- The first entry must be valid and the top level must not be a forest
debugEntryTree :: [Maybe DebugEntry] -> Tree DebugEntry
debugEntryTree es = case rec es of
      ([x],[]) -> x
      ([],[])  -> error "Cannot make a tree from the Debug entries: there is no entry"
      (_,[])   -> error "Cannot make a tree from the Debug entries: they form a forest"
      (_,_)    -> error "Cannot make a tree from the Debug entries: there are remaining entries"
   where
      rec :: [Maybe DebugEntry] -> ([Tree DebugEntry], [Maybe DebugEntry])
      rec (Just x:xs)
         | debugEntryHasChildren x = let (cs,r) = rec xs 
                                         (ss,r2) = rec r
                                      in (Node x cs : ss, r2)
         | otherwise               = let (cs,r) = rec xs in (Node x [] : cs, r)
      rec (Nothing:xs)             = ([],xs)
      rec []                       = ([],[])



getDebugEntry :: Endianness -> CompilationUnitHeader -> [DebugAbbrevEntry] -> Maybe BS.ByteString -> Get (Maybe DebugEntry)
getDebugEntry endian cuh abbrevs strings = do
   let 
      addressSize = cuhAddressSize cuh
      format      = cuhDwarfFormat cuh

   -- get abbrev code
   code <- getULEB128
   if code == 0
      then return Nothing
      else do
         -- find corresponding abbrev
         -- TODO: use a hash map instead
         let abbrev = head (filter (\x -> debugAbbrevCode x == code) abbrevs)
         -- read attributes values
         attrs <- forM (debugAbbrevAttributes abbrev) $ \att -> do
            value <- getAttributeValue addressSize endian format strings (debugAbbrevAttrName att) (debugAbbrevAttrForm att)
            return $ DebugAttribute (debugAbbrevAttrName att) value
         return . Just $ DebugEntry code 
                           (debugAbbrevTag abbrev) 
                           (debugAbbrevHasChildren abbrev)
                           attrs 

getDebugEntries :: Endianness -> CompilationUnitHeader -> [DebugAbbrevEntry] -> Maybe BS.ByteString -> Get [Maybe DebugEntry]
getDebugEntries endian cuh abbrevs strings = 
   getWhole (getDebugEntry endian cuh abbrevs strings)

-- | Entry in the abbreviation table (section .debug_abbrev)
data DebugAbbrevEntry = DebugAbbrevEntry
   { debugAbbrevCode         :: Word64
   , debugAbbrevTag          :: Tag
   , debugAbbrevHasChildren  :: Bool
   , debugAbbrevAttributes   :: [DebugAbbrevAttribute]
   }
   deriving (Show)

data DebugAbbrevAttribute = DebugAbbrevAttribute
   { debugAbbrevAttrName  :: Attribute
   , debugAbbrevAttrForm  :: Form
   }
   deriving (Show)

getAbbrevAttribute :: Get (Maybe DebugAbbrevAttribute)
getAbbrevAttribute = do
   attr <- getULEB128
   form <- getULEB128
   return $ case (attr,form) of
      (0,0) -> Nothing
      _     -> Just (DebugAbbrevAttribute (toAttribute attr) (toForm form))

-- | Read an entry except if the code is 0 (discard it)
getDebugAbbrevEntry :: Get (Maybe DebugAbbrevEntry)
getDebugAbbrevEntry = do
   code <- getULEB128
   if code == 0
      then return Nothing
      else fmap Just $ DebugAbbrevEntry code
               <$> (toTag <$> getULEB128)
               <*> ((== 1) <$> getWord8)
               <*> (fmap fromJust <$> getWhile isJust getAbbrevAttribute)
         
getDebugAbbrevEntries :: Get [DebugAbbrevEntry]
getDebugAbbrevEntries = fmap fromJust <$> getWhile isJust getDebugAbbrevEntry

data RawAttributeValue
   = RawAttrValueAddress           ByteString
   | RawAttrValueBlock             ByteString
   | RawAttrValueUnsignedConstant  Word64
   | RawAttrValueSignedConstant    Int64
   | RawAttrValueDwarfExpr         [DwarfExpr]
   | RawAttrValueFlag              Bool
   | RawAttrValueOffset            Word64          -- ^ Offset in another section
   | RawAttrValueRelativeReference Word64          -- ^ Offset in the compilation unit
   | RawAttrValueAbsoluteReference Word64          -- ^ Offset from the beginning of the .debug_info section
   | RawAttrValueTypeReference     Word64          -- ^ Type reference (i.e. signature in the .debug_type section)
   | RawAttrValueString            Text            -- ^ String
   | RawAttrValueStringPointer     Word64          -- ^ String pointer in the .debug_str section
   deriving (Show)

data AttributeValue
   = AttrValueAddress           ByteString
   | AttrValueBlock             ByteString
   | AttrValueUnsignedConstant  Word64
   | AttrValueSignedConstant    Int64
   | AttrValueDwarfExpr         [DwarfExpr]
   | AttrValueFlag              Bool
   | AttrValueOffset            Word64                -- ^ Offset in another section
   | AttrValueRelativeReference Word64                -- ^ Offset in the compilation unit
   | AttrValueAbsoluteReference Word64                -- ^ Offset from the beginning of the .debug_info section
   | AttrValueTypeReference     Word64                -- ^ Type reference (i.e. signature in the .debug_type section)
   | AttrValueString            Text                  -- ^ String
   | AttrValueEncoding          Encoding
   | AttrValueEndianity         Endianity
   | AttrValueDecimalSign       DecimalSign
   | AttrValueAccessibility     Accessibility
   | AttrValueVisibility        Visibility
   | AttrValueVirtuality        Virtuality
   | AttrValueLanguage          Language
   | AttrValueCaseSensitivity   CaseSensitivity
   | AttrValueCallingConvention CallingConvention
   | AttrValueInlining          Inlining
   | AttrValueArrayOrdering     ArrayOrdering
   deriving (Show)

data ArrayOrdering
   = ArrayOrderingRowMajor
   | ArrayOrderingColMajor
   | ArrayOrderingCustom Word8
   deriving (Show,Eq)

toArrayOrdering :: Word8 -> ArrayOrdering
toArrayOrdering x = case x of
   0x00 -> ArrayOrderingRowMajor
   0x01 -> ArrayOrderingColMajor
   v    -> ArrayOrderingCustom v

fromArrayOrdering :: ArrayOrdering -> Word8
fromArrayOrdering x = case x of
   ArrayOrderingRowMajor  -> 0x00
   ArrayOrderingColMajor  -> 0x01
   ArrayOrderingCustom v  -> v

data Inlining
   = InliningNotInlined
   | InliningInlined
   | InliningDeclaredNotInlined
   | InliningDeclaredInlined
   | InliningCustom Word8
   deriving (Show,Eq)

toInlining :: Word8 -> Inlining
toInlining x = case x of
   0x00 -> InliningNotInlined
   0x01 -> InliningInlined
   0x02 -> InliningDeclaredNotInlined
   0x03 -> InliningDeclaredInlined
   v    -> InliningCustom v

fromInlining :: Inlining -> Word8
fromInlining x = case x of
   InliningNotInlined         -> 0x00
   InliningInlined            -> 0x01
   InliningDeclaredNotInlined -> 0x02
   InliningDeclaredInlined    -> 0x03
   InliningCustom v           -> v
   

data CallingConvention
   = CallingConventionNormal
   | CallingConventionProgram
   | CallingConventionNoCall
   | CallingConventionCustom Word8
   deriving (Show,Eq)

toCallingConvention :: Word8 -> CallingConvention
toCallingConvention x = case x of
   0x01 -> CallingConventionNormal
   0x02 -> CallingConventionProgram
   0x03 -> CallingConventionNoCall
   v    -> CallingConventionCustom v
   
fromCallingConvention :: CallingConvention -> Word8
fromCallingConvention x = case x of
   CallingConventionNormal    -> 0x01
   CallingConventionProgram   -> 0x02
   CallingConventionNoCall    -> 0x03
   CallingConventionCustom v  -> v
   

data CaseSensitivity
   = CaseSensitive
   | CaseUp
   | CaseDown
   | CaseInsensitive
   | CaseCustom Word8
   deriving (Show,Eq)

toCaseSensitivity :: Word8 -> CaseSensitivity
toCaseSensitivity x = case x of
   0x00 -> CaseSensitive
   0x01 -> CaseUp
   0x02 -> CaseDown
   0x03 -> CaseInsensitive
   v    -> CaseCustom v

fromCaseSensitivity :: CaseSensitivity -> Word8
fromCaseSensitivity x = case x of
   CaseSensitive     -> 0x00
   CaseUp            -> 0x01
   CaseDown          -> 0x02
   CaseInsensitive   -> 0x03
   CaseCustom v      -> v
   

data Language
   = LanguageC89
   | LanguageC
   | LanguageAda83
   | LanguageCPlusPlus
   | LanguageCobol74
   | LanguageCobol85
   | LanguageFortran77
   | LanguageFortran90
   | LanguagePascal83
   | LanguageModula2
   | LanguageJava
   | LanguageC99
   | LanguageAda95
   | LanguageFortran95
   | LanguagePLI
   | LanguageObjectiveC
   | LanguageObjectiveCPlusPlus
   | LanguageUPC
   | LanguageD
   | LanguagePython
   | LanguageCustom Word16
   deriving (Show,Eq)

toLanguage :: Word16 -> Language
toLanguage x = case x of
   0x01 -> LanguageC89
   0x02 -> LanguageC
   0x03 -> LanguageAda83
   0x04 -> LanguageCPlusPlus
   0x05 -> LanguageCobol74
   0x06 -> LanguageCobol85
   0x07 -> LanguageFortran77
   0x08 -> LanguageFortran90
   0x09 -> LanguagePascal83
   0x0a -> LanguageModula2
   0x0b -> LanguageJava
   0x0c -> LanguageC99
   0x0d -> LanguageAda95
   0x0e -> LanguageFortran95
   0x0f -> LanguagePLI
   0x10 -> LanguageObjectiveC
   0x11 -> LanguageObjectiveCPlusPlus
   0x12 -> LanguageUPC
   0x13 -> LanguageD
   0x14 -> LanguagePython
   v    -> LanguageCustom v

fromLanguage :: Language -> Word16
fromLanguage x = case x of
   LanguageC89                -> 0x01
   LanguageC                  -> 0x02
   LanguageAda83              -> 0x03
   LanguageCPlusPlus          -> 0x04
   LanguageCobol74            -> 0x05
   LanguageCobol85            -> 0x06
   LanguageFortran77          -> 0x07
   LanguageFortran90          -> 0x08
   LanguagePascal83           -> 0x09
   LanguageModula2            -> 0x0a
   LanguageJava               -> 0x0b
   LanguageC99                -> 0x0c
   LanguageAda95              -> 0x0d
   LanguageFortran95          -> 0x0e
   LanguagePLI                -> 0x0f
   LanguageObjectiveC         -> 0x10
   LanguageObjectiveCPlusPlus -> 0x11
   LanguageUPC                -> 0x12
   LanguageD                  -> 0x13
   LanguagePython             -> 0x14
   LanguageCustom v           -> v

data Virtuality
   = VirtualityNone
   | VirtualityVirtual
   | VirtualityPureVirtual
   | VirtualityCustom Word8
   deriving (Show,Eq)

toVirtuality :: Word8 -> Virtuality
toVirtuality x = case x of
   0x00 -> VirtualityNone
   0x01 -> VirtualityVirtual
   0x02 -> VirtualityPureVirtual
   v    -> VirtualityCustom v

fromVirtuality :: Virtuality -> Word8
fromVirtuality x = case x of
   VirtualityNone        -> 0x00
   VirtualityVirtual     -> 0x01
   VirtualityPureVirtual -> 0x02
   VirtualityCustom v    -> v
   

data Visibility
   = VisibilityLocal
   | VisibilityExported
   | VisibilityQualified
   | VisibilityCustom Word8
   deriving (Show,Eq)

toVisibility :: Word8 -> Visibility
toVisibility x = case x of
   0x01 -> VisibilityLocal
   0x02 -> VisibilityExported
   0x03 -> VisibilityQualified
   v    -> VisibilityCustom v

fromVisibility :: Visibility -> Word8
fromVisibility x = case x of
   VisibilityLocal      -> 0x01
   VisibilityExported   -> 0x02
   VisibilityQualified  -> 0x03
   VisibilityCustom v   -> v

data Accessibility
   = AccessibilityPublic
   | AccessibilityProtected
   | AccessibilityPrivate
   | AccessibilityCustom Word8
   deriving (Show,Eq)

toAccessibility :: Word8 -> Accessibility
toAccessibility x = case x of
   0x01 -> AccessibilityPublic
   0x02 -> AccessibilityProtected
   0x03 -> AccessibilityPrivate
   v    -> AccessibilityCustom v

fromAccessibility :: Accessibility -> Word8
fromAccessibility x = case x of
   AccessibilityPublic     -> 0x01
   AccessibilityProtected  -> 0x02
   AccessibilityPrivate    -> 0x03
   AccessibilityCustom v   -> v

data DecimalSign
   = DecimalSignUnsigned
   | DecimalSignLeadingOverpunch
   | DecimalSignTrailingOverpunch
   | DecimalSignLeadingSeparate
   | DecimalSignTrailingSeparate
   | DecimalSignCustom Word8
   deriving (Show,Eq)

toDecimalSign :: Word8 -> DecimalSign
toDecimalSign x = case x of
   0x01 -> DecimalSignUnsigned
   0x02 -> DecimalSignLeadingOverpunch
   0x03 -> DecimalSignTrailingOverpunch
   0x04 -> DecimalSignLeadingSeparate
   0x05 -> DecimalSignTrailingSeparate
   v    -> DecimalSignCustom v

fromDecimalSign :: DecimalSign -> Word8
fromDecimalSign x = case x of
   DecimalSignUnsigned           -> 0x01
   DecimalSignLeadingOverpunch   -> 0x02
   DecimalSignTrailingOverpunch  -> 0x03
   DecimalSignLeadingSeparate    -> 0x04
   DecimalSignTrailingSeparate   -> 0x05
   DecimalSignCustom v           -> v

data Endianity
   = EndianDefault
   | EndianBig
   | EndianLittle
   | EndianCustom Word8
   deriving (Show,Eq)

toEndianity :: Word8 -> Endianity
toEndianity x = case x of
   0x00 -> EndianDefault
   0x01 -> EndianBig
   0x02 -> EndianLittle
   v    -> EndianCustom v

fromEndianity :: Endianity -> Word8
fromEndianity x = case x of
   EndianDefault  -> 0x00
   EndianBig      -> 0x01
   EndianLittle   -> 0x02
   EndianCustom v -> v

data Encoding
   = EncodingAddress
   | EncodingBoolean
   | EncodingComplexFloat
   | EncodingFloat
   | EncodingSigned
   | EncodingSignedChar
   | EncodingUnsigned
   | EncodingUnsignedChar
   | EncodingImaginaryFloat
   | EncodingPackedDecimal
   | EncodingNumericString
   | EncodingEdited
   | EncodingSignedFixed
   | EncodingUnsignedFixed
   | EncodingDecimalFloat
   | EncodingUTF
   | EncodingCustom Word8
   deriving (Show,Eq)

toEncoding :: Word8 -> Encoding
toEncoding x = case x of
   0x01  -> EncodingAddress
   0x02  -> EncodingBoolean
   0x03  -> EncodingComplexFloat
   0x04  -> EncodingFloat
   0x05  -> EncodingSigned
   0x06  -> EncodingSignedChar
   0x07  -> EncodingUnsigned
   0x08  -> EncodingUnsignedChar
   0x09  -> EncodingImaginaryFloat
   0x0a  -> EncodingPackedDecimal
   0x0b  -> EncodingNumericString
   0x0c  -> EncodingEdited
   0x0d  -> EncodingSignedFixed
   0x0e  -> EncodingUnsignedFixed
   0xf   -> EncodingDecimalFloat
   0x10  -> EncodingUTF
   _     -> EncodingCustom x

fromEncoding :: Encoding -> Word8
fromEncoding x = case x of
   EncodingAddress         -> 0x01
   EncodingBoolean         -> 0x02
   EncodingComplexFloat    -> 0x03
   EncodingFloat           -> 0x04
   EncodingSigned          -> 0x05
   EncodingSignedChar      -> 0x06
   EncodingUnsigned        -> 0x07
   EncodingUnsignedChar    -> 0x08
   EncodingImaginaryFloat  -> 0x09
   EncodingPackedDecimal   -> 0x0a
   EncodingNumericString   -> 0x0b
   EncodingEdited          -> 0x0c
   EncodingSignedFixed     -> 0x0d
   EncodingUnsignedFixed   -> 0x0e
   EncodingDecimalFloat    -> 0xf
   EncodingUTF             -> 0x10
   EncodingCustom v        -> v


-- | Attribute value
getAttributeValue :: Word8 -> Endianness -> DwarfFormat -> Maybe BS.ByteString -> Attribute -> Form -> Get AttributeValue
getAttributeValue addressSize endian format strings att form = do
   raw <- getValueFromForm addressSize endian format form
   case raw of
      RawAttrValueAddress x               -> return $ AttrValueAddress x            
      RawAttrValueBlock x                 -> return $ AttrValueBlock x              
      RawAttrValueUnsignedConstant x
         | att == AttrEncoding            -> return $ AttrValueEncoding (toEncoding (fromIntegral x))
         | att == AttrEndianity           -> return $ AttrValueEndianity (toEndianity (fromIntegral x))
         | att == AttrDecimalSign         -> return $ AttrValueDecimalSign (toDecimalSign (fromIntegral x))
         | att == AttrAccessibility       -> return $ AttrValueAccessibility (toAccessibility (fromIntegral x))
         | att == AttrVisibility          -> return $ AttrValueVisibility (toVisibility (fromIntegral x))
         | att == AttrVirtuality          -> return $ AttrValueVirtuality (toVirtuality (fromIntegral x))
         | att == AttrLanguage            -> return $ AttrValueLanguage (toLanguage (fromIntegral x))
         | att == AttrIdentifierCase      -> return $ AttrValueCaseSensitivity (toCaseSensitivity (fromIntegral x))
         | att == AttrCallingConvention   -> return $ AttrValueCallingConvention (toCallingConvention (fromIntegral x))
         | att == AttrInline              -> return $ AttrValueInlining (toInlining (fromIntegral x))
         | att == AttrOrdering            -> return $ AttrValueArrayOrdering (toArrayOrdering (fromIntegral x))
         | otherwise                      -> return $ AttrValueUnsignedConstant x   
      RawAttrValueSignedConstant x        -> return $ AttrValueSignedConstant x     
      RawAttrValueDwarfExpr x             -> return $ AttrValueDwarfExpr x          
      RawAttrValueFlag x                  -> return $ AttrValueFlag x               
      RawAttrValueOffset x                -> return $ AttrValueOffset x             
      RawAttrValueRelativeReference x     -> return $ AttrValueRelativeReference x  
      RawAttrValueAbsoluteReference x     -> return $ AttrValueAbsoluteReference x  
      RawAttrValueTypeReference x         -> return $ AttrValueTypeReference x      
      RawAttrValueString x                -> return $ AttrValueString x             
      RawAttrValueStringPointer off    -> do
         -- read the string
         let str = Text.decodeUtf8 . runGetOrFail getByteStringNul . BS.drop (fromIntegral off) <$> strings
         return (AttrValueString (fromJust str))


-- | Get raw attribute value from form only
getValueFromForm :: Word8 -> Endianness -> DwarfFormat -> Form -> Get RawAttributeValue
getValueFromForm addressSize endian format form = do
   let (gw8,gw16,gw32,gw64,gwN) = getGetters endian format
   case form of
      FormAddress       -> RawAttrValueAddress           <$> getByteString (fromIntegral addressSize)
      FormBlock1        -> RawAttrValueBlock             <$> (getByteString =<< (fromIntegral <$> gw8))
      FormBlock2        -> RawAttrValueBlock             <$> (getByteString =<< (fromIntegral <$> gw16))
      FormBlock4        -> RawAttrValueBlock             <$> (getByteString =<< (fromIntegral <$> gw32))
      FormBlock         -> RawAttrValueBlock             <$> (getByteString =<< (fromIntegral <$> (getULEB128 :: Get Word64)))
      FormData1         -> RawAttrValueUnsignedConstant  <$> (fromIntegral <$> gw8)
      FormData2         -> RawAttrValueUnsignedConstant  <$> (fromIntegral <$> gw16)
      FormData4         -> RawAttrValueUnsignedConstant  <$> (fromIntegral <$> gw32)
      FormData8         -> RawAttrValueUnsignedConstant  <$> (fromIntegral <$> gw64)
      FormUData         -> RawAttrValueUnsignedConstant  <$> getULEB128
      FormSData         -> RawAttrValueSignedConstant    <$> getSLEB128
      FormExprLoc       -> do
         sz <- fromIntegral    <$> (getULEB128 :: Get Word64)
         RawAttrValueDwarfExpr <$> isolate sz (getWhole (getDwarfExpr endian format))
      FormFlag          -> RawAttrValueFlag              <$> ((/= 0) <$> gw8)
      FormFlagPresent   -> return (RawAttrValueFlag True)
      FormSecOffset     -> RawAttrValueOffset            <$> gwN
      FormRef1          -> RawAttrValueRelativeReference <$> (fromIntegral <$> gw8)
      FormRef2          -> RawAttrValueRelativeReference <$> (fromIntegral <$> gw16)
      FormRef4          -> RawAttrValueRelativeReference <$> (fromIntegral <$> gw32)
      FormRef8          -> RawAttrValueRelativeReference <$> (fromIntegral <$> gw64)
      FormRefUData      -> RawAttrValueRelativeReference <$> getULEB128
      FormRefAddress    -> RawAttrValueAbsoluteReference <$> gwN
      FormRefSig8       -> RawAttrValueTypeReference     <$> gw64
      FormString        -> RawAttrValueString            <$> (Text.decodeUtf8 <$> getByteStringNul)
      FormStringPointer -> RawAttrValueStringPointer     <$> gwN
      FormIndirect      -> getValueFromForm addressSize endian format =<< (toForm <$> getULEB128)

getDebugInfo :: Endianness -> BS.ByteString -> Maybe BS.ByteString -> Get DebugInfo
getDebugInfo endian secAbbrevs strings = do
   cuh <- getCompilationUnitHeader endian
   -- the length in the header excludes only itself
   let len = case cuhDwarfFormat cuh of
            Dwarf32 -> fromIntegral (cuhUnitLength cuh) - 7
            Dwarf64 -> fromIntegral (cuhUnitLength cuh) - 11
   -- get abbreviations
   let 
      abbrevBS = BS.drop (fromIntegral $ cuhAbbrevOffset cuh) secAbbrevs
      abbrevs = runGetOrFail getDebugAbbrevEntries abbrevBS

   -- get entries
   entries <- isolate len $ getDebugEntries endian cuh abbrevs strings

   return $ DebugInfo cuh entries

getDebugType :: Endianness -> Get DebugType
getDebugType endian = do
   tuh <- getTypeUnitHeader endian
   -- the length in the header excludes only itself
   let len = case tuhDwarfFormat tuh of
            Dwarf32 -> fromIntegral (tuhUnitLength tuh) - 19
            Dwarf64 -> fromIntegral (tuhUnitLength tuh) - 27
   bs <- getByteString len
   return $ DebugType tuh bs
