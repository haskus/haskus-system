{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import CmdLine (Options(..), getOptions)

import ViperVM.Format.Elf
import ViperVM.Format.Elf.PreHeader
import ViperVM.Format.Elf.Header
import ViperVM.Format.Elf.Section
import ViperVM.Format.Elf.Segment
import ViperVM.Format.Elf.Intel
import ViperVM.Format.Elf.Symbol
import ViperVM.Format.Elf.Relocation
import ViperVM.Format.Elf.Dynamic

import ViperVM.Format.Dwarf

import ViperVM.Format.Binary.BitSet as BitSet

import Control.Monad (when, msum, mzero, MonadPlus)
import Data.Foldable (forM_)
import Data.Text.Format
import Happstack.Server
import Lucid
import Data.FileEmbed
import Data.Word
import Data.Tree (drawTree)
import Data.Text (Text)
import qualified Data.Vector as Vector
import qualified Data.List as List
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy.IO as Text
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS

main :: IO ()
main = do
   opts <- getOptions
   elf <- readElf (optpath opts)
   server (optpath opts) elf (nullConf { port = optport opts} )

server :: FilePath -> Elf -> Conf -> IO ()
server pth elf conf = do
   Text.putStrLn (format "Starting Web server at localhost: {}" (Only $ port conf))

   let ok' = ok . toResponse . renderBS . appTemplate

   simpleHTTP conf $ msum
      [ dir "css" $ dir "style.css" $ ok css

      -- Section specific
      , dir "section" $ nullDir >> ok' (sectionsPage pth elf)
      
      , dir "section" $ path $ \secnum -> do
         -- retrieve section by index
         sec <- lookupMaybe (getSectionByIndex elf secnum)
         msum
            [ nullDir >> ok' (sectionPage pth elf secnum sec) 
            
            -- dump section content
            , dir "content" $ do
               -- select suggested output filename by the browser
               let filename = format "section{}.bin" (Only (secnum :: Int))
                   disp     = format "attachment; filename=\"{}\"" (Only filename)
               ok 
                  $ addHeader "Content-Disposition" (Text.unpack disp)
                  $ toResponseBS (C.pack "application/octet-stream")
                  $ LBS.fromStrict
                  $ getSectionContentBS elf sec
            ]

      -- Segment specific
      , dir "segment" $ nullDir >> ok' (segmentsPage pth elf)

      , nullDir >> ok' (welcomePage pth elf)
      ]

-- | Return the value in a Maybe or mzero in MonadPlus
lookupMaybe :: MonadPlus m => Maybe a -> m a
lookupMaybe = maybe mzero return

hexStr :: Integral a => a -> Text.Text
hexStr a = format "0x{}" (Only $ hex a)

welcomePage :: FilePath -> Elf -> Html ()
welcomePage pth elf = do
   p_ . toHtml $ "Info about: " ++ pth
   h2_ "Pre-header"
   showPreHeader (elfPreHeader elf)
   h2_ "Header"
   showHeader (elfHeader elf)
   h2_ "Sections"
   showSections elf
   h2_ "Segments"
   showSegments elf

sectionsPage :: FilePath -> Elf -> Html ()
sectionsPage pth elf = do
   p_ . toHtml $ "Info about: " ++ pth
   h2_ "Sections"
   showSections elf

segmentsPage :: FilePath -> Elf -> Html ()
segmentsPage pth elf = do
   p_ . toHtml $ "Info about: " ++ pth
   h2_ "Segments"
   showSegments elf

sectionPage :: FilePath -> Elf -> Int -> Section -> Html ()
sectionPage pth elf i s = do
   p_ . toHtml $ "Info about: " ++ pth
   h2_ "Section"
   let
      secname = getSectionName elf s
      name = case secname of
         Just str -> toHtml str
         Nothing  -> span_ [class_ "invalid"] "Invalid section name"
   h3_ $ do
      toHtml $ format "Section {} \"" (Only (i :: Int))
      name
      "\""
   showSection elf i secname s

showPreHeader :: PreHeader -> Html ()
showPreHeader ph = table_ $ do
   tr_ $ do
      th_ "Word size"
      td_ $ case preHeaderWordSize ph of
         WordSize32 -> "32 bits"
         WordSize64 -> "64 bits"
   tr_ $ do
      th_ "Endianness"
      td_ $ case preHeaderEndianness ph of
         LittleEndian -> "Little endian"
         BigEndian    -> "Big endian"
   tr_ $ do
      th_ "Version"
      td_ . toHtml $ show (preHeaderVersion ph)
      when (preHeaderVersion ph /= elfCurrentVersion) $
         td_ . toHtml $ "This is strange: ELF version should always be " ++ show elfCurrentVersion
   tr_ $ do
      th_ "OS ABI"
      td_ . toHtml $ show (preHeaderOSABI ph)
   tr_ $ do
      th_ "ABI version"
      td_ . toHtml $ show (preHeaderABIVersion ph)

showHeader :: Header -> Html ()
showHeader h = table_ $ do
   tr_ $ do
      th_ "Type"
      td_ . toHtml $ show (headerType h)
   tr_ $ do
      th_ "Architecture"
      td_ . toHtml $ show (headerArch h)
   tr_ $ do
      th_ "Version"
      td_ . toHtml $ show (headerVersion h)
   tr_ $ do
      th_ "Entry address"
      td_ . toHtml $ hexStr (headerEntryAddress h)
   tr_ $ do
      th_ "Segment table offset"
      td_ . toHtml $ show (headerSegmentTableOffset h)
   tr_ $ do
      th_ "Section table offset"
      td_ . toHtml $ show (headerSectionTableOffset h)
   tr_ $ do
      th_ "Flags"
      td_ . toHtml $ show (headerFlags h)
   tr_ $ do
      th_ "Header size"
      td_ . toHtml $ show (headerHeaderSize h)
   tr_ $ do
      th_ "Segment entry size"
      td_ . toHtml $ show (headerSegmentEntrySize h)
   tr_ $ do
      th_ "Segment entry count"
      td_ . toHtml $ show (headerSegmentEntryCount h)
   tr_ $ do
      th_ "Section entry size"
      td_ . toHtml $ show (headerSectionEntrySize h)
   tr_ $ do
      th_ "Section entry count"
      td_ . toHtml $ show (headerSectionEntryCount h)
   tr_ $ do
      th_ "Section names entry index"
      td_ . toHtml $ "Section " ++ show (headerSectionNameIndex h)


showSections :: Elf -> Html ()
showSections elf =
   forM_ (Vector.indexed (elfSections elf)) $ \(i,s) -> do
      let
         secname = getSectionName elf s
         name = case secname of
            Just str -> toHtml str
            Nothing  -> span_ [class_ "invalid"] "Invalid section name"
      h3_ $ do
         toHtml $ format "Section {} \"" (Only (i :: Int))
         name
         "\""
      showSection elf i secname s

showSection :: Elf -> Int -> Maybe Text -> Section -> Html ()
showSection elf secnum secname s = do
   table_ $ do
      tr_ $ do
         th_ "Name index"
         td_ . toHtml $ show (sectionNameIndex s)
      tr_ $ do
         th_ "Type"
         td_ . toHtml $ show (sectionType s)
      tr_ $ do
         th_ "Flags"
         td_ . toHtml . concat . List.intersperse ", " $ fmap show (BitSet.toList $ sectionFlags s)
      tr_ $ do
         th_ "Address"
         td_ . toHtml $ hexStr (sectionAddr s)
      tr_ $ do
         th_ "Offset"
         td_ . toHtml $ show (sectionOffset s)
      tr_ $ do
         th_ "Size"
         td_ . toHtml $ show (sectionSize s)
      tr_ $ do
         th_ "Link"
         td_ . toHtml $ show (sectionLink s)
      tr_ $ do
         th_ "Info"
         td_ . toHtml $ show (sectionInfo s)
      tr_ $ do
         th_ "Alignment"
         td_ . toHtml $ show (sectionAlignment s)
      tr_ $ do
         th_ "Entry size"
         td_ . toHtml $ show (sectionEntrySize s)

      case getFullSectionType elf s of
         -- Show string table
         BasicSectionType SectionTypeSTRTAB -> tr_ $ do
            th_ "Strings"
            let strs = getStringsFromSection elf s
            td_ $ table_ $ do
               tr_ $ do
                  th_ "Offset"
                  th_ "Value"
               forM_ strs $ \(i,str) -> tr_ $ do
                  td_ (toHtml (show i))
                  td_ (toHtml str)

         -- Show symbol table
         BasicSectionType SectionTypeSYMTAB -> tr_ $ do
            th_ "Symbols"
            let syms = getSymbolsFromSection elf s
            td_ $ showSymbols elf s syms

         -- Show dynamic symbol table
         BasicSectionType SectionTypeDYNSYM -> tr_ $ do
            th_ "Dynamic symbols"
            let syms = getSymbolsFromSection elf s
            td_ $ showSymbols elf s syms

         -- Show dynamic info
         BasicSectionType SectionTypeDYNAMIC -> tr_ $ do
            th_ "Dynamic information"
            let des = getDynamicEntriesFromSection elf s
            td_ $ showDynamicEntries des

         -- Show interpreter path
         BasicSectionType SectionTypePROGBITS
            | getSectionName elf s == Just ".interp" -> tr_ $ do
               th_ "Interpreter path"
               let c = getSectionContentBS elf s
               td_ . toHtml $ Text.decodeUtf8 c

         -- Show relocation entries
         typ@(SectionTypeRelocation {}) -> tr_ $ do
            th_ "Relocation entries"
            let es = getRelocationEntriesFromSection elf s
            td_ $ showRelocationEntries
                     (relocSectionHasAddend typ)
                     es

         -- Show version needed entries
         BasicSectionType SectionTypeGNU_verneed -> tr_ $ do
            th_ "Version needed entries"
            let es = getVersionNeededEntriesFromSection elf s
            td_ $ showVersionNeededEntries es

         -- Show notes
         BasicSectionType SectionTypeNOTE -> tr_ $ do
            th_ "Notes"
            let es = getNoteEntriesFromSection elf s
            td_ $ showNoteEntries es

         -- Show Intel debug opt
         BasicSectionType SectionTypePROGBITS
            | secname == Just ".debug_opt_report" -> tr_ $ do
               th_ "Intel ZCA table"
               let zca = getZCATableFromSection elf s
               td_ $ do
                  showZCATable zca

         -- Show debug info
         BasicSectionType SectionTypePROGBITS
            | getSectionName elf s == Just ".debug_info" -> tr_ $ do
               th_ "Debug info"
               let c = getDebugInfoFromSection elf s
               td_ $ showDebugInfo c

         -- Show debug type
         BasicSectionType SectionTypePROGBITS
            | getSectionName elf s == Just ".debug_type" -> tr_ $ do
               th_ "Debug type"
               let c = getDebugTypeFromSection elf s
               td_ . toHtml $ show c

         -- Show debug abbrev
         BasicSectionType SectionTypePROGBITS
            | getSectionName elf s == Just ".debug_abbrev" -> tr_ $ do
               th_ "Debug abbreviations"
               let c = getDebugAbbrevFromSection elf s
               td_ $ showDebugAbbrev c

         _ -> return ()



   let contentPath = Text.toStrict $ format "/section/{}/content/" (Only secnum)
   br_ []
   div_ $ do
      "Download: "
      a_ [href_ contentPath] "raw"

showSegments :: Elf -> Html ()
showSegments elf = do
   let segs = elfSegments elf
   table_ $ do
      tr_ $ do
         th_ "Index"
         th_ "Type"
         th_ "Flags"
         th_ "Offset"
         th_ "Virtual address"
         th_ "Physical address"
         th_ "Size in the file"
         th_ "Size in memory"
         th_ "Alignment"
      forM_ (Vector.indexed segs) $ \(i,seg) -> tr_ $ do
         td_ . toHtml $ show i
         td_ . toHtml $ show (segmentType seg)
         td_ $ do
            let flags = BitSet.toList (segmentFlags seg)
                spc   = toHtmlRaw ("&nbsp;&nbsp;&nbsp;" :: Text)
            if SegmentFlagReadable   `elem` flags then " R " else spc
            if SegmentFlagWritable   `elem` flags then " W " else spc
            if SegmentFlagExecutable `elem` flags then " X " else spc
         td_ . toHtml $ show (segmentOffset seg)
         td_ . toHtml $ hexStr (segmentVirtualAddress seg)
         td_ . toHtml $ hexStr (segmentPhysicalAddress seg)
         td_ . toHtml $ show (segmentSizeInFile seg)
         td_ . toHtml $ show (segmentSizeInMemory seg)
         td_ $ do
            let alg = segmentAlignment seg
            toHtml $ show alg
            toHtml $ " (2^" ++ show (round (logBase 2 (fromIntegral alg) :: Float) :: Int) ++ ")"

showZCATable :: ZCATable -> Html ()
showZCATable t =
   table_ $ do
      tr_ $ do
         th_ "Version major"
         td_ . toHtml $ show (zcaVersionMajor (zcaHeader t))
      tr_ $ do
         th_ "Version minor"
         td_ . toHtml $ show (zcaVersionMinor (zcaHeader t))
      tr_ $ do
         th_ "Entry table offset"
         td_ . toHtml $ show (zcaEntryOffset (zcaHeader t))
      tr_ $ do
         th_ "Entry table count"
         td_ . toHtml $ show (zcaEntryCount (zcaHeader t))
      tr_ $ do
         th_ "String table offset"
         td_ . toHtml $ show (zcaStringsOffset (zcaHeader t))
      tr_ $ do
         th_ "String table size"
         td_ . toHtml $ show (zcaStringsSize (zcaHeader t))
      tr_ $ do
         th_ "Value table offset"
         td_ . toHtml $ show (zcaExprsOffset (zcaHeader t))
      tr_ $ do
         th_ "Value table size"
         td_ . toHtml $ show (zcaExprsSize (zcaHeader t))
      tr_ $ do
         th_ "Unknown 2"
         td_ . toHtml $ show (zcaStuff1 (zcaHeader t))
      tr_ $ do
         th_ "Entries"
         td_ $ table_ $ do
            tr_ $ do
               th_ "Offset"
               th_ "Name"
               th_ "Value"
            forM_ (zcaEntries t) $ \e -> tr_ $ do
               td_ $ toHtml $ hexStr (zcaIP e)
               td_ $ toHtml $ zcaName e
               td_ $ do
                  "["
                  toHtml . Text.concat . List.intersperse "," . fmap hexStr . BS.unpack $ zcaValue e
                  "]"
         

showSymbols :: Elf -> Section -> [SymbolEntry] -> Html ()
showSymbols elf symSec ss = do
   let 
      symtab = getSectionByIndex elf (sectionLink symSec)
      getSymName idx = case (idx,symtab) of
         (0, _)        -> Nothing
         (_, Just sec) -> getStringFromSection elf sec idx
         (_, Nothing)  -> Nothing
         
   table_ $ do
      tr_ $ do
         th_ "(index) Name"
         th_ "Binding"
         th_ "Type"
         th_ "Visibility"
         th_ "Info"
         th_ "Value"
         th_ "Size"
      forM_ ss $ \s -> tr_ $ do
         td_ $ do
            let idx = symbolNameIndex s
            case getSymName idx of
               Nothing   -> do
                  toHtml $ format "({}) " (Only idx)
                  span_ [class_ "invalid"] "None"
               Just name -> do
                  toHtml $ format "({}) {}" (idx, name)

         td_ $ case symbolBinding s of
            SymbolBindingLocal      -> span_ [class_ "sym_local"]  "Local"
            SymbolBindingGlobal     -> span_ [class_ "sym_global"] "Global"
            SymbolBindingWeak       -> span_ [class_ "sym_weak"]   "Weak"
            SymbolBindingUnknown v  -> toHtml $ format "Unknown ({})" (Only v)

         td_ $ case symbolType s of
            SymbolTypeNone          -> "None"
            SymbolTypeData          -> "Data"
            SymbolTypeCode          -> "Code"
            SymbolTypeSection       -> "Section"
            SymbolTypeFile          -> "File"
            SymbolTypeCommonData    -> "Data (common)"
            SymbolTypeTLSData       -> "Data (TLS)"
            SymbolTypeUnknown v     -> toHtml $ format "Unknown ({})" (Only v)

         td_ $ case symbolVisibility s of
            SymbolVisibilityDefault    -> "Default"
            SymbolVisibilityInternal   -> "Internal"
            SymbolVisibilityHidden     -> "Hidden"
            SymbolVisibilityProtected  -> "Protected"

         td_ $ case symbolInfo s of
            SymbolInfoUndefined           -> span_ [class_ "sym_undefined"] "Undefined"
            SymbolInfoAbsolute            -> span_ [class_ "sym_absolute"] "Absolute"
            SymbolInfoCommon              -> "Common"
            SymbolInfoIndexInExtraTable   -> "In extra table"
            SymbolInfoSectionBeforeAll    -> "Before all others sections"
            SymbolInfoSectionAfterAll     -> "After all others sections"
            SymbolInfoSectionIndex v      -> toHtml $ format "In section {}" (Only v)
            SymbolInfoUnknown v           -> toHtml $ format "Unknown ({})" (Only v)

         td_ . toHtml $ show (symbolValue s)
         td_ . toHtml $ show (symbolSize s)

showRelocationEntries :: Bool -> [RelocationEntry] -> Html ()
showRelocationEntries withAddend es = do
   table_ $ do
      tr_ $ do
         th_ "Address"
         th_ "Type"
         th_ "Symbol index"
         when withAddend $ th_ "Addend"
      forM_ es $ \e -> tr_ $ do
         td_ . toHtml $ hexStr (relocAddress e)
         td_ . toHtml $ show (relocType e)
         td_ $ do
            let idx = relocSymbolIndex e
            toHtml $ show idx
         case (withAddend, relocAddend e) of
            (True, Just x) -> td_ . toHtml $ show x
            _              -> return ()

showVersionNeededEntries :: [VersionNeeded] -> Html ()
showVersionNeededEntries es = do
   table_ $ do
      tr_ $ do
         th_ "Version"
         th_ "File name"
         th_ "Versions"
      forM_ es $ \e -> tr_ $ do
         td_ . toHtml $ show (vnVersion e)
         td_ . toHtml $ vnFileName e
         td_ . table_ $ do
            tr_ $ do
               th_ "Name"
               th_ "Hash"
               th_ "Flags"
               th_ "Other"
            forM_ (vnEntries e) $ \v -> do
               tr_ $ do
                  td_ . toHtml $ vnaName v
                  td_ . toHtml $ show (vnaHash v)
                  td_ . toHtml $ show (vnaFlags v)
                  td_ . toHtml $ show (vnaOther v)

showNoteEntries :: [Note] -> Html ()
showNoteEntries es = do
   table_ $ do
      tr_ $ do
         th_ "Name"
         th_ "Type"
         th_ "Descriptor"
      forM_ es $ \e -> tr_ $ do
         td_ . toHtml $ noteName e
         td_ . toHtml $ show (noteType e)
         td_ . toHtml $ show (BS.unpack $ noteDescriptor e)

showDebugAbbrev :: [DebugAbbrevEntry] -> Html ()
showDebugAbbrev es = do
   table_ $ do
      tr_ $ do
         th_ "Code"
         th_ "Tag"
         th_ "Has children"
         th_ "Attributes"
      forM_ es $ \e -> tr_ $ do
         td_ . toHtml $ show (debugAbbrevCode e)
         td_ . toHtml $ show (debugAbbrevTag e)
         td_ . toHtml $ show (debugAbbrevHasChildren e)
         td_ . table_ $ do
            tr_ $ do
               th_ "Attribute"
               th_ "Form"
            forM_ (debugAbbrevAttributes e) $ \att -> tr_ $ do
               td_ . toHtml . show $ debugAbbrevAttrName att
               td_ . toHtml . show $ debugAbbrevAttrForm att

showDebugInfo :: [DebugInfo] -> Html ()
showDebugInfo dis = do
   forM_ dis $ \di -> table_ $ do
      let cuh = debugInfoCompilationUnitHeader di
      tr_ $ do
         th_ "Format"
         td_ . toHtml $ show (cuhDwarfFormat cuh)
      tr_ $ do
         th_ "Compilation unit length"
         td_ . toHtml $ show (cuhUnitLength cuh)
      tr_ $ do
         th_ "DWARF version"
         td_ . toHtml $ show (cuhVersion cuh)
      tr_ $ do
         th_ "Offset in abbreviation table"
         td_ . toHtml $ show (cuhAbbrevOffset cuh)
      tr_ $ do
         th_ "Address size"
         td_ . toHtml $ show (cuhAddressSize cuh)
      tr_ $ do
         th_ "Entries"
         td_ $ table_ $ do
            tr_ $ do
               th_ "Abbrev code"
               th_ "Tag"
               th_ "Has children"
               th_ "Attributes"
            forM_ (debugInfoEntries di) $ \ent -> 
               case ent of
                  Nothing -> tr_ $ td_ [colspan_ "4"] "NULL entry"
                  Just e  -> tr_ $ do
                     td_ . toHtml $ show (debugEntryAbbrevCode e)
                     td_ . toHtml $ show (debugEntryTag e)
                     td_ . toHtml $ show (debugEntryHasChildren e)
                     td_ $ table_ $ do
                        tr_ $ do
                           th_ "Name"
                           th_ "Value"
                        forM_ (debugEntryAttributes e) $ \att -> tr_ $ do
                           td_ . toHtml $ show (debugAttrName att)
                           td_ . toHtml $ show (debugAttrValue att)
      tr_ $ do
         th_ "Entry tree"
         td_ . pre_ . toHtml . drawTree . fmap show $ debugEntryTree (debugInfoEntries di)


showDynamicEntries :: [DynamicEntry] -> Html ()
showDynamicEntries es = do
   let
      showFlags :: (Show a, CBitSet a) => BitSet Word64 a -> Html ()
      showFlags = toHtml . concat . List.intersperse ", " . fmap show . BitSet.toList

   table_ $ do
      tr_ $ do
         th_ "Type"
         th_ "Value"
      forM_ es $ \e -> tr_ $ do
         case e of
            DynEntryRaw raw -> do
               td_ . toHtml $ show (rawDynType raw)
               td_ . toHtml $ hexStr (rawDynValue raw)
            DynEntryNone -> do
               td_ "None"
               td_ ""
            DynEntryFlags flags -> do
               td_ "Flags"
               td_ $ showFlags flags
            DynEntryStateFlags flags -> do
               td_ "State flags"
               td_ $ showFlags flags
            DynEntryPositionalFlags flags -> do
               td_ "Positional flags"
               td_ $ showFlags flags
            DynEntryFeatureSelection features -> do
               td_ "Feature selection"
               td_ $ showFlags features
            DynEntryNeededLibrary lib -> do
               td_ "Needed library"
               td_ . toHtml $ lib
            DynEntryStringTableAddress addr -> do
               td_ "String table address"
               td_ . toHtml $ hexStr addr
            DynEntryStringTableSize sz -> do
               td_ "String table size"
               td_ . toHtml $ format "{} bytes" (Only sz)
            DynEntrySymbolTableAddress addr -> do
               td_ "Symbol table address"
               td_ . toHtml $ hexStr addr
            DynEntrySymbolEntrySize sz -> do
               td_ "Symbol entry size"
               td_ . toHtml $ format "{} bytes" (Only sz)
            DynEntryInitFunctionAddress addr -> do
               td_ "Init function address"
               td_ . toHtml $ hexStr addr
            DynEntryFiniFunctionAddress addr -> do
               td_ "Fini function address"
               td_ . toHtml $ hexStr addr
            DynEntryInitFunctionArrayAddress addr -> do
               td_ "Init function array address"
               td_ . toHtml $ hexStr addr
            DynEntryFiniFunctionArrayAddress addr -> do
               td_ "Fini function array address"
               td_ . toHtml $ hexStr addr
            DynEntryInitFunctionArraySize sz -> do
               td_ "Init function array size"
               td_ . toHtml $ format "{} bytes" (Only sz)
            DynEntryFiniFunctionArraySize sz -> do
               td_ "Fini function array size"
               td_ . toHtml $ format "{} bytes" (Only sz)
            DynEntrySymbolHashTableAddress addr -> do
               td_ "Symbol hash table address"
               td_ . toHtml $ hexStr addr
            DynEntryGNUSymbolHashTableAddress addr -> do
               td_ "GNU symbol hash table address"
               td_ . toHtml $ hexStr addr
            DynEntryPLTRelocAddress addr -> do
               td_ "PLT relocations address"
               td_ . toHtml $ hexStr addr
            DynEntryPLTGOTAddress addr -> do
               td_ "PLT GOT address"
               td_ . toHtml $ hexStr addr
            DynEntryPLTRelocSize sz -> do
               td_ "Size of PLT relocations"
               td_ . toHtml $ format "{} bytes" (Only sz)
            DynEntryRelocaAddress addr -> do
               td_ "Relocations with addend address"
               td_ . toHtml $ hexStr addr
            DynEntryRelocaSize sz -> do
               td_ "Size of relocations with addend"
               td_ . toHtml $ format "{} bytes" (Only sz)
            DynEntryRelocaEntrySize sz -> do
               td_ "Size of a relocation with addend entry"
               td_ . toHtml $ format "{} bytes" (Only sz)


appTemplate :: Html () -> Html ()
appTemplate doc = do
   head_ $ do
      title_ "ViperVM Web Interface"
      meta_ [ httpEquiv_ "Content-Type"
            , content_ "text/html;charset=utf-8"
            ]
      link_ [ rel_  "stylesheet"
            , type_ "text/css"
            , href_ "/css/style.css"
            ]
   h1_ "ELF viewer"
   doc

css :: Response
css = toResponseBS
   (C.pack "text/css")
   (LBS.fromStrict $(embedFile "src/apps/Elf/style.css"))

