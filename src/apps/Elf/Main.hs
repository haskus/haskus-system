{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import CmdLine (Options(..), getOptions)

import ViperVM.Format.Elf
import ViperVM.Format.Elf.PreHeader
import ViperVM.Format.Elf.Header
import ViperVM.Format.Elf.Section

import Control.Monad (when)
import Data.Foldable (msum, forM_)
import Data.Text.Format
import Happstack.Server
import Lucid
import Data.FileEmbed
import qualified Data.Text.Lazy.IO as Text
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

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

--      , dir "test" $ nullDir >> ok' testPage

      , nullDir >> ok' (welcomePage pth elf)
      ]

welcomePage :: FilePath -> Elf -> Html ()
welcomePage pth elf = do
   p_ . toHtml $ "Info about: " ++ pth
   h2_ "Pre-header"
   showPreHeader (elfPreHeader elf)
   h2_ "Header"
   showHeader (elfHeader elf)
   h2_ "Sections"
   forM_ (elfSections elf `zip` [0..] ) $ \(s,i) -> do
      let name = case extractSectionNameByIndex elf (sectionNameIndex s) of
            Just str -> toHtml str
            Nothing  -> span_ [class_ "invalid"] "Invalid section name"
      h3_ $ do
         toHtml $ format "Section {} \"" (Only (i :: Int))
         name
         "\""
      showSection elf s

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
      td_ . toHtml $ show (headerEntry h)
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


showSection :: Elf -> Section -> Html ()
showSection elf s = table_ $ do
   tr_ $ do
      th_ "Name index"
      td_ . toHtml $ show (sectionNameIndex s)
   tr_ $ do
      th_ "Type"
      td_ . toHtml $ show (sectionType s)
   tr_ $ do
      th_ "Flags"
      td_ . toHtml $ show (sectionFlags s)
   tr_ $ do
      th_ "Address"
      td_ . toHtml $ show (sectionAddr s)
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
   case sectionType s of
      -- Show string table
      SectionTypeSTRTAB -> tr_ $ do
         th_ "Strings"
         let strs = extractSectionStrings elf s
         td_ $ ul_ $ forM_ strs $ \(i,str) -> do
            li_ . toHtml $ format "{} - \"{}\"" (i,str)

      -- Show symbol table
      SectionTypeSYMTAB -> tr_ $ do
         th_ "Symbols"
         let syms = getSectionSymbols elf s
         td_ $ showSymbols syms
      _ -> return ()

showSymbols :: [SymbolEntry] -> Html ()
showSymbols ss = do
   table_ $ do
      tr_ $ do
         th_ "Name index"
         th_ "Binding"
         th_ "Type"
         th_ "Visibility"
         th_ "Info"
         th_ "Value"
         th_ "Size"
      forM_ ss $ \s -> tr_ $ do
         td_ . toHtml $ show (symbolNameIndex s)
         td_ . toHtml $ show (symbolBinding s)
         td_ . toHtml $ show (symbolType s)
         td_ . toHtml $ show (symbolVisibility s)
         td_ . toHtml $ show (symbolInfo s)
         td_ . toHtml $ show (symbolValue s)
         td_ . toHtml $ show (symbolSize s)

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
css = toResponseBS (C.pack "text/css") (L.fromStrict $(embedFile "src/apps/Elf/style.css"))

