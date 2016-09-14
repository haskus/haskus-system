{-# LANGUAGE LambdaCase, TemplateHaskell #-}

import CmdLine (Options(..), getOptions)

import qualified ViperVM.Arch.X86_64.ISA.Insn       as X86
import qualified ViperVM.Arch.X86_64.ISA.Insns      as X86
import qualified ViperVM.Arch.X86_64.ISA.OpcodeMaps as X86
import qualified ViperVM.Arch.X86_64.ISA.Encoding   as X86
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Bits
import ViperVM.Utils.Embed

import Paths_ViperVM
import Data.Version
import Control.Monad
import Text.Printf
import Network.Socket (withSocketsDo)
import Network.HTTP.Base (urlEncode)
import Numeric
import Happstack.Server
import Data.Char (toUpper)
import Data.Maybe
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Vector as V

import Text.Blaze.Html5 ((!), toHtml, docTypeHtml, Html, toValue)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = withSocketsDo $ do

   opts <- getOptions

   server (nullConf {port = optport opts})


server :: Conf -> IO ()
server conf = do
   putStrLn (printf "Starting Web server at localhost:%d" (port conf))
   simpleHTTP conf $ msum
      [ 
      
        -- CSS 
        dir "css" $ dir "style.css" $ ok css

      , dir "all" $ nullDir >> (ok . toResponse . appTemplate "List all" $ showAll)
      , dir "maps" $ nullDir >> (ok . toResponse . appTemplate "Maps" $ showMaps)

      , dir "insn" $ path $ \mnemo -> (ok . toResponse . appTemplate mnemo $ showInsnByMnemo mnemo)

        -- Show welcome screen
      , nullDir >> (ok . toResponse . appTemplate "Welcome" $ showWelcome)
      ]

css :: Response
css = toResponseBS (C.pack "text/css") (L.fromStrict $(embedFile "src/apps/X86Web/style.css"))

-- | Template of all pages
appTemplate :: String -> Html -> Html
appTemplate title bdy = docTypeHtml $ do
   H.head $ do
      H.title (toHtml "ViperVM X86 instructions")
      H.meta ! A.httpEquiv (toValue "Content-Type")
             ! A.content   (toValue "text/html;charset=utf-8")
      H.link ! A.rel       (toValue "stylesheet") 
             ! A.type_     (toValue "text/css")
             ! A.href      (toValue "/css/style.css")
   H.body $ do
      H.div (toHtml $ "ViperVM " ++ showVersion version ++ " / " ++ title)
         ! A.class_ (toValue "headtitle")
      bdy

-- | Welcoming screen
showWelcome :: Html
showWelcome = do
   H.h2 (toHtml "Instructions")
   H.ul $ do
      H.li $ H.a (toHtml "List all") ! A.href (toValue "/all")
      H.li $ H.a (toHtml "Tables")   ! A.href (toValue "/maps")

showInsnByMnemo :: String -> Html
showInsnByMnemo mnemo = do
   let is = filter (\i -> X86.insnMnemonic i == mnemo) X86.instructions
   forM_ is showInsn

-- | List all instructions
showAll :: Html
showAll = do
   let is = List.nub . fmap X86.insnMnemonic $ X86.instructions
   H.ul $ forM_ is $ \i ->
      H.li $ showMnemo i

-- | Show an instruction
showInsn :: X86.X86Insn -> Html
showInsn i = do
   H.h2 $ toHtml $ X86.insnMnemonic i ++ " - " ++ X86.insnDesc i
   H.h3 (toHtml "Properties")
   H.ul $ forM_ (X86.insnProperties i) $ \p -> H.li (toHtml (show p))
   H.h3 (toHtml "Flags")
   H.ul $ forM_ (X86.insnFlags i) $ \f -> H.li (toHtml (show f))
   H.h3 (toHtml "Encodings")
   forM_ (X86.insnEncodings i) $ \e -> do
         case X86.encOpcodeEncoding e of
            X86.EncLegacy -> H.h4 (toHtml "Legacy encoding")
            X86.EncVEX    -> H.h4 (toHtml "VEX encoding")
         H.table ( do
            H.tr $ do
               H.th (toHtml "Mandatory prefix")
               H.th (toHtml "Opcode map")
               H.th (toHtml "Opcode")
               H.th (toHtml "Properties")
               H.th (toHtml "Operands")
               forM_ [1.. length(X86.encOperands e)] $ \x ->
                  H.th (toHtml (show x))
            forM_ (X86.encGenerateOpcodes e) $ \x -> do
               let
                  rev = fromMaybe False (testBit x <$> X86.encReversableBit e)
               showEnc x rev e
            ) ! A.class_ (toValue "insn_table")
   H.hr


myShowHex :: (Show a,Integral a,Ord a,Num a) => Bool -> a -> String
myShowHex usePad x = pad ++ fmap toUpper (showHex x "")
   where
      pad = if usePad && x <= 0xF then "0" else ""



showEnc :: Word8 -> Bool -> X86.Encoding -> Html
showEnc oc rv e = H.tr $ do
   let rowsp3 x = x ! A.rowspan (toValue (3 :: Int))
   rowsp3 $ case X86.encMandatoryPrefix e of
      Nothing -> H.td (toHtml " ")
      Just p  -> H.td (toHtml (show p))
   rowsp3 $ H.td (toHtml (show (X86.encOpcodeMap e)))
   rowsp3 $ H.td $ do
      toHtml (myShowHex True oc)
      case X86.encOpcodeFullExt e of
         Nothing -> return ()
         Just p  -> toHtml (" " ++ myShowHex True p)
      case X86.encOpcodeExt e of
         Nothing -> return ()
         Just p  -> toHtml (" /" ++ show p)
      case X86.encOpcodeLExt e of
         Nothing    -> return ()
         Just True  -> toHtml " L1"
         Just False -> toHtml " L0"
      case X86.encOpcodeWExt e of
         Nothing    -> return ()
         Just True  -> toHtml " W1"
         Just False -> toHtml " W0"

   rowsp3 $ H.td (H.ul $ forM_ (X86.encProperties e) $ \p -> H.li (toHtml (show p)))
      ! A.style (toValue "text-align:left; padding-right:1em;")
   let 
      ops = X86.encOperands e
      rev = if rv then reverse else id
   H.th (toHtml "Mode")
   forM_ ops $ \o -> H.td (toHtml (show (X86.opMode o)))
   H.tr $ do
      H.th (toHtml "Type")
      forM_ (rev ops) $ \o -> H.td (toHtml (show (X86.opType o)))
   H.tr $ do
      H.th (toHtml "Encoding")
      forM_ (rev ops) $ \o -> H.td . toHtml $ case X86.opEnc o of
         X86.RM          -> "ModRM.rm"
         X86.Reg         -> "ModRM.reg"
         X86.Imm         -> "Immediate"
         X86.Imm8h       -> "Imm8 [7:4]"
         X86.Imm8l       -> "Imm8 [3:0]"
         X86.Implicit    -> "Implicit"
         X86.Vvvv        -> "VEX.vvvv"
         X86.OpcodeLow3  -> "Opcode [2:0]"


showMaps :: Html
showMaps = do
   H.h1 (toHtml "Legacy encodings")
   showOpcodeMap "Primary opcode map"   (X86.MapLegacy X86.MapPrimary)
   showOpcodeMap "Secondary opcode map" (X86.MapLegacy X86.Map0F)
   showOpcodeMap "0F38 opcode map"      (X86.MapLegacy X86.Map0F38)
   showOpcodeMap "0F3A opcode map"      (X86.MapLegacy X86.Map0F3A)
   showOpcodeMap "3DNow! opcode map"    (X86.MapLegacy X86.Map3DNow)

   H.h1 (toHtml "VEX encodings")
   showOpcodeMap "VEX 1 opcode map" (X86.MapVex 1)
   showOpcodeMap "VEX 2 opcode map" (X86.MapVex 2)
   showOpcodeMap "VEX 3 opcode map" (X86.MapVex 3)

showOpcodeMap :: String -> X86.OpcodeMap -> Html
showOpcodeMap s ocm = do
   case Map.lookup ocm X86.opcodeMaps of
      Just m -> do
         H.h2 (toHtml s)
         showMap m
      Nothing -> return ()


showMap :: V.Vector [X86.MapEntry] -> Html
showMap v = (H.table $ do
   H.tr $ do
      H.th (toHtml "Nibble")
      forM_ [0..15] $ \x -> H.th (toHtml (myShowHex False (x :: Int)))
   forM_ [0..15] $ \l -> H.tr $ do
      H.th (toHtml (myShowHex False l))
      forM_ [0..15] $ \h -> do
         let is = fmap X86.entryInsn $ v V.! (l `shiftL` 4 + h)
         H.td $ sequence_
            $ List.intersperse (toHtml ", ")
            $ fmap showMnemo 
            $ List.nub
            $ fmap X86.insnMnemonic is
   ) ! A.class_ (toValue "opcode_map")

showMnemo :: String -> Html
showMnemo mnemo = do
   H.a (toHtml mnemo)
      ! A.href (toValue ("/insn/" ++ urlEncode mnemo))
