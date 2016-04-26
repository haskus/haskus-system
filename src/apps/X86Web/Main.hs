{-# LANGUAGE LambdaCase, TemplateHaskell #-}

import CmdLine (Options(..), getOptions)

import qualified ViperVM.Arch.X86_64.Assembler.Insns as X86
import qualified ViperVM.Arch.X86_64.Assembler.Tables as X86

import Paths_ViperVM
import Data.FileEmbed
import Data.Version
import Control.Monad
import Text.Printf
import Network.Socket (withSocketsDo)
import Network.HTTP.Base (urlEncode)
import Numeric
import Happstack.Server
import Data.Word
import Data.Bits
import qualified Data.List as List
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
   forM_ (X86.insnEncodings i) $ \case
      e@X86.LegacyEncoding {} -> do
         H.h4 (toHtml "Legacy encoding")
         H.table (do
            H.tr $ do
               H.th (toHtml "Mandatory prefix")
               H.th (toHtml "Opcode map")
               H.th (toHtml "Opcode")
               H.th (toHtml "Properties")
               H.th (toHtml "Operands")
            forM_ (X86.getLegacyOpcodes e) $ \x ->
               showLegEnc (X86.fgOpcode x) (X86.fgReversed x) (X86.fgSized x) (X86.fgSignExtended x) e


            ) ! A.class_ (toValue "insn_table")
      e@X86.VexEncoding {} -> do
         H.h4 (toHtml "VEX encoding")
         H.table (do
            H.tr $ do
               H.th (toHtml "Mandatory prefix")
               H.th (toHtml "Opcode map")
               H.th (toHtml "Opcode")
               H.th (toHtml "LW")
               H.th (toHtml "Operands")
            H.tr $ do
               case X86.vexMandatoryPrefix e of
                  Nothing -> H.td (toHtml " ")
                  Just p  -> H.td (toHtml (showHex p ""))
               H.td (toHtml (show (X86.vexOpcodeMap e)))
               H.td (toHtml (showHex (X86.vexOpcode e) ""))
               H.td (toHtml (show (X86.vexLW e)))
               let 
                  ops = X86.vexParams e
               H.td $ (H.table $ do
                  H.tr $ do
                     H.th (toHtml "Mode")
                     forM_ ops $ \o -> H.td (toHtml (show (X86.opMode o)))
                  H.tr $ do
                     H.th (toHtml "Type")
                     forM_ ops $ \o -> H.td (toHtml (show (X86.opType o)))
                  H.tr $ do
                     H.th (toHtml "Encoding")
                     forM_ ops $ \o -> H.td . toHtml $ case X86.opEnc o of
                        X86.RM          -> "ModRM.rm"
                        X86.Reg         -> "ModRM.reg"
                        X86.Imm         -> "Imm"
                        X86.Imm8h       -> "Imm8 [7:4]"
                        X86.Imm8l       -> "Imm8 [3:0]"
                        X86.Implicit    -> "Implicit"
                        X86.Vvvv        -> "VEX.vvvv"
                        X86.OpcodeLow3  -> error "Unsupported OpReg for VEX encoding"
                  ) ! A.class_ (toValue "insn_table")

            ) ! A.class_ (toValue "insn_table")
   H.hr




showLegEnc :: Word8 -> Bool -> Bool -> Bool -> X86.Encoding -> Html
showLegEnc oc rv sz se e = H.tr $ do
   case X86.legacyMandatoryPrefix e of
      Nothing -> H.td (toHtml " ")
      Just p  -> H.td (toHtml (showHex p ""))
   H.td (toHtml (show (X86.legacyOpcodeMap e)))
   H.td $ do
      toHtml (showHex oc "")
      case X86.legacyOpcodeExt e of
         Nothing -> return ()
         Just p  -> toHtml (" /" ++ (showHex p ""))
   H.td (toHtml (show (X86.legacyProperties e)))
   let 
      ops = X86.legacyParams e
      rev = if rv then reverse else id
   H.td $ (H.table $ do
      H.tr $ do
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
            X86.Imm         -> case (sz,se) of
               (False,_)    -> "Imm8"
               (True,False) -> "ImmN"
               (True,True)  -> "Sign-extended Imm8"
            X86.Imm8h       -> "Imm8 [7:4]"
            X86.Imm8l       -> "Imm8 [3:0]"
            X86.Implicit    -> "Implicit"
            X86.Vvvv        -> "VEX.vvvv"
            X86.OpcodeLow3  -> "Opcode [2:0]"
      ) ! A.class_ (toValue "insn_table")


showMaps :: Html
showMaps = do
   H.h1 (toHtml "Legacy encodings")
   H.h2 (toHtml "Primary opcode map")
   showMap X86.opcodeMapPrimary
   H.h2 (toHtml "Secondary opcode map")
   showMap X86.opcodeMap0F
   H.h2 (toHtml "0F38 opcode map")
   showMap X86.opcodeMap0F38
   H.h2 (toHtml "0F3A opcode map")
   showMap X86.opcodeMap0F3A
   H.h2 (toHtml "3DNow! opcode map")
   showMap X86.opcodeMap3DNow

   H.h1 (toHtml "VEX encodings")
   H.h2 (toHtml "VEX 1 opcode map")
   showMap X86.opcodeMapVex1
   H.h2 (toHtml "VEX 2 opcode map")
   showMap X86.opcodeMapVex2
   H.h2 (toHtml "VEX 3 opcode map")
   showMap X86.opcodeMapVex3


showMap :: V.Vector [(X86.Encoding,X86.X86Insn)] -> Html
showMap v = (H.table $ do
   H.tr $ do
      H.th (toHtml "Nibble")
      forM_ [0..15] $ \x -> H.th (toHtml (showHex (x :: Int) ""))
   forM_ [0..15] $ \h -> H.tr $ do
      H.th (toHtml (showHex h ""))
      forM_ [0..15] $ \l -> do
         let is = fmap snd $ v V.! (l `shiftL` 4 + h)
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
