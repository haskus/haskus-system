{-# LANGUAGE LambdaCase, TemplateHaskell #-}

import CmdLine (Options(..), getOptions)

import qualified ViperVM.Arch.X86_64.Assembler.Insns as X86

import Paths_ViperVM
import Data.FileEmbed
import Data.Version
import Control.Monad
import Text.Printf
import Network.Socket (withSocketsDo)
import Numeric
import Happstack.Server
import Data.Word
import Data.Bits

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
showWelcome = forM_ X86.instructions $ \i -> do
   H.h2 $ toHtml $ X86.iMnemonic i ++ " - " ++ X86.iDesc i
   H.h3 (toHtml "Properties")
   H.ul $ forM_ (X86.iProperties i) $ \p -> H.li (toHtml (show p))
   H.h3 (toHtml "Flags")
   H.ul $ forM_ (X86.iFlags i) $ \f -> H.li (toHtml (show f))
   H.h3 (toHtml "Encodings")
   forM_ (X86.iEncoding i) $ \case
      X86.LegacyEncoding e -> do
         H.h4 (toHtml "Legacy encoding")
         H.table (do
            H.tr $ do
               H.th (toHtml "Mandatory prefix")
               H.th (toHtml "Opcode map")
               H.th (toHtml "Opcode")
               H.th (toHtml "Opcode extension")
               H.th (toHtml "Properties")
               H.th (toHtml "Operands")
            let
               sz = X86.sizable            (X86.legEncOpcodeFields e)
               rv = X86.reversable         (X86.legEncOpcodeFields e)
               se = X86.signExtendableImm8 (X86.legEncOpcodeFields e)
               oc = X86.legEncOpcode e
               testOp = \case
                  X86.E_OpReg -> True
                  _           -> False
               op = any testOp $ fmap X86.opEnc (X86.legEncParams e)
            showLegEnc oc False False False e
            case rv of
               Nothing -> return ()
               Just x  -> showLegEnc (setBit oc x) True False False e
            case (sz,se) of
               (Nothing,Nothing) -> return ()
               (Nothing,Just _)  -> error "Invalid opcode fields"
               (Just x,Nothing)  -> showLegEnc (setBit oc x) False True False e
               (Just x,Just y)   -> do
                  showLegEnc (setBit oc x) False True False e
                  showLegEnc (setBit (setBit oc x) y) False True True e
            -- operand in the last 3 bits of the opcode
            case op of
               False -> return ()
               True  -> forM_ [1..7] $ \x -> showLegEnc (oc+x) False False False e


            ) ! A.class_ (toValue "insn_table")
      X86.VexEncoding    e -> do
         H.h4 (toHtml "VEX encoding")
         H.table (do
            H.tr $ do
               H.th (toHtml "Mandatory prefix")
               H.th (toHtml "Opcode map")
               H.th (toHtml "Opcode")
               H.th (toHtml "LW")
               H.th (toHtml "Operands")
            H.tr $ do
               case X86.vexEncMandatoryPrefix e of
                  Nothing -> H.td (toHtml " ")
                  Just p  -> H.td (toHtml (showHex p ""))
               H.td (toHtml (show (X86.vexEncOpcodeMap e)))
               H.td (toHtml (showHex (X86.vexEncOpcode e) ""))
               H.td (toHtml (show (X86.vexEncLW e)))
               let 
                  ops = X86.vexEncParams e
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
                        X86.E_ModRM     -> "ModRM.rm"
                        X86.E_ModReg    -> "ModRM.reg"
                        X86.E_Imm       -> "Imm"
                        X86.E_Imm8_7_4  -> "Imm8 [7:4]"
                        X86.E_Imm8_3_0  -> "Imm8 [3:0]"
                        X86.E_Implicit  -> "Implicit"
                        X86.E_VexV      -> "VEX.vvvv"
                        X86.E_OpReg     -> error "Unsupported OpReg for VEX encoding"
                  ) ! A.class_ (toValue "insn_table")

            ) ! A.class_ (toValue "insn_table")
   H.hr




showLegEnc :: Word8 -> Bool -> Bool -> Bool -> X86.LegEnc -> Html
showLegEnc oc rv sz se e = H.tr $ do
   case X86.legEncMandatoryPrefix e of
      Nothing -> H.td (toHtml " ")
      Just p  -> H.td (toHtml (showHex p ""))
   H.td (toHtml (show (X86.legEncOpcodeMap e)))
   H.td (toHtml (showHex oc ""))
   case X86.legEncOpcodeExt e of
      Nothing -> H.td (toHtml " ")
      Just p  -> H.td (toHtml ("/" ++ (showHex p "")))
   H.td (toHtml (show (X86.legEncProperties e)))
   let 
      ops = X86.legEncParams e
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
            X86.E_ModRM     -> "ModRM.rm"
            X86.E_ModReg    -> "ModRM.reg"
            X86.E_Imm       -> case (sz,se) of
               (False,_)    -> "Imm8"
               (True,False) -> "ImmN"
               (True,True)  -> "Sign-extended Imm8"
            X86.E_Imm8_7_4  -> "Imm8 [7:4]"
            X86.E_Imm8_3_0  -> "Imm8 [3:0]"
            X86.E_Implicit  -> "Implicit"
            X86.E_VexV      -> "VEX.vvvv"
            X86.E_OpReg     -> "Opcode [2:0]"
      ) ! A.class_ (toValue "insn_table")
