{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}

module Main where

import SystemInfoCmdLine (Options(..), getOptions)

import qualified Haskus.Arch.X86_64.ISA.Insn             as X86
import qualified Haskus.Arch.X86_64.ISA.Insns            as X86
import qualified Haskus.Arch.X86_64.ISA.OpcodeMaps       as X86
import qualified Haskus.Arch.X86_64.ISA.Encoding         as X86
import qualified Haskus.Arch.X86_64.ISA.Register         as X86
import qualified Haskus.Arch.X86_64.ISA.Operand          as X86
import qualified Haskus.Arch.X86_64.ISA.Memory           as X86
import qualified Haskus.Arch.X86_64.ISA.Size             as X86
import qualified Haskus.Arch.X86_64.ISA.Solver           as X86
import qualified Haskus.Arch.X86_64.ISA.Immediate        as X86
import Haskus.Arch.X86_64.ISA.Mode
import Haskus.Arch.X86_64.ISA.Solver
import Haskus.Arch.Common.Register
import Haskus.Arch.Common.Memory
import Haskus.Arch.Common.Immediate

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Bits
import Haskus.Utils.Embed.ByteString
import Haskus.Utils.Solver
import Haskus.Utils.Flow
import Haskus.Utils.Text (tshow)
import qualified Haskus.Utils.List as List

import qualified Haskus.Utils.Text as Text
import Paths_haskus_system
import Data.Version
import Control.Monad
import Text.Printf
import Network.Socket (withSocketsDo)
import Network.HTTP.Base (urlEncode)
import Numeric
import Happstack.Server
import Data.Char (toUpper)
import Data.Maybe
import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Data.Vector as V

import Haskus.Web.Html
import Haskus.Web.Page
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = withSocketsDo $ do

   opts <- getOptions

   server (nullConf {port = optport opts})


server :: Conf -> IO ()
server conf = do
   putStrLn (printf "Starting Web server at localhost:%d" (port conf))
   let
      defaultRep t = ok . toResponse . renderBS . tmplPage t
   simpleHTTP conf $ msum
      [ dir "css"  $ dir "style.css" $ ok css
      , dir "all"  $ nullDir >> defaultRep "List all" showAll
      , dir "maps" $ nullDir >> defaultRep "Maps"     showMaps
      , dir "insn" $ path $ \mnemo -> defaultRep mnemo (showInsnByMnemo mnemo)
      , dir "regs" $ nullDir >> defaultRep "Registers" showRegs
      , nullDir >> defaultRep "System info" showWelcome
      ]

css :: Response
css = toResponseBS (C.pack "text/css") (L.fromStrict $(embedBSFile "src/system-info/style.css"))

tmplPage :: String -> Html () -> Html ()
tmplPage title bdy = htmlPage opts bdy
   where
      opts = HtmlPageOpts
         { pageTitle      = toHtml <| "haskus-system " <> showVersion version <> (if null title then "" else " - " <> title)
         , pageIsWebApp   = False
         , pageOpenGraph  = Nothing
         , pageThemeColor = Nothing
         , pageCss        = [ "/css/style.css" ]
         , pageJquery     = True
         , pageScripts    = []
         , pageManifest   = Nothing
         , pageIcon       = Nothing
         , pageCustomHead = mempty
         }

-- | Welcoming screen
showWelcome :: Html ()
showWelcome = do
   h2_ "X86 Instructions"
   ul_ $ do
      li_ $ a_ [href_ "/all" ] "List all"
      li_ $ a_ [href_ "/maps"] "Tables"
   h2_ "X86 Registers"
   ul_ $ do
      li_ $ a_ [href_ "/regs"] "List all"

showInsnByMnemo :: String -> Html ()
showInsnByMnemo mnemo = do
   let is = filter (\i -> X86.insnMnemonic i == mnemo) X86.instructions
   forM_ is showInsn

-- | List all instructions
showAll :: Html ()
showAll = do
   let is = List.nub . fmap X86.insnMnemonic $ X86.instructions
   ul_ $ forM_ is $ \i ->
      li_ $ showMnemo i

-- | Show an instruction
showInsn :: X86.X86Insn -> Html ()
showInsn i = do
   h2_ $ toHtml $ X86.insnMnemonic i ++ " - " ++ X86.insnDesc i
   h3_ "Properties"
   ul_ $ forM_ (X86.insnProperties i) $ \p -> li_ (toHtml (show p))
   h3_ "Flags"
   ul_ $ forM_ (X86.insnFlags i) $ \f -> li_ (toHtml (show f))
   h3_ "Encodings"
   forM_ (X86.insnEncodings i) $ \e -> do
         case X86.encOpcodeEncoding e of
            X86.EncLegacy -> h4_ "Legacy encoding"
            X86.EncVEX    -> h4_ "VEX encoding"
         table_ [class_ "insn_table"] do
            tr_ $ do
               th_ "Mandatory prefix"
               th_ "Opcode map"
               th_ "Opcode"
               th_ "Properties"
               th_ "Operands"
               forM_ [1.. length(X86.encOperands e)] $ \x ->
                  th_ (toHtml (show x))
            forM_ (X86.encGenerateOpcodes e) $ \x -> do
               let
                  rev = fromMaybe False (testBit x <$> X86.encReversableBit e)
               showEnc x rev e
   hr_ []


myShowHex :: (Show a,Integral a) => Bool -> a -> String
myShowHex usePad x = pad ++ fmap toUpper (showHex x "")
   where
      pad = if usePad && x <= 0xF then "0" else ""

showPredicate :: X86Pred -> Html ()
showPredicate x = toHtml $ case x of
   ContextPred (Mode m)         -> modeName m
   ContextPred CS_D             -> "CS.D"
   ContextPred SS_B             -> "SS.B"
   PrefixPred Prefix66          -> "Prefix 66"
   PrefixPred Prefix67          -> "Prefix 67"
   PrefixPred PrefixW           -> "W"
   PrefixPred PrefixL           -> "L"
   InsnPred Default64OpSize     -> "Def64OpSize"
   InsnPred Force8bit           -> "Force 8-bit opcode bit"
   InsnPred SignExtendBit       -> "Sign-extend immediate"
   InsnPred FPUSizeBit          -> "FPU alternative size bit"
   InsnPred RegModRM            -> "ModRM.mod = 11b"
   EncodingPred PLegacyEncoding -> "Legacy encoding"
   EncodingPred PRexEncoding    -> "REX prefix"
   EncodingPred PVexEncoding    -> "VEX encoding"
   EncodingPred PXopEncoding    -> "XOP encoding"
   EncodingPred PEvexEncoding   -> "EVEX encoding"
   EncodingPred PMvexEncoding   -> "MVEX encoding"

showEnc :: Word8 -> Bool -> X86.Encoding -> Html ()
showEnc oc rv e = tr_ $ do
   let rowsp3 x = x `with` [rowspan_ "3"]
   rowsp3 $ case X86.encMandatoryPrefix e of
      Nothing -> td_ " "
      Just p  -> td_ (toHtml (show p))
   rowsp3 $ td_ (toHtml (show (X86.encOpcodeMap e)))
   rowsp3 $ td_ $ do
      toHtml (myShowHex True oc)
      case X86.encOpcodeFullExt e of
         Nothing -> return ()
         Just p  -> toHtml (" " ++ myShowHex True p)
      case X86.encOpcodeExt e of
         Nothing -> return ()
         Just p  -> toHtml (" /" ++ show p)
      case X86.encOpcodeLExt e of
         Nothing    -> return ()
         Just True  -> " L1"
         Just False -> " L0"
      case X86.encOpcodeWExt e of
         Nothing    -> return ()
         Just True  -> " W1"
         Just False -> " W0"

   rowsp3 $ td_ [ style_ "text-align:left; padding-right:1em;" ] do
      ul_ $ forM_ (X86.encProperties e) $ \p -> li_ (toHtml (show p))
   let 
      ops = X86.encOperands e
      rev = if rv then reverse else id
   th_ "Mode"
   forM_ ops $ \o -> td_ (toHtml (show (X86.opMode o)))
   tr_ $ do
      th_ "Type"
      forM_ (rev ops) $ \o -> td_ $ do
         let
            oracle :: PredOracle X86Pred
            oracle = makeOracle
                        [(InsnPred Default64OpSize, if | X86.DefaultOperandSize64 `elem` X86.encProperties e -> SetPred
                                                       | otherwise                                           -> UnsetPred)
                        ,(InsnPred Force8bit      , case X86.encNoForce8Bit e of
                                                      Nothing -> UnsetPred
                                                      Just t
                                                         | testBit oc t -> UnsetPred
                                                         | otherwise    -> SetPred)
                        ,(EncodingPred PLegacyEncoding, if | X86.isLegacyEncoding e -> SetPred
                                                           | otherwise              -> UnsetPred)
                        ,(EncodingPred PVexEncoding   , if | X86.isVexEncoding e    -> SetPred
                                                           | otherwise              -> UnsetPred)
                        ]

            preReduce :: (Predicated a, Pred a ~ X86Pred) => a -> a
            preReduce x = case reducePredicates oracle x of
                     Match     f -> liftTerminal f
                     DontMatch f -> f
                     _           -> error "Invalid predicated value"

            showOpFam = \case
               X86.T_Reg fam  -> showPredTable showRegFam (preReduce fam)
               X86.T_Mem fam  -> showPredTable showMemFam (preReduce fam)
               X86.T_Imm fam  -> showPredTable showImmFam (preReduce fam)
               t              -> toHtml (show t)

         showPredTable showOpFam (preReduce (X86.opFam o))

   tr_ $ do
      th_ "Encoding"
      forM_ (rev ops) $ \o -> td_ <| case X86.opStore o of
         X86.S_RM          -> "ModRM.rm"
         X86.S_Reg         -> "ModRM.reg"
         X86.S_Imm         -> "Immediate"
         X86.S_Imm8h       -> "Imm8 [7:4]"
         X86.S_Imm8l       -> "Imm8 [3:0]"
         X86.S_Implicit    -> "Implicit"
         X86.S_Vvvv        -> "VEX.vvvv"
         X86.S_OpcodeLow3  -> "Opcode [2:0]"

showMemFam :: X86.X86MemFamT -> Html ()
showMemFam (MemFam maddr _ty msz) = do
   -- toHtml (tshow ty) --TODO: show memory type?
   -- "@"
   fromMaybe "Memory" (fmap showAddrFam maddr)
   " { "
   fromMaybe "?" (fmap (toHtml . tshow) msz)
   " bits }"

showAddrFam :: X86.AddrFam -> Html ()
showAddrFam X86.AddrFam{..} = do
   forM_ addrFamSeg \seg -> do
      showSegFam seg
      ":"
   "["
   forM_ addrFamBase \r -> showReg r

   forM_ addrFamIndex \ifam -> do
      when (isJust addrFamBase) " + "
      showRegFam ifam
      forM_ addrFamIndexSize \sz -> do
         "["
         toHtml (tshow sz)
         "]"

   forM_ addrFamScale \case
      X86.Scale1 -> pure ()
      X86.Scale2 -> " * 2"
      X86.Scale4 -> " * 4"
      X86.Scale8 -> " * 8"

   forM_ addrFamDisp \dsp -> do
      when (isJust addrFamBase || isJust addrFamScale)
         " + "
      toHtml (tshow dsp)

   forM_ addrFamDispSize \dsz -> do
      when (isJust addrFamBase || isJust addrFamScale || isJust addrFamDisp)
         " + "
      "DISP"
      toHtml (tshow (X86.sizeInBits dsz))
   "]"

   forM_ addrFamAlign \al -> do
      " @ "
      toHtml (tshow al)
   


-- | Show segment family
showSegFam :: X86.SegFam -> Html ()
showSegFam (X86.FixedSeg r)       = span_ (showReg r)
showSegFam (X86.OverridableSeg r) = span_ [style_ "font-style: italic"] (showReg r)

showReg :: X86.X86Reg -> Html ()
showReg r = toHtml (X86.registerName r)

showImmFam :: X86.X86ImmFamT -> Html ()
showImmFam i = toHtml case i of
   ImmFam sz Nothing    Nothing _  -> tshow (X86.opSizeInBits sz) <> "-bit immediate"
   ImmFam sz (Just sz2) Nothing _  -> tshow (X86.opSizeInBits sz) <> "-bit immediate (sign-extended to " <> tshow (X86.opSizeInBits sz2) <> "-bit)"
   _ -> tshow i
         
showRegFam :: X86.X86RegFamT -> Html ()
showRegFam r = case r of
   RegFam (Singleton b)
          (Singleton i)
          (Singleton s)
          (Singleton off)
          sub
      -> toHtml (X86.registerName (Reg b i s off sub))
   RegFam (Singleton X86.GPR)
          (Any)
          (Singleton 8)
          (Singleton 0)
          _
      -> "Any 8-bit GPR (except ah,bh,ch,dh)"
   RegFam (Singleton X86.GPR)
          (Any)
          (Singleton s)
          (Singleton 0)
          _
      -> toHtml ("Any " ++ show s ++ "-bit GPR")
   RegFam (Singleton X86.Seg)
          (Any)
          (Singleton 16)
          (Singleton 0)
          _
      -> "Any segment register"
   RegFam (Singleton X86.GPR)
          (NoneOf [4,5,6,7])
          (Singleton 8)
          (OneOf [0,8])
          _
      -> "Any legacy 8-bit GPR"
   r' -> toHtml (show r')

showPredTable :: forall a.
   ( Pred a ~ X86Pred
   , Eq (PredTerm a)
   , Ord (PredTerm a)
   , Show a
   , Eq a
   , Predicated a
   ) => (PredTerm a -> Html ()) -> a -> Html ()
showPredTable showValue a = do
   case createPredicateTable a (null . checkOracle False) True of
      Left r   -> showValue r
      Right [] -> toHtml ("Error: empty table! " ++ show a)
      Right rs -> do
         let modePreds = filter X86.isModePredicate (getPredicates a)
         if not (null modePreds)
            then do
               let
                  -- simplified predicated value for each mode
                  modeValues = [ (mode,value)
                               | mode <- modePreds
                               , let oracle = makeOracleX86 [(mode,SetPred)]
                               , let value = simplifyPredicates oracle a
                               ]
                  -- group modes having the same predicated value
                  groupedModes = List.groupOn snd modeValues
                                 ||> \x -> (fmap fst x, snd (head x))
               forM_ groupedModes \(modes,val) -> div_ do
                  intersperseM_ " / " modes \mode -> case mode of
                     ContextPred (Mode m) -> case m of
                        LongMode x -> case x of
                           Long64bitMode     -> "64-bit mode"
                           CompatibilityMode -> "Compat mode"
                        LegacyMode x -> case x of
                           ProtectedMode     -> "Protected mode"
                           Virtual8086Mode   -> "Virtual 8086 mode"
                           RealMode          -> "Real mode"
                     _ -> pure ()
                  showPredTable showValue val
            else showPredicateTable @a showValue (getPredicates a) rs

showPredicateTable :: forall a.
   ( Pred a ~ X86Pred
   , Ord (PredTerm a)
   , Eq (PredTerm a)
   , Predicated a
   ) => (PredTerm a -> Html ())
     -> [Pred a]
     -> [(PredOracle (Pred a),PredTerm a)]
     -> Html ()
showPredicateTable showValue preds rs =
   table_ [class_ "predicatetable"] do
      let ps = List.sort preds
      tr_ $ do
         forM_ ps $ \p -> th_ $ toHtml (showPredicate p)
         th_ "Value"
      forM_ (List.groupOn snd (List.sortOn snd rs)) $ \ws ->
         forM_ ws $ \(oracle,v) -> tr_ $ do
            forM_ ps $ \p -> case predState oracle p of
               UnsetPred   -> td_ "0"
               SetPred     -> td_ "1"
               UndefPred   -> td_ "*"
               InvalidPred -> td_ "X"
            td_ $ showValue v



showMaps :: Html ()
showMaps = do
   h1_ "Legacy encodings"
   showOpcodeMap "Primary opcode map"   (X86.MapLegacy X86.MapPrimary)
   showOpcodeMap "Secondary opcode map" (X86.MapLegacy X86.Map0F)
   showOpcodeMap "0F38 opcode map"      (X86.MapLegacy X86.Map0F38)
   showOpcodeMap "0F3A opcode map"      (X86.MapLegacy X86.Map0F3A)
   showOpcodeMap "3DNow! opcode map"    (X86.MapLegacy X86.Map3DNow)

   h1_ "VEX encodings"
   showOpcodeMap "VEX 1 opcode map" (X86.MapVex 1)
   showOpcodeMap "VEX 2 opcode map" (X86.MapVex 2)
   showOpcodeMap "VEX 3 opcode map" (X86.MapVex 3)

showOpcodeMap :: String -> X86.OpcodeMap -> Html ()
showOpcodeMap s ocm = do
   case Map.lookup ocm X86.opcodeMaps of
      Just m -> do
         h2_ (toHtml s)
         showMap m
      Nothing -> return ()


showMap :: V.Vector [X86.MapEntry] -> Html ()
showMap v =
   table_ [class_ "opcode_map"] do
      tr_ $ do
         th_ "Nibble"
         forM_ [0..15] $ \x -> th_ (toHtml (myShowHex False (x :: Int)))
      forM_ [0..15] $ \l -> tr_ $ do
         th_ (toHtml (myShowHex False l))
         forM_ [0..15] $ \h -> do
            let is = fmap X86.entryInsn $ v V.! (l `shiftL` 4 + h)
            td_ $ sequence_
               $ List.intersperse ", "
               $ fmap showMnemo 
               $ List.nub
               $ fmap X86.insnMnemonic is

showMnemo :: String -> Html ()
showMnemo mnemo = do
   a_ [ href_ <| "/insn/" <> Text.pack (urlEncode mnemo)
      ] (toHtml mnemo)

-- | Show registers
showRegs :: Html ()
showRegs = do

   h1_ "Registers"

   let
      showMode :: X86Mode -> Html ()
      showMode mode = do
         h2_ (toHtml (show mode))
         let
            regs  = Set.toList $ X86.getModeRegisters mode
            f x y = registerBank x == registerBank y
            regs' = List.groupBy f regs

         ul_ $ forM_ regs' $ \rs -> li_ $
            toHtml (mconcat (List.intersperse "," (fmap X86.registerName rs)))

   showMode (LongMode Long64bitMode)
   showMode (LongMode CompatibilityMode)
   showMode (LegacyMode ProtectedMode)
   showMode (LegacyMode Virtual8086Mode)
   showMode (LegacyMode RealMode)
