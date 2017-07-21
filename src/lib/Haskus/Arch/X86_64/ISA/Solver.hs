{-# LANGUAGE LambdaCase #-}

-- | Constraint solver
module Haskus.Arch.X86_64.ISA.Solver
   ( X86Rule
   , X86Constraint
   -- * Predicates
   , X86Pred (..)
   , ContextPred (..)
   , InsnPred (..)
   , PrefixPred (..)
   , EncodingPred (..)
   , X86PredOracle
   , PredError (..)
   , checkOracle
   , pPrefix
   , pMode
   , pMode64bit
   , pCS_D
   , pLegacy8bitRegs
   , pOverriddenOperationSize64
   , pForce8bit
   , pOverriddenAddressSize
   -- * Rules
   , rDefaultOperationSize
   , rDefaultAddressSize
   , rOverriddenOperationSize
   , rOverriddenOperationSize64
   , rOverriddenAddressSize
   )
where

import Haskus.Arch.X86_64.ISA.Size
import Haskus.Arch.X86_64.ISA.Mode
import Haskus.Utils.Solver
import Haskus.Utils.Maybe

-- | Context predicate
data ContextPred
   = Mode X86Mode -- ^ Execution mode
   | CS_D         -- ^ CS.D segment descriptor flag
   | SS_B         -- ^ SS.B segment descriptor flag
   deriving (Show,Eq,Ord)

-- | Prefix predicates
data PrefixPred
   = Prefix66     -- ^ 0x66
   | Prefix67     -- ^ 0x67
   | PrefixW      -- ^ W bit
   | PrefixL      -- ^ L bit
   deriving (Show,Eq,Ord)

-- | Instruction specific predicates
data InsnPred
   = Default64OpSize    -- ^ Instruction defaulting to 64-bit size
--   | ModRM_mod ValidMod -- ^ Required ModRM.mod contents
   | Force8bit          -- ^ Instruction having a set Force8Bit bit in the opcode
   deriving (Show,Eq,Ord)

-- | Encoding predicates
data EncodingPred
   = PLegacyEncoding -- ^ Legacy encoding
   | PRexEncoding    -- ^ Legacy encoding with REX prefix
   | PVexEncoding    -- ^ VEX encoding
   | PXopEncoding    -- ^ XOP encoding
   | PEvexEncoding   -- ^ EVEX encoding
   | PMvexEncoding   -- ^ MVEX encoding
   deriving (Show,Eq,Ord)

-- | All the predicats
data X86Pred
   = ContextPred ContextPred
   | PrefixPred PrefixPred
   | EncodingPred EncodingPred
   | InsnPred InsnPred
   deriving (Show,Eq,Ord)

type X86Rule a     = Rule String X86Pred a
type X86Constraint = Constraint String X86Pred

-----------------------------------------------------
-- Predicates
-----------------------------------------------------

-- | Predicate oracle
type X86PredOracle = PredOracle X86Pred

data PredError
   = PredImply [(X86Pred,PredState)] [(X86Pred,PredState)]
   | PredIncompatible [(X86Pred,PredState)]
   deriving (Show)

-- | Check an oracle, return a list of incompatible predicates
checkOracle :: X86PredOracle -> [PredError]
checkOracle oracle =
      (fmap PredIncompatible (mapMaybe checkCompat incompatible))
      ++ (fmap (uncurry PredImply) (mapMaybe checkImply implies))
   where
      predAll is        = all (\(p,s) -> predIs oracle p s) is
      predAllOrUndef is = all (\(p,s) -> case predState oracle p of
                                          UndefPred -> True
                                          s'        -> s == s'
                             ) is

      checkCompat is = 
         if predAll is
            then Just is
            else Nothing

      checkImply (cs,rs) =
         if predAll cs && not (predAllOrUndef rs)
            then Just (cs,rs)
            else Nothing

      implies =
         [
           -- CS.D doesn't make sense in real-mode and virtual 8086 mode
            (  [ (ContextPred (Mode (LegacyMode RealMode))       , SetPred)]
            ,  [ (ContextPred CS_D                               , UndefPred)]
            )
         ,  (  [ (ContextPred (Mode (LegacyMode Virtual8086Mode)), SetPred)]
            ,  [ (ContextPred CS_D                               , UndefPred)]
            )

         ,  -- REX prefix only valid with a legacy encoding
            (  [(EncodingPred PRexEncoding   , SetPred)]
            ,  [(EncodingPred PLegacyEncoding, SetPred)]
            )

         , -- CS.D can't be 1 in long 64-bit mode.
            (  [ (ContextPred (Mode (LongMode Long64bitMode)), SetPred)]
            ,  [ (ContextPred CS_D                           , UnsetPred)]
            )

         , -- W doesn't make sense in real-mode and virtual 8086 mode
            ( [ (PrefixPred PrefixW                             , SetPred)]
            , [ (ContextPred (Mode (LegacyMode RealMode))       , UndefPred)]
            )
         ,  ( [ (PrefixPred PrefixW                             , SetPred)]
            , [ (ContextPred (Mode (LegacyMode Virtual8086Mode)), UndefPred)]
            )

         , -- L doesn't make sense in real-mode and virtual 8086 mode
            ( [ (PrefixPred PrefixL                             , SetPred)]
            , [ (ContextPred (Mode (LegacyMode RealMode))       , UndefPred)]
            )
         ,  ( [ (PrefixPred PrefixL                             , SetPred)]
            , [ (ContextPred (Mode (LegacyMode Virtual8086Mode)), UndefPred)]
            )

         ]

      incompatible =
         -- execution modes are incompatible
         exclusive (fmap (ContextPred . Mode) allModes)

         -- encodings are incompatible
         ++ exclusive (fmap EncodingPred encodings)

      exclusive []     = []
      exclusive [_]    = []
      exclusive (x:xs) = [[(x,SetPred),(y,SetPred)] | y <- xs] ++ exclusive xs

      encodings =   [ PLegacyEncoding
                    , PVexEncoding
                    , PXopEncoding
                    , PEvexEncoding
                    , PMvexEncoding
                    ]


-- | Allow the use of legacy 8-bit AH,BH,CH,DH registers
pLegacy8bitRegs :: X86Constraint
pLegacy8bitRegs = And [ Predicate (EncodingPred PLegacyEncoding)
                      , Not (Predicate (EncodingPred PRexEncoding))
                      ]

-- | 64-bit long mode predicate
pMode64bit :: X86Constraint
pMode64bit = pMode (LongMode Long64bitMode)

-- | Exclusive mode predicate
pMode :: X86Mode -> X86Constraint
pMode = Predicate . ContextPred . Mode
         

-- | CS.D flag
pCS_D :: X86Constraint
pCS_D = Predicate (ContextPred CS_D)

-- | Prefix predicate
pPrefix :: PrefixPred -> X86Constraint
pPrefix = Predicate . PrefixPred

-- | Overriden 64-bit operation size predicate
pOverriddenOperationSize64 :: OperandSize -> X86Constraint
pOverriddenOperationSize64 t = rOverriddenOperationSize64 `evalsTo` t

-- | Force 8-bit operand size
pForce8bit :: X86Constraint
pForce8bit = Predicate (InsnPred Force8bit)

-- | Overriden address size predicate
pOverriddenAddressSize :: AddressSize -> X86Constraint
pOverriddenAddressSize t = rOverriddenAddressSize `evalsTo` t

-----------------------------------------------------
-- Predicates
-----------------------------------------------------

-- | Default operation size (DOS)
rDefaultOperationSize :: X86Rule OperandSize
rDefaultOperationSize = NonTerminal
      [ (pMode (LegacyMode RealMode)           , Terminal OpSize16)
      , (pMode (LegacyMode Virtual8086Mode)    , Terminal OpSize16)
      , (pMode (LegacyMode ProtectedMode)      , s16o32)
      , (pMode (LongMode   CompatibilityMode)  , s16o32)
      , (pMode (LongMode   Long64bitMode)      , s32oFail)
      ]
   where
      s16o32   = NonTerminal
                  [ (Not pCS_D, Terminal OpSize16)
                  , (pCS_D    , Terminal OpSize32)
                  ]
      s32oFail = NonTerminal
                  [ (Not pCS_D, Terminal OpSize32)
                  ]

-- | Default address size (DAS)
rDefaultAddressSize :: X86Rule AddressSize
rDefaultAddressSize = NonTerminal
      [ (pMode (LegacyMode RealMode)           , Terminal AddrSize16)
      , (pMode (LegacyMode Virtual8086Mode)    , Terminal AddrSize16)
      , (pMode (LegacyMode ProtectedMode)      , s16o32)
      , (pMode (LongMode   CompatibilityMode)  , s16o32)
      , (pMode (LongMode   Long64bitMode)      , s32oFail)
      ]
   where
      s16o32   = NonTerminal
                  [ (Not pCS_D, Terminal AddrSize16)
                  , (pCS_D    , Terminal AddrSize32)
                  ]
      s32oFail = NonTerminal
                  [ (Not pCS_D, Terminal AddrSize64)
                  ]

-- | Overridden operation size (OOS)
rOverriddenOperationSize :: X86Rule OperandSize
rOverriddenOperationSize = NonTerminal
      [ ((pPrefix Prefix66)      , orderedNonTerminal p66)
      , ((Not (pPrefix Prefix66)), rDefaultOperationSize)
      ]
   where
      p66 = [ (rDefaultOperationSize `evalsTo` OpSize16, Terminal OpSize32)
            , (rDefaultOperationSize `evalsTo` OpSize32, Terminal OpSize16)
            ]

-- | Overridden address size (OAS)
rOverriddenAddressSize :: X86Rule AddressSize
rOverriddenAddressSize = NonTerminal
      [ ((pPrefix Prefix67)      , orderedNonTerminal p67)
      , ((Not (pPrefix Prefix67)), rDefaultAddressSize)
      ]
   where
      p67 = [ (rDefaultAddressSize `evalsTo` AddrSize16, Terminal AddrSize32)
            , (rDefaultAddressSize `evalsTo` AddrSize32, Terminal AddrSize16)
            , (rDefaultAddressSize `evalsTo` AddrSize64, Terminal AddrSize32)
            ]

-- | Overriden operation size for 64-bit
--
-- Support W prefix and default operation size set to 64-bit in 64-bit mode.
rOverriddenOperationSize64 :: X86Rule OperandSize
rOverriddenOperationSize64 = orderedNonTerminal
      [ (Not pMode64bit, rOverriddenOperationSize)
      , (Or [ Predicate (InsnPred Default64OpSize)
            , Predicate (PrefixPred PrefixW)
            ], Terminal OpSize64)
      , (CBool True, rOverriddenOperationSize)
      ]
