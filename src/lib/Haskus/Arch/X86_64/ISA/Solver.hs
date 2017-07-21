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
   , pCheck
   , pPrefix
   , pMode
   , pModeEx
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
import Haskus.Utils.Flow

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

-- | Architecture invariant predicate
pCheck :: Eq a => X86Rule a -> X86Rule a
pCheck r = if null rs
      then r
      else orderedNonTerminal rs'
   where
      ps = getPredicates r

      rs' = rs ++ [(CBool True, r)]
      rs = concat [ rexp
                  , csdp
                  , csdp2
                  , csdp3
                  , modesp
                  , encsp
                  ]

      makeP True  rule = [rule]
      makeP False _    = []

      -- we have to be careful not to add predicates that were not already
      -- present in the rule, otherwise `getPredicates` will return our added
      -- predicates...
      rexp = makeP 
               (EncodingPred PLegacyEncoding `elem` ps
                && EncodingPred PRexEncoding `elem` ps
               )
               (And [ Not (Predicate (EncodingPred PLegacyEncoding))
                    , Predicate (EncodingPred PRexEncoding)
                    ]
               , Fail "REX prefix not allowed without legacy encoding"
               )

      csdp = makeP
               ( ContextPred (Mode (LongMode Long64bitMode)) `elem` ps
                 && ContextPred CS_D `elem` ps
               )
               (And [ pMode (LongMode Long64bitMode)
                    , Predicate (ContextPred CS_D)
                    ]
               , Fail "CS.D and long 64-bit mode are mutually exclusive"
               )

      csdp2 = makeP
               ( ContextPred (Mode (LegacyMode RealMode)) `elem` ps
                 && ContextPred CS_D `elem` ps
               )
               (And [ pMode (LegacyMode RealMode)
                    , Predicate (ContextPred CS_D)
                    ]
               , Fail "CS.D makes no sense in real mode"
               )

      csdp3 = makeP
               ( ContextPred (Mode (LegacyMode Virtual8086Mode)) `elem` ps
                 && ContextPred CS_D `elem` ps
               )
               (And [ pMode (LegacyMode Virtual8086Mode)
                    , Predicate (ContextPred CS_D)
                    ]
               , Fail "CS.D makes no sense in virtual 8086 mode"
               )

      modes  = [ LongMode Long64bitMode
               , LongMode CompatibilityMode
               , LegacyMode RealMode
               , LegacyMode Virtual8086Mode
               , LegacyMode ProtectedMode
               ]
      modes' = filter (`elem` ps) (fmap (ContextPred . Mode) modes)

      modesp = makeP
                  (length modes' > 1)
                  (Not <| Xor <| fmap Predicate modes'
                  , Fail "Execution modes are mutually exclusive"
                  )


      encs =   [ PLegacyEncoding
               , PVexEncoding
               , PXopEncoding
               , PEvexEncoding
               , PMvexEncoding
               ]
      encs' = filter (`elem` ps) (fmap EncodingPred encs)

      encsp = makeP
                  (length encs' > 1)
                  ( Not <| Xor <| fmap Predicate encs'
                  , Fail "Encodings are mutually exclusive"
                  )



-- | Allow the use of legacy 8-bit AH,BH,CH,DH registers
pLegacy8bitRegs :: X86Constraint
pLegacy8bitRegs = And [ Predicate (EncodingPred PLegacyEncoding)
                      , Not (Predicate (EncodingPred PRexEncoding))
                      ]

-- | 64-bit long mode predicate
pMode64bit :: X86Constraint
pMode64bit = pModeEx (LongMode Long64bitMode)

-- | Exclusive mode predicate
pMode :: X86Mode -> X86Constraint
pMode = Predicate . ContextPred . Mode

-- | Mode predicate
pModeEx :: X86Mode -> X86Constraint
pModeEx m = And cs
   where
      -- we make all the modes mutually exclusive
      cs = fmap (\m' -> f m' (pMode  m'))
            [ LongMode Long64bitMode
            , LongMode CompatibilityMode
            , LegacyMode RealMode
            , LegacyMode Virtual8086Mode
            , LegacyMode ProtectedMode
            ]
      
      f m' | m == m'   = id
           | otherwise = Not
         

-- | CS.D flag
pCS_D :: X86Constraint
pCS_D = Predicate (ContextPred CS_D)

-- | Prefix predicate
pPrefix :: PrefixPred -> X86Constraint
pPrefix = Predicate . PrefixPred

-- | Overriden 64-bit operation size predicate
pOverriddenOperationSize64 :: OperandSize -> X86Constraint
pOverriddenOperationSize64 t = rOverriddenOperationSize64 `evalsTo` Terminal t

-- | Force 8-bit operand size
pForce8bit :: X86Constraint
pForce8bit = Predicate (InsnPred Force8bit)

-- | Overriden address size predicate
pOverriddenAddressSize :: AddressSize -> X86Constraint
pOverriddenAddressSize t = rOverriddenAddressSize `evalsTo` Terminal t

-----------------------------------------------------
-- Predicates
-----------------------------------------------------

-- | Default operation size (DOS)
rDefaultOperationSize :: X86Rule OperandSize
rDefaultOperationSize = NonTerminal
      [ (pModeEx (LegacyMode RealMode)           , Terminal OpSize16)
      , (pModeEx (LegacyMode Virtual8086Mode)    , Terminal OpSize16)
      , (pModeEx (LegacyMode ProtectedMode)      , s16o32)
      , (pModeEx (LongMode   CompatibilityMode)  , s16o32)
      , (pModeEx (LongMode   Long64bitMode)      , s32oFail)
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
      [ (pModeEx (LegacyMode RealMode)           , Terminal AddrSize16)
      , (pModeEx (LegacyMode Virtual8086Mode)    , Terminal AddrSize16)
      , (pModeEx (LegacyMode ProtectedMode)      , s16o32)
      , (pModeEx (LongMode   CompatibilityMode)  , s16o32)
      , (pModeEx (LongMode   Long64bitMode)      , s32oFail)
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
      p66 = [ (rDefaultOperationSize `evalsTo` Terminal OpSize16, Terminal OpSize32)
            , (rDefaultOperationSize `evalsTo` Terminal OpSize32, Terminal OpSize16)
            ]

-- | Overridden address size (OAS)
rOverriddenAddressSize :: X86Rule AddressSize
rOverriddenAddressSize = NonTerminal
      [ ((pPrefix Prefix67)      , orderedNonTerminal p67)
      , ((Not (pPrefix Prefix67)), rDefaultAddressSize)
      ]
   where
      p67 = [ (rDefaultAddressSize `evalsTo` Terminal AddrSize16, Terminal AddrSize32)
            , (rDefaultAddressSize `evalsTo` Terminal AddrSize32, Terminal AddrSize16)
            , (rDefaultAddressSize `evalsTo` Terminal AddrSize64, Terminal AddrSize32)
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
