{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | Register
module Haskus.Arch.Common.Register
   ( Reg (..)
   , TypedReg (..)
   -- * Register family
   , RegFam (..)
   , Qualifier (..)
   , regMatchFamily
   , regFixupFamily
   , regFixupFamilyMaybe
   , matchQualifier
   , fixupQualifier
   , fixupQualifierMaybe
   , regFamFromReg
   )
where

-- | Register
data Reg banks = Reg
   { registerBank   :: banks                -- ^ Register bank
   , registerId     :: {-# UNPACK #-} !Word -- ^ Register ID
   , registerSize   :: {-# UNPACK #-} !Word -- ^ Register size in bits
   , registerOffset :: {-# UNPACK #-} !Word -- ^ Register offset in bits (alias register)
   }
   deriving (Show,Eq,Ord)


-- | Typed register
data TypedReg typ bank = TypedReg
   { typedReg     :: Reg bank   -- ^ Register
   , typedRegType :: typ        -- ^ Register type
   }
   deriving (Show,Eq,Ord)

--------------------------
-- Register family
-- ~~~~~~~~~~~~~~~
--
-- A register family is a set of registers that have some things in common.
-- We encode them similarly to usual Register except that their properties
-- support qualifiers (OneOf,NoneOf...).
--
-- We also have a `Guard p a` qualifier which uses `p` as a guard for `a`:
-- during matching, a function to evalute `p` must be provided. It is used for
-- instance for x86 register families which depend on the operand size: we guard
-- each register variant (AX,EAX,RAX) with a predicate `OperandSizeEq n` (with
-- n=16,32,64 respectively).
--


-- | Qualifiers
data Qualifier p a
   = Set a
   | NoneOf [a]
   | OneOf  [a]
   | Any
   | Or [Qualifier p a]
   | Guard p (Qualifier p a)
   deriving (Show,Eq)

-- | Register family
data RegFam pred banks = RegFam
   { regFamBank   :: Qualifier pred banks -- ^ Register bank
   , regFamId     :: Qualifier pred Word  -- ^ Register ID
   , regFamSize   :: Qualifier pred Word  -- ^ Register size in bits
   , regFamOffset :: Qualifier pred Word  -- ^ Register offset in bits
   }
   deriving (Show,Eq)

-- | Test if a register match a family
regMatchFamily :: (Eq b) => (p -> Bool) -> RegFam p b -> Reg b -> Bool
regMatchFamily predSolver RegFam{..} Reg{..} =
      matchQualifier predSolver registerBank regFamBank
      && matchQualifier predSolver registerId regFamId
      && matchQualifier predSolver registerSize regFamSize
      && matchQualifier predSolver registerOffset regFamOffset

-- | Fixup a family (all fields must reduce to Set qualifier)
regFixupFamily :: (Eq b) => (p -> Bool) -> RegFam p b -> Reg b
regFixupFamily predSolver fam =
   case regFixupFamilyMaybe predSolver fam of
      Nothing -> error "Cannot fixup family"
      Just c  -> c

-- | Try to fixup a family (all fields must reduce to Set qualifier)
regFixupFamilyMaybe :: (Eq b) => (p -> Bool) -> RegFam p b -> Maybe (Reg b)
regFixupFamilyMaybe predSolver RegFam{..} =
   Reg <$> fixupQualifierMaybe predSolver regFamBank
       <*> fixupQualifierMaybe predSolver regFamId
       <*> fixupQualifierMaybe predSolver regFamSize
       <*> fixupQualifierMaybe predSolver regFamOffset

-- | Match a qualifier
matchQualifier :: Eq a => (p -> Bool) -> a -> Qualifier p a -> Bool
matchQualifier predSolver = test
   where
      test y (Set x)     = x == y
      test y (NoneOf xs) = y `notElem` xs
      test y (OneOf xs)  = y `elem` xs
      test _ Any         = True
      test y (Or xs)     = any (test y) xs
      test y (Guard p x) = predSolver p && test y x

-- | Reduce a qualifier to a Set if possible and retrieve the value
fixupQualifier :: Eq a => (p -> Bool) -> Qualifier p a -> a
fixupQualifier fp q = case fixupQualifierMaybe fp q of
      Nothing -> error "Can't fix up qualifier"
      Just x  -> x

-- | Reduce a qualifier to a Set if possible and retrieve the value
fixupQualifierMaybe :: Eq a => (p -> Bool) -> Qualifier p a -> Maybe a
fixupQualifierMaybe fp = \case
   Set x      -> Just x
   NoneOf _   -> Nothing
   OneOf  [x] -> Just x
   OneOf  _   -> Nothing
   Any        -> Nothing
   Or     []  -> Nothing
   Or  (x:xs) -> case fixupQualifierMaybe fp x of
                     Nothing -> fixupQualifierMaybe fp (Or xs)
                     c       -> c
   Guard p x -> if fp p then fixupQualifierMaybe fp x else Nothing

-- | Make a family matching a single register
regFamFromReg :: Reg b -> RegFam p b
regFamFromReg Reg{..} = RegFam
   { regFamBank   = Set registerBank
   , regFamId     = Set registerId
   , regFamSize   = Set registerSize
   , regFamOffset = Set registerOffset
   }
