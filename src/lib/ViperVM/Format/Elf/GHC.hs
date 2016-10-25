-- | GHC specific ELF additions

module ViperVM.Format.Elf.GHC
   ( decodeZString
   )
where

import Data.Char

-- Since Haskell allows many symbols in constructor and variable names that C
-- compilers or assembly might not allow (e.g. :, %, #) these have to be
-- encoded. The encoding is called z-encoding
--
-- * https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/SymbolNames
-- * compiler/utils/Encoding.hs in GHC sources
--
-- Tuples
-- ------
--       Decoded     Encoded  Comment
--       ()          Z0T      Unit / 0-tuple
--                            There is no Z1T
--       (,)         Z2T      2-tuple
--       (,,)        Z3T      3-tuple
--       ...                  And so on
--
-- Unboxed Tuples
-- --------------
--       Decoded  Encoded  Comment
--                         There is no Z0H
--       (# #)    Z1H      unboxed 1-tuple (note the space)
--       (#,#)    Z2H      unboxed 2-tuple
--       (#,,#)   Z3H      unboxed 3-tuple
--       ...               And so on
--
-- Alphanumeric Characters
-- -----------------------
--
--       Decoded        Encoded        Comment
--       a-y, A-Y, 0-9  a-y, A-Y, 0-9  
--       z, Z           zz, ZZ         'Z' and 'z' must be escaped
--
-- Constructor Characters
-- ----------------------
--
--       Decoded  Encoded  Comment
--       (        ZL       Left
--       )        ZR       Right
--       [        ZM       'M' before 'N' in []
--       ]        ZN    
--       :        ZC       Colon
--
-- Variable Characters
-- -------------------
--
--       Decoded  Encoded  Mnemonic
--       &        za       Ampersand
--       |        zb       Bar
--       ^        zc       Caret
--       $        zd       Dollar
--       =        ze       Equals
--       >        zg       Greater than
--       #        zh       Hash
--       .        zi       The dot of the 'i'
--       <        zl       Less than
--       -        zm       Minus
--       !        zn       Not
--       +        zp       Plus
--       '        zq       Quote
--       \        zr       Reverse slash
--       /        zs       Slash
--       *        zt       Times sign
--       _        zu       Underscore
--       %        zv       
--
-- Other
-- -----
--
-- Any other character is encoded as a 'z' followed by its hex code
-- (lower case, variable length) followed by 'U'. If the hex code
-- starts with 'a', 'b, 'c', 'd', 'e' or 'f', then an extra '0' is
-- placed before the hex code to avoid conflicts with the other
-- escape characters. 


decodeZString :: String -> Maybe String
decodeZString [] = Just []
decodeZString ('Z' : d : rest)
  | isDigit d = decode_tuple   d rest
  | otherwise = (:) <$> decode_upper d <*> decodeZString rest
decodeZString ('z' : d : rest)
  | isDigit d = decode_num_esc d rest
  | otherwise = (:) <$> decode_lower d <*> decodeZString rest
decodeZString (c   : rest) = (c :) <$> decodeZString rest

decode_upper :: Char -> Maybe Char
decode_upper 'L' = Just '('
decode_upper 'R' = Just ')'
decode_upper 'M' = Just '['
decode_upper 'N' = Just ']'
decode_upper 'C' = Just ':'
decode_upper 'Z' = Just 'Z'
decode_upper _   = Nothing

decode_lower :: Char -> Maybe Char
decode_lower 'z' = Just 'z'
decode_lower 'a' = Just '&'
decode_lower 'b' = Just '|'
decode_lower 'c' = Just '^'
decode_lower 'd' = Just '$'
decode_lower 'e' = Just '='
decode_lower 'g' = Just '>'
decode_lower 'h' = Just '#'
decode_lower 'i' = Just '.'
decode_lower 'l' = Just '<'
decode_lower 'm' = Just '-'
decode_lower 'n' = Just '!'
decode_lower 'p' = Just '+'
decode_lower 'q' = Just '\''
decode_lower 'r' = Just '\\'
decode_lower 's' = Just '/'
decode_lower 't' = Just '*'
decode_lower 'u' = Just '_'
decode_lower 'v' = Just '%'
decode_lower _   = Nothing

-- Characters not having a specific code are coded as z224U (in hex)
decode_num_esc :: Char -> String -> Maybe String
decode_num_esc d = go (digitToInt d)
  where
    go n (c : rest)
      | isHexDigit c    = go (16*n + digitToInt c) rest
    go n ('U' : rest)   = (chr n :) <$> decodeZString rest
    go n other          = Nothing

decode_tuple :: Char -> String -> Maybe String
decode_tuple d = go (digitToInt d)
   where
    -- NB. recurse back to decodeZString after decoding the tuple, because
    -- the tuple might be embedded in a longer name.
    go n (c : rest)
      | isDigit c    = go (10*n + digitToInt c) rest
    go 0 ('T':rest)  = do
      r <- decodeZString rest
      return ("()" ++ r)
    go n ('T':rest)  = do
      r <- decodeZString rest
      return ('(' : replicate (n-1) ',' ++ ")" ++ r)
    go 1 ('H':rest)  = do
      r <- decodeZString rest
      return ("(# #)" ++ r)
    go n ('H':rest)  = do
      r <- decodeZString rest
      return ('(' : '#' : replicate (n-1) ',' ++ "#)" ++ r)
    go n other       = Nothing

