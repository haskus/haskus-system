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
--  * https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/SymbolNames
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


decodeZString :: String -> String
decodeZString [] = []
decodeZString ('Z' : d : rest)
  | isDigit d = decode_tuple   d rest
  | otherwise = decode_upper   d : decodeZString rest
decodeZString ('z' : d : rest)
  | isDigit d = decode_num_esc d rest
  | otherwise = decode_lower   d : decodeZString rest
decodeZString (c   : rest) = c : decodeZString rest

decode_upper :: Char -> Char
decode_upper 'L' = '('
decode_upper 'R' = ')'
decode_upper 'M' = '['
decode_upper 'N' = ']'
decode_upper 'C' = ':'
decode_upper 'Z' = 'Z'
decode_upper ch  = {-pprTrace "decode_upper" (char ch)-} ch

decode_lower :: Char -> Char
decode_lower 'z' = 'z'
decode_lower 'a' = '&'
decode_lower 'b' = '|'
decode_lower 'c' = '^'
decode_lower 'd' = '$'
decode_lower 'e' = '='
decode_lower 'g' = '>'
decode_lower 'h' = '#'
decode_lower 'i' = '.'
decode_lower 'l' = '<'
decode_lower 'm' = '-'
decode_lower 'n' = '!'
decode_lower 'p' = '+'
decode_lower 'q' = '\''
decode_lower 'r' = '\\'
decode_lower 's' = '/'
decode_lower 't' = '*'
decode_lower 'u' = '_'
decode_lower 'v' = '%'
decode_lower ch  = ch

-- Characters not having a specific code are coded as z224U (in hex)
decode_num_esc :: Char -> String -> String
decode_num_esc d = go (digitToInt d)
  where
    go n (c : rest)
      | isHexDigit c    = go (16*n + digitToInt c) rest
    go n ('U' : rest)   = chr n : decodeZString rest
    go n other          = error ("decode_num_esc: " ++ show n ++  ' ':other)

decode_tuple :: Char -> String -> String
decode_tuple d = go (digitToInt d)
   where
    -- NB. recurse back to decodeZString after decoding the tuple, because
    -- the tuple might be embedded in a longer name.
    go n (c : rest)
      | isDigit c    = go (10*n + digitToInt c) rest
    go 0 ('T':rest)  = "()" ++ decodeZString rest
    go n ('T':rest)  = '(' : replicate (n-1) ',' ++ ")" ++ decodeZString rest
    go 1 ('H':rest)  = "(# #)" ++ decodeZString rest
    go n ('H':rest)  = '(' : '#' : replicate (n-1) ',' ++ "#)" ++ decodeZString rest
    go n other       = error ("decode_tuple: " ++ show n ++ ' ':other)

