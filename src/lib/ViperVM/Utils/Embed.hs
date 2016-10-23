-- | Embed data into the executable binary
module ViperVM.Utils.Embed
   ( embedBytes
   , module Data.FileEmbed
   )
where

import Language.Haskell.TH
import Data.FileEmbed

import ViperVM.Format.Binary.Word

-- | Embed bytes in a C array, return an Addr#
embedBytes :: [Word8] -> Q Exp
embedBytes bs = return $ LitE (StringPrimL bs)
