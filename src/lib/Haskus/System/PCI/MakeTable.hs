module Haskus.System.PCI.MakeTable
   ( pcis
   , Vendor (..)
   , Device (..)
   , fromList
   )
where


import Haskus.Utils.Flow
import Haskus.Format.Binary.Bits

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Text.Megaparsec hiding (Dec)
import Text.Megaparsec.String
import Text.Megaparsec.Lexer hiding (space)
import Data.IntMap.Strict

pcis :: QuasiQuoter
pcis = quoteFile $ QuasiQuoter
   { quoteDec  = undefined
   , quoteExp  = makeTable
   , quotePat  = undefined
   , quoteType = undefined
   }

makeTable :: String -> Q Exp
makeTable str =
   case runParser parseLines "PCI vendor/device identifier table" str of
      Right e   -> return e
      Left err  -> fail (show err)

data Vendor = Vendor
   { vendorName    :: String
   , vendorDevices :: IntMap Device
   }

data Device = Device
   { deviceName       :: String
   , deviceSubDevices :: IntMap String
   }


-- | Parse PCI ids
parseLines :: Parser Exp
parseLines = vendors
   where

      end        = void eol <|> eof

      vendors = do
         vs <- many vendor
         return $ VarE (mkName "fromList")
                     `AppE` ListE vs
         
      vendor = do
         skipUseless
         vid <- hexadecimal
         someSpace
         vname <- anyChar `manyTill` end
         devs  <- many device
         skipUseless
         return $ TupE
            [ LitE $ IntegerL vid
            , ConE (mkName "Vendor")
               `AppE` (LitE $ StringL $ vname)
               `AppE` (VarE (mkName "fromList")
                  `AppE` ListE devs)
            ]

      device = try $ do
         skipUseless
         void (char '\t')
         did <- hexadecimal
         someSpace
         dname <- anyChar `manyTill` end
         subs  <- many subdevice
         return $ TupE
            [ LitE $ IntegerL did
            , ConE (mkName "Device")
               `AppE` (LitE $ StringL $ dname)
               `AppE` (VarE (mkName "fromList")
                  `AppE` ListE subs)
            ]

      subdevice = try $ do
         skipUseless
         void (char '\t')
         void (char '\t')
         vid <- hexadecimal
         someSpace
         did <- hexadecimal
         someSpace
         dname <- anyChar `manyTill` end
         return $ TupE
            [ LitE $ IntegerL $ (vid `shiftL` 16) .|. did
            , LitE $ StringL $ dname
            ]

      -- 'space' from MegaParsec also consider line-breaks as spaces...
      someSpace  = skipSome (char ' ')

      skipUseless = skipMany (comment <|> eol)

      comment = do
         void (string "#")
         anyChar `manyTill` end

