module Haskus.System.PCI.MakeTable
   ( pcis
   , fromList
   )
where

import Haskus.Utils.Flow
import Haskus.Binary.Bits

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import Data.IntMap.Strict
import Data.Void

type Parser = Parsec Void String

pcis :: QuasiQuoter
pcis = quoteFile $ QuasiQuoter
   { quoteDec  = makeTable
   , quoteExp  = undefined
   , quotePat  = undefined
   , quoteType = undefined
   }

makeTable :: String -> Q [Dec]
makeTable str =
   case runParser parseLines "PCI vendor/device identifier table" str of
      Right e   -> return e
      Left err  -> fail (show err)

-- | Parse PCI ids
parseLines :: Parser [Dec]
parseLines = do
      vs <- vendors
      cs <- classes
      return $
         [ SigD (mkName "pciDevices") (ConT (mkName "IntMap")
                                        `AppT` ConT (mkName "Vendor"))
         , ValD (VarP (mkName "pciDevices")) (NormalB vs) []
         , SigD (mkName "pciClasses") (ConT (mkName "IntMap")
                                        `AppT` ConT (mkName "Class"))
         , ValD (VarP (mkName "pciClasses")) (NormalB cs) []
         ]
   where

      end        = void eol <|> eof

      vendors = do
         vs <- many vendor
         return $ VarE (mkName "fromList")
                     `AppE` ListE vs
         
      vendor = try $ do
         skipUseless
         notFollowedBy (char 'C')
         vid <- hexadecimal
         someSpace
         vname <- anySingle `manyTill` end
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
         dname <- anySingle `manyTill` end
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
         dname <- anySingle `manyTill` end
         return $ TupE
            [ LitE $ IntegerL $ (vid `shiftL` 16) .|. did
            , LitE $ StringL $ dname
            ]

      classes = do
         vs <- many cls
         return $ VarE (mkName "fromList")
                     `AppE` ListE vs
      cls = do
         skipUseless
         void (char 'C')
         someSpace
         cid <- hexadecimal
         someSpace
         cname <- anySingle `manyTill` end
         devs  <- many subclass
         skipUseless
         return $ TupE
            [ LitE $ IntegerL cid
            , ConE (mkName "Class")
               `AppE` (LitE $ StringL $ cname)
               `AppE` (VarE (mkName "fromList")
                  `AppE` ListE devs)
            ]
      subclass = try $ do
         skipUseless
         void (char '\t')
         cid <- hexadecimal
         someSpace
         cname <- anySingle `manyTill` end
         subs  <- many progInterface
         return $ TupE
            [ LitE $ IntegerL cid
            , ConE (mkName "SubClass")
               `AppE` (LitE $ StringL $ cname)
               `AppE` (VarE (mkName "fromList")
                  `AppE` ListE subs)
            ]

      progInterface = try $ do
         skipUseless
         void (char '\t')
         void (char '\t')
         cid <- hexadecimal
         someSpace
         cname <- anySingle `manyTill` end
         return $ TupE
            [ LitE $ IntegerL $ cid
            , LitE $ StringL $ cname
            ]

      -- 'space' from MegaParsec also consider line-breaks as spaces...
      someSpace  = skipSome (char ' ')

      skipUseless = skipMany (comment <|> eol)

      comment = do
         void (string "#")
         anySingle `manyTill` end

