module Haskus.Arch.X86_64.Linux.SyscallTable
   ( syscalls
   )
where

import Haskus.Utils.Flow
import Haskus.Utils.Maybe

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer hiding (space)
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

syscalls :: QuasiQuoter
syscalls = QuasiQuoter
   { quoteDec  = makeSyscalls
   , quoteExp  = undefined
   , quotePat  = undefined
   , quoteType = undefined
   }

makeSyscalls :: String -> Q [Dec]
makeSyscalls str =
   case runParser parseLines "syscalls table" str of
      Right entries -> return (concatMap makeSyscall entries)
      Left err      -> fail (show err)

type Entry = (Integer,String,String,[[String]])

makeSyscall :: Entry -> [Dec]
makeSyscall (num,mode,name,typ) = [sysSig,sysFun]
   where
      arity    = length typ - 1
      syscallN = mkName <| mconcat
        [ "syscall"
        , show arity
        , case mode of
              "PrimOp" -> "primop"
              "Safe"   -> "safe"
              r        -> fail ("Invalid syscall mode: " ++ r)
        ]

      makeType :: [[String]] -> Type
      makeType xs = 
         xs ||> fmap (ConT . mkName)
            ||> foldl1 AppT
            |> foldr1 (\x y -> AppT (AppT ArrowT x) y)

      sysName = mkName ("syscall_"++name)
                 
      sysFun  = FunD sysName
                 [ Clause []
                    (NormalB (AppE (VarE syscallN) (LitE (IntegerL num))))
                    []
                 ]
      sysSig  = SigD sysName (makeType typ)

-- | Parse a line with the form:
--    num mode name :: type
-- e.g.
--    4 PrimOp stat :: CString -> Ptr () -> IO Int64
parseLines :: Parser [(Integer,String,String,[[String]])]
parseLines = catMaybes <$> lines'
   where
      lines' = (line `sepEndBy` eol) <* eof

      line = manySpace *>
         (  (Just <$> try entryLine)
         <|> (try comment >> return Nothing)
         <|> (lookAhead end >> return Nothing)
         )

      entryLine = do
         num <- decimal
         someSpace
         mode <- some alphaNumChar
         someSpace
         name <- identifier
         manySpace
         void (string "::")
         manySpace
         typ <- (typElem `sepEndBy` manySpace) `sepBy` arrow
         manySpace
         lookAhead end
         return (num,mode,name,typ)
   
      end        = void eol <|> eof
      arrow      = void (string "->") >> manySpace

      identifier = some (alphaNumChar <|> char '_')
      typElem    = identifier <|> string "()"
         

      -- 'space' from MegaParsec also considers line-breaks as spaces...
      manySpace  = skipMany (char ' ')
      someSpace  = skipSome (char ' ')

      comment = do
         void (string "--")
         anyChar `manyTill` lookAhead end
