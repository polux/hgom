-------------------------------------------------------------------
-- |
-- Module      : Gom.Parser
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--
-- Parser for gom modules.
--------------------------------------------------------------------

module Gom.Parser (parseModule) where

import Text.Parsec
import Text.Parsec.String(Parser)
import Text.Parsec.Language
import qualified Text.Parsec.Token as P
import Control.Applicative((<$>),(<*),(*>),(<*>))

import Gom.Sig

defs :: P.LanguageDef a
defs = javaStyle { 
  P.reservedOpNames = ["=","|",":"],
  P.reservedNames = ["module","abstract","syntax","imports"]
}

lexer :: P.TokenParser a
lexer = P.makeTokenParser defs 

white    :: Parser ()
parens   :: Parser a -> Parser a
ident    :: Parser String
res      :: String -> Parser ()
resOp    :: String -> Parser ()
comma    :: Parser String

white    = P.whiteSpace lexer
parens   = P.parens     lexer
ident    = P.identifier lexer
res      = P.reserved   lexer
resOp    = P.reservedOp lexer
comma    = P.comma      lexer

sortidP :: Parser SortId
sortidP = makeSortId <$> ident 
          <?> "sort name"

fieldidP :: Parser FieldId
fieldidP = makeFieldId <$> ident
           <?> "field name"

ctoridP :: Parser CtorId
ctoridP = makeCtorId <$> ident
          <?> "constructor name"

sigP :: Parser Module
sigP = Module <$> (res "module" *> ident)
              <*> option [] (res "imports" *> sortidP `sepBy` spaces)
              <*  res "abstract" 
              <*  res "syntax"
              <*> sortP `sepBy` spaces
              <*  eof
              <?> "module definition"

sortP ::  Parser SortDef
sortP = do n <- sortidP
           resOp "=" 
           c <- ctorP `sepBy` resOp "|"
           return $ SortDef n c
           <?> "sort definition"

ctorP :: Parser Ctor
ctorP = try variadicP <|> simpleP 
        <?> "constructor declaration"

variadicP :: Parser Ctor
variadicP = Variadic <$> ctoridP <*> parens (sortidP <* resOp "*")
            <?> "variadic constructor"

simpleP :: Parser Ctor
simpleP = Simple <$> ctoridP <*> parens (fieldP `sepBy` comma)
          <?> "non-variadic constructor"

fieldP :: Parser (FieldId, SortId)
fieldP = do x <- fieldidP
            resOp ":"
            ty <- sortidP
            return (x,ty)
         <?> "field declaration"

run :: Parser a -> String -> Either ParseError a
run p input = parse (white *> p) "" input

-- | Parses a gom module.
parseModule :: String -> Either ParseError Module
parseModule = run sigP
