-------------------------------------------------------------------
-- |
-- Module      : Common.Parser
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

module Common.Parser (parseModule) where

import Text.Parsec
import Text.Parsec.String(Parser)
import Text.Parsec.Language
import qualified Text.Parsec.Token as P
import Control.Applicative((<$>),(<*),(*>),(<*>))

import Common.Sig
---import Data.Functor.Identity(Identity)
import Control.Monad.Identity(Identity)
import Data.List

defs :: ParsecT String u Identity Char -> LanguageDef u
defs start = javaStyle { 
  P.reservedOpNames = ["=","|",":"],
  P.reservedNames = ["module","abstract","syntax","imports","implemented by"],
  P.identStart = start
}

lexer, ulexer :: P.TokenParser a
lexer  = P.makeTokenParser $ defs letter
ulexer = P.makeTokenParser $ defs upper

white    :: Parser ()
parens   :: Parser a -> Parser a
angles   :: Parser a -> Parser a
ident    :: Parser String
uident   :: Parser String
res      :: String -> Parser ()
resOp    :: String -> Parser ()
comma    :: Parser String

white    = P.whiteSpace lexer
parens   = P.parens     lexer
angles   = P.angles     lexer
ident    = P.identifier lexer
uident   = P.identifier ulexer
res      = P.reserved   lexer
resOp    = P.reservedOp lexer
comma    = P.comma      lexer

-- sorts defined by the user start with a capital letter
lhsSortidP :: Parser SortId
lhsSortidP = makeSortId <$> uident 
             <?> "sort name"

-- ... but not necessarily the imported ones
rhsSortidP :: Parser SortId
rhsSortidP = makeSortId <$> ident 
             <?> "sort name"

-- The code of this function is convoluted because
-- it reconstructs the string it parses instead of building
-- a datastructure. They may be a better way to do it with parsec.
classidP :: Parser ClassId
classidP = uncurry makeClassId <$> classidP' <?> "class name"
  where classidP' = do 
          qname  <- intercalate "." <$> ident `sepBy1` resOp "." 
          params <- option "" paramsP
          return (qname,params)
        paramsP  = do 
          ps <- angles (classidP' `sepBy` comma)
          return $ "<" ++ intercalate "," (map glue ps) ++ ">"
          where glue = uncurry (++)

fieldidP :: Parser FieldId
fieldidP = makeFieldId <$> ident
           <?> "field name"

ctoridP :: Parser CtorId
ctoridP = makeCtorId <$> ident
          <?> "constructor name"

sigP :: Parser Module
sigP = Module <$> (res "module" *> ident)
              <*> option [] (res "imports" *> rhsSortidP `sepBy` spaces)
              <*  res "abstract" 
              <*  res "syntax"
              <*> sortP `sepBy` spaces
              <*  eof
              <?> "module definition"

sortP ::  Parser SortDef
sortP = do n  <- lhsSortidP
           cn <- option Nothing $ Just <$> (res "implemented by" *> classidP)
           resOp "=" 
           c <- ctorP `sepBy` resOp "|"
           return $ SortDef n cn c
           <?> "sort definition"

ctorP :: Parser Ctor
ctorP = try variadicP <|> simpleP 
        <?> "constructor declaration"

variadicP :: Parser Ctor
variadicP = Variadic <$> ctoridP <*> parens (rhsSortidP <* resOp "*")
            <?> "variadic constructor"

simpleP :: Parser Ctor
simpleP = Simple <$> ctoridP <*> parens (fieldP `sepBy` comma)
          <?> "non-variadic constructor"

fieldP :: Parser (FieldId, SortId)
fieldP = do x <- fieldidP
            resOp ":"
            ty <- rhsSortidP
            return (x,ty)
         <?> "field declaration"

run :: Parser a -> String -> Either ParseError a
run p = parse (white *> p) ""

-- | Parses a gom module.
parseModule :: String -> Either ParseError Module
parseModule = run sigP
