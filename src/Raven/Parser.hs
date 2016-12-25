
{-# LANGUAGE FlexibleContexts #-}

module Raven.Parser ( parse
                    , bool
                    , string
                    , comment
                    ) where

import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, newline)
import Text.Parsec (Parsec, ParseError, Stream, (<|>), many, noneOf)
import qualified Text.Parsec (parse, string)
import Data.Functor.Identity
import Raven.Types



parse :: Stream s Identity t => Parsec s () a -> s -> Either ParseError a
parse parser stream = Text.Parsec.parse parser "" stream

bool :: Parser Expr
bool = trueExpr <|> falseExpr
  where trueExpr = (string' "true") *> pure (RBool True)
        falseExpr = (string' "false") *> pure (RBool False)

string' = Text.Parsec.string

string :: Parser Expr
string = RString <$> (char '\"' *> stringCharacters <* char '\"')
  where stringCharacters = many $ noneOf "\"" 

comment :: Parser Expr
comment = RComment <$> (string' ";;" *> (many $ noneOf "\n"))
