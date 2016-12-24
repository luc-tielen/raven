
{-# LANGUAGE FlexibleContexts #-}

module Raven.Parser ( bool
                    , parse
                    ) where

import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, string, noneOf)
import Text.Parsec (Parsec, ParseError, Stream, (<|>))
import qualified Text.Parsec (parse)
import Data.Functor.Identity
import Raven.Types


parse :: Stream s Identity t => Parsec s () a -> s -> Either ParseError a
parse parser stream = Text.Parsec.parse parser "" stream

bool :: Parser Expr
bool = true' <|> false'
  where true' = (string "true") *> pure (RBool True)
        false' = (string "false") *> pure (RBool False)
        
