
{-# LANGUAGE FlexibleContexts #-}

module Raven.Parser ( parse
                    , bool
                    , string
                    , number
                    , comment
                    , symbol
                    ) where

import Text.Parsec hiding (parse, string)
import qualified Text.Parsec (parse, string)
import Text.Parsec.String
import Data.Functor.Identity
import Data.List (foldl')
import Data.Char (digitToInt)
import Raven.Types



parse :: Stream s Identity t => Parsec s () a -> s -> Either ParseError a
parse parser stream = Text.Parsec.parse parser "" stream

bool :: Parser Expr
bool = trueExpr <|> falseExpr
  where trueExpr = (string' "true") >> pure (RBool True)
        falseExpr = (string' "false") >> pure (RBool False)

string' = Text.Parsec.string

string :: Parser Expr
string = RString <$> (char '\"' *> stringCharacters <* char '\"')
  where stringCharacters = many $ noneOf "\"" 

comment :: Parser Expr
comment = RComment <$> (string' ";;" >> (many $ noneOf "\n"))

symbol :: Parser Expr
symbol = RSymbol <$> symbolParser
  where symbolParser = do
          firstChar <- letter
          restChars <- many symbolChars
          return $ firstChar : restChars
        symbolChars = alphaNum <|> oneOf "+-.*/<=>!?$%_&^,~"

decimalInteger :: Parser Expr
decimalInteger = RNumber . Integral <$> do
  integerChars <- many1 digit
  return $ read integerChars

hexInteger :: Parser Expr
hexInteger = RNumber . Integral <$> do
  hexPrefix <- string' "0x"
  hexChars <- many1 hexDigit
  return $ read $ hexPrefix ++ hexChars

binaryInteger :: Parser Expr
binaryInteger = RNumber . Integral <$> do
  binPrefix <- string' "0b"
  binChars <- many1 $ oneOf "01"
  return $ bin2dec binChars

-- TODO avoid try...
integer :: Parser Expr
integer = try hexInteger
       <|> try binaryInteger
       <|> decimalInteger

-- TODO double, rational
-- TODO simplifiy number parsers if possible using parsec helpers
  
-- TODO how to handle int/float?
number :: Parser Expr
number = integer


-- Helper functions

bin2dec :: String -> Int
bin2dec = foldl' (\acc x -> acc * 2 + digitToInt x) 0
