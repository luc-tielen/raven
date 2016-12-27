
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Raven.Parser ( parse
                    , bool
                    , string
                    , number
                    , comment
                    , symbol
                    ) where

import Text.Megaparsec hiding (parse, string, string')
import qualified Text.Megaparsec (parse, string, string')
import Text.Megaparsec.Prim hiding (parse)
import Text.Megaparsec.String
import Data.Functor.Identity
import Data.List (foldl')
import Data.Char (digitToInt)
import Raven.Types


parse :: Parsec e s a -> s -> Either (ParseError (Token s) e) a
parse parser stream = Text.Megaparsec.parse parser "" stream

bool :: Parser Expr
bool = trueExpr <|> falseExpr
  where trueExpr = (stringS "true") >> return (RBool True)
        falseExpr = (stringS "false") >> return (RBool False)

stringI :: (Token s ~ Char, Text.Megaparsec.Prim.MonadParsec e s m) => String -> m String
stringI = Text.Megaparsec.string'  -- case insensitive
stringS :: (Token s ~ Char, Text.Megaparsec.Prim.MonadParsec e s m) => String -> m String
stringS = Text.Megaparsec.string   -- case sensitive

string :: Parser Expr
string = RString <$> (char '\"' *> stringCharacters <* char '\"')
  where stringCharacters = many $ noneOf "\"" 

comment :: Parser Expr
comment = RComment <$> (stringS ";;" >> (many $ noneOf "\n"))

symbol :: Parser Expr
symbol = RSymbol <$> symbolParser
  where symbolParser = do
          firstChar <- letterChar
          restChars <- many symbolChars
          return $ firstChar : restChars
        symbolChars = alphaNumChar <|> oneOf "+-.*/<=>!?$%_&^,~"

decimalInteger :: Parser Expr
decimalInteger = RNumber . Integral <$> do
  integerChars <- some digitChar
  return $ read integerChars

hexInteger :: Parser Expr
hexInteger = RNumber . Integral <$> do
  hexPrefix <- stringS "0x"
  hexChars <- some hexDigitChar
  return $ read $ hexPrefix ++ hexChars

binaryInteger :: Parser Expr
binaryInteger = RNumber . Integral <$> do
  binPrefix <- stringS "0b"
  binChars <- some $ oneOf "01"
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
