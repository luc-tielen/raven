
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
import Text.Megaparsec.Lexer as L hiding (number, symbol)
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
integral :: Parser Expr
integral = try hexInteger
        <|> try binaryInteger
        <|> decimalInteger

rational :: Parser Expr
rational = RNumber <$> do
  nominator <- some digitChar
  char '/'
  denominator <- some digitChar
  return $ Rational (read nominator) (read denominator)

real :: Parser Expr
real = RNumber <$> do
  value <- float
  return $ Real value

complex :: Parser Expr
complex = RNumber <$> do
  firstPart <- try signedFloatNoScientific <|> signedInteger
  maybeSecondPart <- optional (try signedFloatNoScientific <|> signedInteger)
  char 'i'
  case maybeSecondPart of
    Just secondPart -> return $ Complex firstPart secondPart
    Nothing -> return $ Complex 0 firstPart
  where signedFloatNoScientific = signed (return ()) floatNoScientific
        floatNoScientific = do
          val1 <- some digitChar
          char '.'
          val2 <- some digitChar
          return $ ((read val1) :: Double) + ((read ("0." ++ val2)) :: Double)
        signedInteger = fromIntegral <$> (signed (return ()) integer)


-- TODO simplifiy number parsers if possible using parsec helpers
-- TODO avoid try
number :: Parser Expr
number = try complex
      <|> try real
      <|> try rational
      <|> integral


-- Helper functions

bin2dec :: String -> Int
bin2dec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

stringI :: (Token s ~ Char, Text.Megaparsec.Prim.MonadParsec e s m) => String -> m String
stringI = Text.Megaparsec.string'  -- case insensitive string helper

stringS :: (Token s ~ Char, Text.Megaparsec.Prim.MonadParsec e s m) => String -> m String
stringS = Text.Megaparsec.string   -- case sensitive string helper
