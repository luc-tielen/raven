
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

bool :: Parser Expression
bool = trueExpression <|> falseExpression
  where trueExpression = (stringS "true") >> return (RavenBool True)
        falseExpression = (stringS "false") >> return (RavenBool False)

string :: Parser Expression
string = RavenString <$> between doubleQuote doubleQuote stringCharacters
  where doubleQuote = char '\"'
        stringCharacters = many $ noneOf "\"" 

comment :: Parser Expression
comment = RavenComment <$> (stringS ";;" >> (many $ noneOf "\n"))

symbol :: Parser Expression
symbol = RavenSymbol <$> symbolParser
  where symbolParser = do
          firstChar <- letterChar
          restChars <- many symbolChars
          return $ firstChar : restChars
        symbolChars = alphaNumChar <|> oneOf "+-.*/<=>!?$%_&^,~"

integral :: Parser Expression
integral = RavenNumber . RavenIntegral <$> do
  firstPart <- stringS "0x" <|> stringS "0b" <|> some digitChar
  case firstPart of
    "0x" -> do
      hexDigits <- some hexDigitChar
      return $ read $ "0x" ++ hexDigits
    "0b" -> do
      binDigits <- some $ oneOf "01"
      return $ bin2dec binDigits
    digits -> return $ read digits

rational :: Parser Expression
rational = RavenNumber <$> do
  nominator <- some digitChar
  char '/'
  denominator <- some digitChar
  return $ RavenRational (read nominator) (read denominator)

real :: Parser Expression
real = RavenNumber . RavenReal <$> float

complex :: Parser Expression
complex = RavenNumber <$> do
  firstPart <- signedFloatNoScientific <||> signedInteger
  maybeSecondPart <- optional $ signedFloatNoScientific <||> signedInteger
  char 'i'
  case maybeSecondPart of
    Just secondPart -> return $ RavenComplex firstPart secondPart
    Nothing -> return $ RavenComplex 0 firstPart
  where signedFloatNoScientific = signed (return ()) floatNoScientific
        floatNoScientific = do
          val1 <- some digitChar
          char '.'
          val2 <- some digitChar
          let units = (read val1) :: Double
          let fractions = (read ("0." ++ val2)) :: Double
          return $ units + fractions
        signedInteger = fromIntegral <$> (signed (return ()) integer)

number :: Parser Expression
number = complex
      <||> real
      <||> rational
      <||> integral


-- Helper functions

(<||>) :: Parser a -> Parser a -> Parser a
a <||> b = try a <|> b

bin2dec :: String -> Int
bin2dec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

stringI :: (Token s ~ Char, Text.Megaparsec.Prim.MonadParsec e s m) => String -> m String
stringI = Text.Megaparsec.string'  -- case insensitive string helper

stringS :: (Token s ~ Char, Text.Megaparsec.Prim.MonadParsec e s m) => String -> m String
stringS = Text.Megaparsec.string   -- case sensitive string helper
