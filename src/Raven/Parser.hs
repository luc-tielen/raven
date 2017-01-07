
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Raven.Parser ( parse
                    , bool
                    , string
                    , number
                    , comment
                    , symbol
                    , identifier
                    , define
                    ) where

import Text.Megaparsec hiding (parse, string, string')
import qualified Text.Megaparsec (parse, string, string')
import Text.Megaparsec.Prim hiding (parse)
import Text.Megaparsec.Lexer as L hiding (number, symbol, space, lexeme)
import qualified Text.Megaparsec.Lexer as L (space, lexeme)
import Text.Megaparsec.String
import Control.Applicative (empty)
import Data.Functor.Identity
import Data.List (foldl')
import Data.Char (digitToInt)
import Data.Maybe
import Raven.Types


parse :: Parsec e s a -> s -> Either (ParseError (Token s) e) a
parse parser stream = Text.Megaparsec.parse parser "" stream

bool :: Parser Expression
bool = lexeme $ trueExpression <|> falseExpression
  where trueExpression = (stringS "true") >> return (RavenBool True)
        falseExpression = (stringS "false") >> return (RavenBool False)

string :: Parser Expression
string = lexeme $ RavenString <$> between doubleQuote doubleQuote stringCharacters
  where doubleQuote = char '\"'
        stringCharacters = many $ noneOf "\"" 

comment :: Parser Expression
comment = RavenComment <$> (stringS ";;" >> (many $ noneOf "\n"))

symbol :: Parser Expression
symbol = lexeme $ RavenSymbol <$> symbolParser
  where symbolParser = do
          firstChar <- letterChar
          restChars <- many symbolChars
          return $ firstChar : restChars
        symbolChars = alphaNumChar <|> oneOf "+-.*/<=>!?$%_&^,~"

integral :: Parser Expression
integral = lexeme $ RavenNumber . RavenIntegral <$> do
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
rational = lexeme $ RavenNumber <$> do
  sign <- optional $ char '-'
  nominator <- some digitChar
  char '/'
  denominator <- some digitChar
  let signChar = fromMaybe ' ' sign
  return $ RavenRational (read $ signChar : nominator) (read denominator)

real :: Parser Expression
real = lexeme $ RavenNumber . RavenReal <$> float

complex :: Parser Expression
complex = lexeme $ RavenNumber <$> do
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

-- TODO fix bugs when parsing invalid numbers (see tests)
number :: Parser Expression
number = complex
      <||> real
      <||> rational
      <||> integral

identifier :: Parser Identifier
identifier = lexeme $ normalIdentifier <|> peculiarIdentifier
  where normalIdentifier = do
          initial <- initialChar
          subsequent <- many subsequentChar
          return (initial : subsequent)
        initialChar = letterChar <|> specialInitialChar
        subsequentChar = initialChar <|> digitChar <|> specialSubsequentChar
        specialInitialChar = oneOf "!$%&*/:<=>?^_~"
        specialSubsequentChar = oneOf "+-.@"
        peculiarIdentifier = stringS "+" <|> stringS "-" <|> stringS "..."

define :: Parser Expression
define = betweenParens $ do
  lexeme $ stringS "def"
  id <- identifier
  value <- literal
  return $ RavenDefine id value


-- Helper functions

(<||>) :: Parser a -> Parser a -> Parser a
a <||> b = try a <|> b

bin2dec :: String -> Int
bin2dec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

stringI :: (Token s ~ Char, Text.Megaparsec.Prim.MonadParsec e s m) => String -> m String
stringI = Text.Megaparsec.string'  -- case insensitive string helper

stringS :: (Token s ~ Char, Text.Megaparsec.Prim.MonadParsec e s m) => String -> m String
stringS = Text.Megaparsec.string   -- case sensitive string helper

literal :: Parser Expression
literal = bool <|> string <|> symbol <|> number  -- TODO add remaining types later...

-- White space parser
ws :: (Token s ~ Char, Text.Megaparsec.Prim.MonadParsec e s m) => m ()
ws = L.space spaceParser commentParser blockCommentParser
  where spaceParser = spaceChar >> return ()
        commentParser = empty
        blockCommentParser = empty

-- Helper for creating lexemes that consume trailing whitespace
lexeme :: (Token s ~ Char, Text.Megaparsec.Prim.MonadParsec e s m) => m a -> m a
lexeme = L.lexeme ws

betweenParens :: Parser a -> Parser a
betweenParens = between openParen closeParen
  where openParen = char '('
        closeParen = char ')'
