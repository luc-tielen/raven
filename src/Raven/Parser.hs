
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Raven.Parser ( parse
                    , bool
                    , string
                    , number
                    , identifier
                    , variable
                    , symbol
                    , define
                    , functionCall
                    , lambda
                    , ifExpr
                    , assignment
                    , andExpr
                    , orExpr
                    , begin
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
  where trueExpression = (stringS "true") >> return (RavenLiteral $ RavenBool True)
        falseExpression = (stringS "false") >> return (RavenLiteral $ RavenBool False)

string :: Parser Expression
string = lexeme $ RavenLiteral . RavenString <$> between doubleQuote doubleQuote stringCharacters
  where doubleQuote = char '\"'
        stringCharacters = many $ noneOf "\"" 

integral :: Parser Expression
integral = lexeme $ RavenLiteral . RavenNumber . RavenIntegral <$> do
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
rational = lexeme $ RavenLiteral . RavenNumber <$> do
  sign <- optional $ char '-'
  nominator <- some digitChar
  char '/'
  denominator <- some digitChar
  let signChar = fromMaybe ' ' sign
  return $ RavenRational (read $ signChar : nominator) (read denominator)

real :: Parser Expression
real = lexeme $ RavenLiteral . RavenNumber . RavenReal <$> float

complex :: Parser Expression
complex = lexeme $ RavenLiteral . RavenNumber <$> do
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

symbol :: Parser Expression
symbol = do
  quote
  value <- identifier
  return $ RavenLiteral $ RavenSymbol value
  where quote = char '\''

variable' :: Parser Variable
variable' = do
  notFollowedBy keywords <?> "A variable cannot have a reserved keyword as name."  -- Technically this is allowed in R5RS scheme, but disallowed here
  identifier
  where keywords = choice $ map stringS listOfKeywords

variable :: Parser Expression
variable = RavenVariable <$> variable'

define :: Parser Expression
define = betweenParens $ do
  lexeme $ stringS "def"
  defineVariable <|> defineFunction
  where defineVariable = do
          var <- variable'
          value <- literal
          return $ RavenDefine var value
        defineFunction = do
          (var, args) <- lexeme . betweenParens $ do
            var' <- variable'
            args' <- variable' `sepBy` ws
            return (var', args')
          expressions <- some expression
          return $ RavenDefine var $ makeFunction args expressions

functionCall :: Parser Expression
functionCall = betweenParens $ do
  op <- expression
  args <- expression `sepBy` ws
  return $ RavenFunctionCall op args

lambda :: Parser Expression
lambda = betweenParens $ do
  lexeme $ stringS "lambda"
  args <- lexeme . betweenParens $ variable' `sepBy` ws
  expressions <- some expression
  return $ makeFunction args expressions

ifExpr :: Parser Expression
ifExpr = betweenParens $ do
  stringS "if "
  test <- expression
  ifClause <- expression
  elseClause <- optional expression
  return $ RavenIf $ If test ifClause elseClause

assignment :: Parser Expression
assignment = betweenParens $ do
  lexeme $ stringS "set!"
  var <- variable'
  expr <- expression
  return $ RavenAssign $ Assign var expr

andExpr :: Parser Expression
andExpr = betweenParens $ do
  lexeme $ stringS "and"
  args <- many expression
  return $ RavenAnd . And $ args

orExpr :: Parser Expression
orExpr = betweenParens $ do
  lexeme $ stringS "or"
  args <- many expression
  return $ RavenOr . Or $ args

begin :: Parser Expression
begin = betweenParens $ do
  lexeme $ stringS "begin"
  expressions <- some expression
  return $ RavenBegin . Begin $ expressions

expression :: Parser Expression
expression = lexeme $ variable
          <|> literal
          <|> functionCall
          <|> lambda
          <|> ifExpr
          <|> assignment
          <|> define
          <|> andExpr
          <|> orExpr
          <|> begin
          -- TODO add rest later

-- Parser related helper functions

(<||>) :: Parser a -> Parser a -> Parser a
a <||> b = try a <|> b

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
        commentParser = skipLineComment ";"
        blockCommentParser = empty

-- Helper for creating lexemes that consume trailing whitespace
lexeme :: (Token s ~ Char, Text.Megaparsec.Prim.MonadParsec e s m) => m a -> m a
lexeme = L.lexeme ws

betweenParens :: Parser a -> Parser a
betweenParens = between openParen closeParen
  where openParen = char '('
        closeParen = char ')'


-- General helper functions

listOfKeywords :: [String]
listOfKeywords = [ "def"
                 , "lambda"
                 , "true"
                 , "false"
                 , "and"
                 , "or"
                 , "begin"
                 , "set!"
                 , "if"
                 ]  -- TODO add more keywords while implementing them

bin2dec :: String -> Int
bin2dec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

makeFunction :: [Variable] -> [Expression] -> Expression
makeFunction args expressions = RavenFunction $ Function args expressions
