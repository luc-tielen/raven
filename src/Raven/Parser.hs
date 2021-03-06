
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
                    , cond
                    , caseExpr
                    , assignment
                    , andExpr
                    , orExpr
                    , begin
                    , delay
                    , doExpr
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
  notFollowedBy keywords <?> "A variable cannot have a reserved keyword as name"  -- Technically this is allowed in R5RS scheme, but disallowed here
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
  stringS "if"
  spaceChar
  test <- expression
  ifClause <- expression
  elseClause <- optional expression
  return $ RavenIf $ If test ifClause elseClause

elseClause :: Parser [Expression]
elseClause = betweenParens $ do
  lexeme $ stringS "else"
  some expression

cond :: Parser Expression
cond = betweenParens $ do
  lexeme $ stringS "cond"
  condClauses <- many condClause
  if condClauses == []
    then do elseClause' <- elseClause
            return $ RavenCond $ Cond condClauses elseClause'
    else do elseClause' <- optional elseClause
            let elseClause'' = fromMaybe [] elseClause'
            return $ RavenCond $ Cond condClauses elseClause''
  where condClause = do
          notFollowedBy elseClause
          lexeme $ betweenParens condClause'
        condClause' = condClauseWithArrow <||> some expression
        condClauseWithArrow = do
          expr <- expression
          lexeme $ stringS "=>"
          recipient <- expression
          return $ [expr, RavenFunctionCall recipient [expr]]

caseExpr :: Parser Expression
caseExpr = betweenParens $ do
  lexeme $ stringS "case"
  expr <- expression
  caseClauses <- many caseClause
  if caseClauses == []
    then do elseClause' <- elseClause
            return $ RavenCase $ Case expr caseClauses elseClause'
    else do elseClause' <- optional elseClause
            let elseClause'' = fromMaybe [] elseClause'
            return $ RavenCase $ Case expr caseClauses elseClause''
  where caseClause = do
          notFollowedBy elseClause
          lexeme $ betweenParens caseClause'
        caseClause' = do
          datums <- lexeme . betweenParens $ many expression  -- TODO change to datum!
          expressions <- some expression
          return (datums, expressions)

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

delay :: Parser Expression
delay = betweenParens $ do
  lexeme $ stringS "delay"
  expr <- expression
  return $ RavenDelay expr

doExpr :: Parser Expression
doExpr = betweenParens $ do
  lexeme $ stringS "do"
  doInit <- lexeme . betweenParens $ many iterationSpec
  doTest <- lexeme . betweenParens $ doTest'
  doCommands <- many expression
  return $ RavenDo $ Do doInit doTest doCommands
  where iterationSpec = lexeme . betweenParens $ do
          var <- variable
          init <- expression
          step <- optional expression
          let step' = fromMaybe var step
          return (var, init, step')
        doTest' = do
          test <- expression
          doResult <- many expression
          return (test, doResult)

expression :: Parser Expression
expression = lexeme
           $   variable
          <||> literal
          <||> functionCall
          <||> lambda
          <||> ifExpr
          <||> cond
          <||> caseExpr
          <||> assignment
          <||> define
          <||> andExpr
          <||> orExpr
          <||> begin
          <||> delay
          <||> doExpr
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
                 , "delay"
                 , "set!"
                 , "if"
                 , "else"
                 , "cond"
                 , "case"
                 , "do"
                 ]  -- TODO add more keywords while implementing them

bin2dec :: String -> Int
bin2dec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

makeFunction :: [Variable] -> [Expression] -> Expression
makeFunction args expressions = RavenFunction $ Function args expressions
