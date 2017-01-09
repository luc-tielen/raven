
module Raven.Types ( Identifier
                   , Variable
                   , Operator
                   , Operand
                   , Literal(..)
                   , Function(..)
                   , IfExpression(..)
                   , Assignment(..)
                   , AndExpression(..)
                   , OrExpression(..)
                   , BeginExpression(..)
                   , Expression(..)
                   , module Raven.Number
                   ) where

import Raven.Number


type Identifier = String
type Variable = Identifier  -- an identifier that is not a keyword TODO newtype?
type Operator = Expression
type Operand  = Expression
type TrueClause = Expression
type FalseClause = Maybe Expression

-- Literals evaluate to themselves
data Literal = RavenBool Bool
             | RavenNumber Number
             | RavenString String
             | RavenSymbol Identifier
             -- TODO add quoted representation
             deriving (Eq, Show)

-- This corresponds with a lambda in code
data Function = Function [Variable] [Expression]  -- TODO add environment
  deriving (Eq, Show)

data IfExpression = If Expression TrueClause FalseClause
  deriving (Eq, Show)

data Assignment = Assign Variable Expression
  deriving (Eq, Show)

-- Representation of special form 'and'
data AndExpression = And [Expression]
  deriving (Eq, Show)

-- Representation of special form 'or'
data OrExpression = Or [Expression]
  deriving (Eq, Show)

-- Representation of special form 'begin'
data BeginExpression = Begin [Expression]
  deriving (Eq, Show)

data Expression = RavenVariable Variable
                | RavenLiteral Literal
                | RavenFunctionCall Operator [Operand]
                | RavenFunction Function
                | RavenIf IfExpression
                | RavenAssign Assignment
                | RavenDefine Variable Expression
                | RavenAnd AndExpression
                | RavenOr OrExpression
                | RavenBegin BeginExpression
                | RavenDelay Expression
                deriving (Eq, Show)

