
module Raven.Types ( Identifier
                   , Variable
                   , Operator
                   , Operand
                   , Literal(..)
                   , Function(..)
                   , AndExpression(..)
                   , Expression(..)
                   , module Raven.Number
                   ) where

import Raven.Number


type Identifier = String
type Variable = Identifier  -- an identifier that is not a keyword TODO newtype?
type Operator = Expression
type Operand  = Expression


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

-- Representation of special form 'and'
data AndExpression = And [Expression]
  deriving (Eq, Show)

data Expression = RavenVariable Variable
                | RavenLiteral Literal
                | RavenFunctionCall Operator [Operand]
                | RavenFunction Function
                | RavenDefine Variable Expression
                | RavenAnd AndExpression
                deriving (Eq, Show)

