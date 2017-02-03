
module Raven.Types ( Identifier
                   , Variable
                   , Operator
                   , Operand
                   , Literal(..)
                   , Function(..)
                   , IfExpression(..)
                   , CondExpression(..)
                   , CaseExpression(..)
                   , Assignment(..)
                   , AndExpression(..)
                   , OrExpression(..)
                   , BeginExpression(..)
                   , DoExpression(..)
                   , Expression(..)
                   , module Raven.Number
                   ) where

import Raven.Number


type Identifier = String
type Variable = Identifier  -- an identifier that is not a keyword TODO newtype?
type Operator = Expression
type Operand  = Expression
type TestExpression = Expression
type TrueClause = Expression
type FalseClause = Maybe Expression
type CondClause = [Expression]
type ElseClause = [Expression]
type CaseMatchList = [Expression]
type CaseClause = (CaseMatchList, [Expression])
type VariableExpression = Expression
type DoInitExpression = Expression
type DoStepExpression = Expression


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

-- Representation of special form 'if'
data IfExpression = If TestExpression TrueClause FalseClause
  deriving (Eq, Show)

-- Representation of special form 'cond'
data CondExpression = Cond [CondClause] ElseClause
  deriving (Eq, Show)

-- Representation of special form 'case'
data CaseExpression = Case Expression [CaseClause] ElseClause
  deriving (Eq, Show)

-- Representation of special form 'set!'
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

-- Representation of special form 'do'
type DoSetup = [(VariableExpression, DoInitExpression, DoStepExpression)]
type DoTest = (TestExpression, [Expression])
type DoCommand = [Expression]
data DoExpression = Do DoSetup DoTest DoCommand
  deriving (Eq, Show)

data Expression = RavenVariable Variable
                | RavenLiteral Literal
                | RavenFunctionCall Operator [Operand]
                | RavenFunction Function
                | RavenIf IfExpression
                | RavenCond CondExpression
                | RavenCase CaseExpression
                | RavenAssign Assignment
                | RavenDefine Variable Expression
                | RavenAnd AndExpression
                | RavenOr OrExpression
                | RavenBegin BeginExpression
                | RavenDelay Expression
                | RavenDo DoExpression
                deriving (Eq, Show)

