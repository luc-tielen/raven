
module Raven.Types ( Identifier
                   , Variable
                   , Literal(..)
                   , Expression(..)
                   , module Raven.Number
                   ) where

import Raven.Number


type Identifier = String
type Variable = Identifier  -- an identifier that is not a keyword TODO newtype?

-- Literals evaluate to themselves
data Literal = RavenBool Bool
             | RavenNumber Number
             | RavenString String
             | RavenSymbol Identifier
             -- TODO add quoted representation
             deriving (Eq, Show)

data Expression = RavenLiteral Literal
                | RavenDefine Variable Expression
                deriving (Eq, Show)

