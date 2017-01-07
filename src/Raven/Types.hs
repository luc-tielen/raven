
module Raven.Types ( Identifier
                   , Variable
                   , Expression(..)
                   , module Raven.Number
                   ) where

import Raven.Number


type Identifier = String
type Variable = Identifier  -- an identifier that is not a keyword TODO newtype?

data Expression = RavenBool Bool  -- TODO group literals together?
                | RavenString String
                | RavenNumber Number
                | RavenSymbol String
                | RavenDefine Variable Expression
  deriving (Eq, Show)

