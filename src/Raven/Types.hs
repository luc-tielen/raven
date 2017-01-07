
module Raven.Types ( Identifier
                   , Expression(..)
                   , module Raven.Number
                   ) where

import Raven.Number


type Identifier = String

data Expression = RavenBool Bool  -- TODO group literals together?
                | RavenString String
                | RavenNumber Number
                | RavenSymbol String
                | RavenDefine Identifier Expression
  deriving (Eq, Show)

