
module Raven.Types ( Expression(..)
                   , module Raven.Number
                   ) where

import Raven.Number

data Expression = RavenBool Bool
                | RavenString String
                | RavenNumber Number
                | RavenComment String
                | RavenSymbol String
  deriving (Eq, Show)


