
module Raven.Types ( Expr(..)
                   , module Raven.Number
                   ) where

import Raven.Number

data Expr = RBool Bool
          | RString String
          | RNumber Number
          | RComment String
          | RSymbol String
  deriving (Eq, Show)


