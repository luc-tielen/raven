
module Raven.Types ( Expr(..)
                   ) where


data Expr = RBool Bool
          | RString String
          | RComment String
          | RSymbol String
  deriving (Eq, Show)
