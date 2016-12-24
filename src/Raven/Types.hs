
module Raven.Types ( Expr(..)
                   ) where


data Expr = RLeftParen  -- TODO other expression types (start with bool)
          | RBool (Bool)
  deriving (Eq, Show)
