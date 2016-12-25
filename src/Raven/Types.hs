
module Raven.Types ( Expr(..)
                   ) where


data Expr = RBool Bool
          | RString String
          | RComment String
  deriving (Eq, Show)
