
module Raven.Types ( Expr(..)
                   ) where


data Expr = RBool (Bool)
          | RString (String)
  deriving (Eq, Show)
