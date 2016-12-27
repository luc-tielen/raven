
-- Helper module for dealing with the numeric tower

module Raven.Number ( Number(..) ) where

type Nominator = Int
type Denominator = Int
type RealPart = Double
type ImagPart = Double
data Number = Integral Int
            | Rational Nominator Denominator
            | Real Double
            | Complex RealPart ImagPart
  deriving (Show)


-- TODO: forbid comparison on Double (with a 'SafeDouble' type)?
instance Eq Number where
  Integral a == Integral b = a == b
  Integral a == Rational n d = a * d == n
  Integral a == Real b = (fromIntegral a) == b
  Integral a == Complex r i = i == 0 && (fromIntegral a) == r
  Rational n1 d1 == Rational n2 d2 = n1 * d2 == n2 * d1
  Rational n d == Real a = (fromIntegral n / fromIntegral d) == a
  Rational n d == Complex r i = i == 0 && (fromIntegral n / fromIntegral d) == r
  Real a == Real b = a == b
  Real a == Complex r i = i == 0 && a == r
  Complex r1 i1 == Complex r2 i2 = r1 == r2 && i1 == i2
  a == b = b == a  -- fallback case, swaps arguments -> ends up in other cases
