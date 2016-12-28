
-- Helper module for dealing with the numeric tower

module Raven.Number ( Number(..) ) where

type Nominator = Int
type Denominator = Int
type RealPart = Double
type ImagPart = Double
-- TODO rename to avoid name clashes with builtin haskell types
data Number = Integral Int
            | Rational Nominator Denominator  -- Assumes Denominator >= 0 (TODO enforce with type)
            | Real Double
            | Complex RealPart ImagPart


instance Show Number where
  show (Integral a) = show a
  show (Rational n d) = show n ++ "/" ++ show d
  show (Real a) = show a
  show (Complex r i) = stringRepresentation
    where stringRepresentation = realPart ++ imagPart ++ "i"
          realPart = if r == 0 then "" else show r
          imagPart
            | r /= 0 && i >= 0 = "+" ++ show i
            | otherwise        = show i

  
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


instance Num Number where
  Integral a + Integral b = Integral $ a + b
  Integral a + Rational n d = Rational (n + a * d) d  -- TODO simplify fraction?
  Integral a + Real b = Real $ (fromIntegral a) + b
  Integral a + Complex r i = Complex ((fromIntegral a) + r) i
  Rational n1 d1 + Rational n2 d2 = Rational (n1 * d2 + n2 * d1) (d1 * d2)  -- TODO simplify fraction?
  Rational n d + Real a = Real $ (fromIntegral n / fromIntegral d) + a
  Rational n d + Complex r i = Complex ((fromIntegral n / fromIntegral d) + r) i
  Real a + Real b = Real $ a + b
  Real a + Complex r i = Complex (a + r) i
  Complex r1 i1 + Complex r2 i2 = Complex (r1 + r2) (i1 + i2)
  a + b = b + a  -- fallback case, swaps arguments

  Integral a - Integral b = Integral $ a - b
  Integral a - Rational n d = Rational (a * d - n) d  -- TODO simplify fraction?
  Integral a - Real b = Real $ (fromIntegral a) - b
  Integral a - Complex r i = Complex ((fromIntegral a) - r) (negate i)
  Rational n d - Integral a = Rational (n - a * d) d  -- TODO simplify fraction?
  Rational n1 d1 - Rational n2 d2 = Rational (n1 * d2 - n2 * d1) (d1 * d2)  -- TODO simplify fraction?
  Rational n d - Real a = Real $ (fromIntegral n / fromIntegral d) - a
  Rational n d - Complex r i = Complex ((fromIntegral n / fromIntegral d) - r) (negate i)
  Real b - Integral a = Real $ b -(fromIntegral a)
  Real a - Rational n d = Real $ a - (fromIntegral n / fromIntegral d)
  Real a - Real b = Real $ a - b
  Real a - Complex r i = Complex (a - r) (negate i)
  Complex r i - Integral a = Complex (r - (fromIntegral a)) i
  Complex r i - Rational n d = Complex (r - (fromIntegral n / fromIntegral d)) i
  Complex r i - Real a = Complex (r - a) i
  Complex r1 i1 - Complex r2 i2 = Complex (r1 - r2) (i1 - i2)
  
  Integral a * Integral b = Integral $ a * b
  Integral a * Rational n d = Rational (a * n) d  -- TODO simplify fraction?
  Integral a * Real b = Real $ (fromIntegral a) * b
  Integral a * Complex r i = Complex (a' * r) (a' * i) where a' = fromIntegral a
  Rational n1 d1 * Rational n2 d2 = Rational (n1 * n2) (d1 * d2)  -- TODO simplify fraction?
  Rational n d * Real a = Real $ a * fromIntegral n / fromIntegral d
  Rational n d * Complex r i = Complex (f * r) (f * i) where f = fromIntegral n / fromIntegral d
  Real a * Real b = Real $ a * b
  Real a * Complex r i = Complex (a * r) (a * i)
  Complex r1 i1 * Complex r2 i2 = Complex (r1 * r2 - i1 * i2) (r1 * i2 + i1 * r2)
  a * b = b * a  -- fallback case, swaps arguments

  abs (Integral a) = Integral $ abs a
  abs (Rational n d) = Rational (abs n) d
  abs (Real a) = Real $ abs a
  abs (Complex _ _) = error "abs not supported for complex numbers!"

  signum (Integral a) = Integral $ signum a
  signum (Rational n _) = Integral $ signum n
  signum (Real a) = Integral $ round $ signum a
  signum (Complex _ _) = error "signum not supported for complex numbers!"

  fromInteger = Integral . fromInteger
