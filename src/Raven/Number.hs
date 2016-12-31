
-- Helper module for dealing with the numeric tower

module Raven.Number ( Number(..)
                    , simplify ) where

import Data.Ratio
type Numerator = Int
type Denominator = Int
type RavenRealPart = Double
type ImagPart = Double
data Number = RavenIntegral Int
            | RavenRational Numerator Denominator  -- Assumes Denominator >= 0 (TODO enforce with type)
            | RavenReal Double
            | RavenComplex RavenRealPart ImagPart


instance Show Number where
  show (RavenIntegral a) = show a
  show (RavenRational n d) = show n ++ "/" ++ show d
  show (RavenReal a) = show a
  show (RavenComplex r i) = stringRepresentation
    where stringRepresentation = realPart ++ imagPart ++ "i"
          realPart = if r == 0 then "" else show r
          imagPart
            | r /= 0 && i >= 0 = "+" ++ show i
            | otherwise        = show i

  
-- TODO: forbid comparison on Double (with a 'SafeDouble' type)?
instance Eq Number where
  RavenIntegral a == RavenIntegral b = a == b
  RavenIntegral a == RavenRational n d = a * d == n
  RavenIntegral a == RavenReal b = (fromIntegral a) == b
  RavenIntegral a == RavenComplex r i = i == 0 && (fromIntegral a) == r
  RavenRational n1 d1 == RavenRational n2 d2 = n1 * d2 == n2 * d1
  RavenRational n d == RavenReal a = (fromIntegral n / fromIntegral d) == a
  RavenRational n d == RavenComplex r i = i == 0 && (fromIntegral n / fromIntegral d) == r
  RavenReal a == RavenReal b = a == b
  RavenReal a == RavenComplex r i = i == 0 && a == r
  RavenComplex r1 i1 == RavenComplex r2 i2 = r1 == r2 && i1 == i2
  a == b = b == a  -- fallback case, swaps arguments -> ends up in other cases


instance Num Number where
  RavenIntegral a + RavenIntegral b = RavenIntegral $ a + b
  RavenIntegral a + RavenRational n d = simplify $ RavenRational (n + a * d) d
  RavenIntegral a + RavenReal b = RavenReal $ (fromIntegral a) + b
  RavenIntegral a + RavenComplex r i = RavenComplex ((fromIntegral a) + r) i
  RavenRational n1 d1 + RavenRational n2 d2 = simplify $ RavenRational (n1 * d2 + n2 * d1) (d1 * d2)
  RavenRational n d + RavenReal a = RavenReal $ (fromIntegral n / fromIntegral d) + a
  RavenRational n d + RavenComplex r i = RavenComplex ((fromIntegral n / fromIntegral d) + r) i
  RavenReal a + RavenReal b = RavenReal $ a + b
  RavenReal a + RavenComplex r i = RavenComplex (a + r) i
  RavenComplex r1 i1 + RavenComplex r2 i2 = RavenComplex (r1 + r2) (i1 + i2)
  a + b = b + a  -- fallback case, swaps arguments

  RavenIntegral a - RavenIntegral b = RavenIntegral $ a - b
  RavenIntegral a - RavenRational n d = simplify $ RavenRational (a * d - n) d
  RavenIntegral a - RavenReal b = RavenReal $ (fromIntegral a) - b
  RavenIntegral a - RavenComplex r i = RavenComplex ((fromIntegral a) - r) (negate i)
  RavenRational n d - RavenIntegral a = simplify $ RavenRational (n - a * d) d
  RavenRational n1 d1 - RavenRational n2 d2 = simplify $ RavenRational (n1 * d2 - n2 * d1) (d1 * d2)
  RavenRational n d - RavenReal a = RavenReal $ (fromIntegral n / fromIntegral d) - a
  RavenRational n d - RavenComplex r i = RavenComplex ((fromIntegral n / fromIntegral d) - r) (negate i)
  RavenReal b - RavenIntegral a = RavenReal $ b - (fromIntegral a)
  RavenReal a - RavenRational n d = RavenReal $ a - (fromIntegral n / fromIntegral d)
  RavenReal a - RavenReal b = RavenReal $ a - b
  RavenReal a - RavenComplex r i = RavenComplex (a - r) (negate i)
  RavenComplex r i - RavenIntegral a = RavenComplex (r - (fromIntegral a)) i
  RavenComplex r i - RavenRational n d = RavenComplex (r - (fromIntegral n / fromIntegral d)) i
  RavenComplex r i - RavenReal a = RavenComplex (r - a) i
  RavenComplex r1 i1 - RavenComplex r2 i2 = RavenComplex (r1 - r2) (i1 - i2)
  
  RavenIntegral a * RavenIntegral b = RavenIntegral $ a * b
  RavenIntegral a * RavenRational n d = simplify $ RavenRational (a * n) d
  RavenIntegral a * RavenReal b = RavenReal $ (fromIntegral a) * b
  RavenIntegral a * RavenComplex r i = RavenComplex (a' * r) (a' * i) where a' = fromIntegral a
  RavenRational n1 d1 * RavenRational n2 d2 = simplify $ RavenRational (n1 * n2) (d1 * d2)
  RavenRational n d * RavenReal a = RavenReal $ a * (fromIntegral n / fromIntegral d)
  RavenRational n d * RavenComplex r i = RavenComplex (f * r) (f * i) where f = fromIntegral n / fromIntegral d
  RavenReal a * RavenReal b = RavenReal $ a * b
  RavenReal a * RavenComplex r i = RavenComplex (a * r) (a * i)
  RavenComplex r1 i1 * RavenComplex r2 i2 = RavenComplex (r1 * r2 - i1 * i2) (r1 * i2 + i1 * r2)
  a * b = b * a  -- fallback case, swaps arguments

  abs (RavenIntegral a) = RavenIntegral $ abs a
  abs (RavenRational n d) = RavenRational (abs n) d
  abs (RavenReal a) = RavenReal $ abs a
  abs (RavenComplex _ _) = error "abs not supported for complex numbers!"

  signum (RavenIntegral a) = RavenIntegral $ signum a
  signum (RavenRational n _) = RavenIntegral $ signum n
  signum (RavenReal a) = RavenIntegral $ round $ signum a
  signum (RavenComplex _ _) = error "signum not supported for complex numbers!"

  fromInteger = RavenIntegral . fromInteger


instance Fractional Number where
  RavenIntegral a / RavenIntegral b = simplify $ RavenRational a b
  RavenIntegral a / RavenRational n d = simplify $ RavenRational (a * d) n
  RavenIntegral a / RavenReal b = RavenReal $ (fromIntegral a) / b
  RavenIntegral a / RavenComplex r i = RavenComplex (fromIntegral a) 0 / RavenComplex r i
  RavenRational n d / RavenIntegral a = simplify $ RavenRational n (a * d)
  RavenRational n1 d1 / RavenRational n2 d2 = simplify $ RavenRational n1 d1 * RavenRational d2 n2
  RavenRational n d / RavenReal a = RavenReal $ (fromIntegral n / fromIntegral d) / a
  RavenRational n d / RavenComplex r i = RavenComplex (fromIntegral n / fromIntegral d) 0 / RavenComplex r i
  RavenReal b / RavenIntegral a = RavenReal $ b / (fromIntegral a)
  RavenReal a / RavenRational n d = RavenReal $ a / (fromIntegral n / fromIntegral d)
  RavenReal a / RavenReal b = RavenReal $ a / b
  RavenReal a / RavenComplex r i = RavenComplex a 0 / RavenComplex r i
  RavenComplex r i / RavenIntegral a = RavenComplex (r / a') (i / a') where a' = fromIntegral a
  RavenComplex r i / RavenRational n d = RavenComplex (r / f) (i / f) where f = fromIntegral n / fromIntegral d
  RavenComplex r i / RavenReal a = RavenComplex (r / a) (i / a)
  RavenComplex r1 i1 / RavenComplex r2 i2 = nominator / denominator
    where nominator = RavenComplex r1 i1 * RavenComplex r2 i2'
          denominator = RavenReal $ (r2 * r2) + (i2' * i2')
          i2' = negate i2

  recip (RavenRational n d) = RavenRational d n
  recip a = (RavenIntegral 1) / a

  fromRational a = simplify $ RavenRational n d
    where n = fromInteger $ numerator a
          d = fromInteger $ denominator a

  -- TODO comparisons


-- Helper functions

simplify :: Number -> Number
simplify (RavenRational n d)
  | n == d = RavenIntegral 1
  | factor == d = RavenIntegral $ n `quot` factor
  | otherwise   = RavenRational (n `quot` factor) (d `quot` factor)
  where factor = gcd n d
simplify a = a
