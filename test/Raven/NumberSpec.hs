
module Raven.NumberSpec ( spec ) where

import Test.Tasty.Hspec
import Test.QuickCheck
import Control.Exception
import Raven.Number


spec :: Spec
spec = describe "Numerical tower behavior" $ do
    describe "Eq" $ do
      it "should be possible to compare various numbers with each other" $ do
        Integral 1 == Integral 1 `shouldBe` True
        Integral 1 == Integral 2 `shouldBe` False
        Integral 1 == Rational 10 10 `shouldBe` True
        Integral 1 == Rational 10 11 `shouldBe` False
        Rational 10 10 == Integral 1 `shouldBe` True
        Rational 10 11 == Integral 1 `shouldBe` False
        Integral 1 == Real 1 `shouldBe` True
        Integral 1 == Real 1.1 `shouldBe` False
        Real 1 == Integral 1 `shouldBe` True
        Real 1.1 == Integral 1 `shouldBe` False
        Integral 1 == Complex 1 0 `shouldBe` True
        Integral 1 == Complex 1 1 `shouldBe` False
        Complex 1 0 == Integral 1 `shouldBe` True
        Complex 1 1 == Integral 1 `shouldBe` False
        Rational 1 1 == Rational 1 1 `shouldBe` True
        Rational 1 1 == Rational 2 2 `shouldBe` True
        Rational 1 3 == Rational 2 6 `shouldBe` True
        Rational 1 1 == Rational 1 2 `shouldBe` False
        Rational 1 1 == Real 1 `shouldBe` True
        Rational 1 1 == Real 1.1 `shouldBe` False
        Real 1 == Rational 1 1 `shouldBe` True
        Real 1.1 == Rational 1 1 `shouldBe` False
        Rational 1 1 == Complex 1 0 `shouldBe` True
        Rational 1 1 == Complex 1 1 `shouldBe` False
        Real 1.1 == Real 1.1 `shouldBe` True
        Real 1.1 == Real 1.2 `shouldBe` False
        Real 1.1 == Complex 1.1 0 `shouldBe` True
        Real 1.1 == Complex 1.1 1 `shouldBe` False
        Complex 1.1 0 == Real 1.1 `shouldBe` True
        Complex 1.1 1 == Real 1.1 `shouldBe` False
        Complex 1 1 == Complex 1 1 `shouldBe` True
        Complex 1 1 == Complex 1 2 `shouldBe` False

    describe "Show" $ do
      it "should be possible to display the various numbers as human readable text" $ do
        let checkIntShow a b = show (Integral a) `shouldBe` b
        let checkRatShow a b c = show (Rational a b) `shouldBe` c
        let checkRealShow a b = show (Real a) `shouldBe` b
        let checkComplexShow a b c = show (Complex a b) `shouldBe` c
        checkIntShow 1 "1"
        checkIntShow 2 "2"
        checkIntShow 10 "10"
        checkIntShow 123456789 "123456789"
        checkRatShow 0 1 "0/1"
        checkRatShow 1 1 "1/1"
        checkRatShow 2 6 "2/6"
        checkRatShow (negate 2) 6 "-2/6"
        checkRatShow 88 113 "88/113"
        checkRealShow 0.1 "0.1"
        checkRealShow 1.0 "1.0"
        checkRealShow 1.00 "1.0"
        checkRealShow 12345.6789 "12345.6789"
        checkComplexShow 1 1 "1.0+1.0i"
        checkComplexShow 1 (negate 1) "1.0-1.0i"
        checkComplexShow (negate 1) 1 "-1.0+1.0i"
        checkComplexShow 0 1 "1.0i"
        checkComplexShow 0 (negate 1) "-1.0i"
        checkComplexShow 1 0 "1.0+0.0i"

    describe "Num" $ do
      it "should be possible to perform addition with numbers" $ property $ do
        \a b c n1 d1 n2 d2 r i -> d2 /= 0 && d1 /= 0 ==> do
          Integral a + Integral b `shouldBe` Integral (a + b)
          Integral a + Rational n1 d1 `shouldBe` Rational (a * d1) d1 + Rational n1 d1
          Integral a + Real c `shouldBe` Real (fromIntegral a + c)
          Integral a + Complex r i `shouldBe` Complex (fromIntegral a + r) i
          Rational n1 d1 + Integral a `shouldBe` Integral a + Rational n1 d1
          Rational n1 d1 + Rational n2 d2 `shouldBe` Rational (n1 * d2 + n2 * d1) (d1 * d2)
          Rational n1 d1 + Real c `shouldBe` Real ((fromIntegral n1 / fromIntegral d1) + c)
          Rational n1 d1 + Complex r i `shouldBe` Complex (r + (fromIntegral n1 / fromIntegral d1)) i
          Real c + Integral a `shouldBe` Integral a + Real c
          Real c + Rational n1 d1 `shouldBe` Rational n1 d1 + Real c
          Real c + Real r `shouldBe` Real (c + r)
          Real c + Complex r i `shouldBe` Complex (c + r) i
          Complex r i + Integral a `shouldBe` Integral a + Complex r i
          Complex r i + Rational n1 d1 `shouldBe` Rational n1 d1 + Complex r i
          Complex r i + Real c `shouldBe` Real c + Complex r i
          Complex r i + Complex c c `shouldBe` Complex (r + c) (i + c)

      it "should be possible to perform substraction with numbers" $ property $ do
        \a b c n1 d1 n2 d2 r i -> d2 /= 0 && d1 /= 0 ==> do
          Integral a - Integral b `shouldBe` Integral (a - b)
          Integral a - Rational n1 d1 `shouldBe` Rational (a * d1) d1 - Rational n1 d1
          Integral a - Real c `shouldBe` Real (fromIntegral a - c)
          Integral a - Complex r i `shouldBe` Complex (fromIntegral a - r) (negate i)
          Rational n1 d1 - Integral a `shouldBe` Rational (n1 - a * d1) d1
          Rational n1 d1 - Rational n2 d2 `shouldBe` Rational (n1 * d2 - n2 * d1) (d1 * d2)
          Rational n1 d1 - Real c `shouldBe` Real ((fromIntegral n1 / fromIntegral d1) - c)
          Rational n1 d1 - Complex r i `shouldBe` Complex ((fromIntegral n1 / fromIntegral d1) - r) (negate i)
          Real c - Integral a `shouldBe` Real (c - fromIntegral a)
          Real c - Rational n1 d1 `shouldBe` Real (c - (fromIntegral n1 / fromIntegral d1))
          Real c - Real r `shouldBe` Real (c - r)
          Real c - Complex r i `shouldBe` Complex (c - r) (negate i)
          Complex r i - Integral a `shouldBe` Complex (r - fromIntegral a) i
          Complex r i - Rational n1 d1 `shouldBe` Complex (r - fromIntegral n1 / fromIntegral d1) i
          Complex r i - Real c `shouldBe` Complex (r - c) i
          Complex r i - Complex c c `shouldBe` Complex (r - c) (i - c)

      it "should be possible to perform multiplication with numbers" $ property $ do
        \a b c n1 d1 n2 d2 r i -> d2 /= 0 && d1 /= 0 ==> do
          Integral a * Integral b `shouldBe` Integral (a * b)
          Integral a * Rational n1 d1 `shouldBe` Rational (a * d1) d1 * Rational n1 d1
          Integral a * Real c `shouldBe` Real ((fromIntegral a) * c)
          Integral a * Complex r i `shouldBe` Complex (fromIntegral a * r) (fromIntegral a * i)
          Rational n1 d1 * Integral a `shouldBe` Integral a * Rational n1 d1
          Rational n1 d1 * Rational n2 d2 `shouldBe` Rational (n1 * n2) (d1 * d2)
          Rational n1 d1 * Real c `shouldBe` Real (c * fromIntegral n1 / fromIntegral d1)
          Rational n1 d1 * Complex r i `shouldBe` Complex ((fromIntegral n1 / fromIntegral d1) * r) ((fromIntegral n1 / fromIntegral d1) * i)
          Real c * Integral a `shouldBe` Integral a * Real c
          Real c * Rational n1 d1 `shouldBe` Rational n1 d1 * Real c
          Real c * Real r `shouldBe` Real (c * r)
          Real c * Complex r i `shouldBe` Complex (c * r) (c * i)
          Complex r i * Integral a `shouldBe` Integral a * Complex r i
          Complex r i * Rational n1 d1 `shouldBe` Rational n1 d1 * Complex r i
          Complex r i * Real c `shouldBe` Real c * Complex r i
          Complex r i * Complex c c `shouldBe` Complex (r * c - i * c) (r * c + i * c)

      it "should be possible to calculate abs value for a number (except complex numbers)" $ do
        let checkIntAbs a b = abs (Integral a) `shouldBe` (Integral b)
        let checkRatAbs a b = abs (Rational a 1) `shouldBe` (Rational b 1)
        let checkRealAbs a b = abs (Real a) `shouldBe` (Real b)
        checkIntAbs 0 0
        checkIntAbs (negate 2) 2
        checkIntAbs 2 2
        checkRatAbs 0 0
        checkRatAbs (negate 2) 2
        checkRatAbs 2 2
        checkRealAbs 0 0
        checkRealAbs (negate 2) 2
        checkRealAbs 2 2
        
      it "raises an error if calculating abs for a complex number" $ do
        let checkAbs a b = evaluate (abs (Complex a b)) `shouldThrow` anyException
        checkAbs 0 0
        checkAbs 0 1
        checkAbs 1 0
        checkAbs 1 1

      it "should be possible to get the sign of a number (except complex numbers)" $ do
        let checkIntSign a b = signum (Integral a) `shouldBe` (Integral b)
        let checkRatSign a b = signum (Rational a 1) `shouldBe` (Integral b)
        let checkRealSign a b = signum (Real a) `shouldBe` (Integral b)
        checkIntSign 0 0
        checkIntSign (negate 2) (negate 1)
        checkIntSign 2 1
        checkRatSign 0 0
        checkRatSign (negate 2) (negate 1)
        checkRatSign 2 1
        checkRealSign 0 0
        checkRealSign (negate 2) (negate 1)
        checkRealSign 2 1

      it "raises an error if calculating the sign for a complex number" $ do
        let checkSign a b = evaluate (signum (Complex a b)) `shouldThrow` anyException
        checkSign 0 0
        checkSign 0 1
        checkSign 1 0
        checkSign 1 1
        
      it "should be possible to convert an Integer to a Number" $ do
        let checkFromInteger a b = ((fromInteger a) :: Number) `shouldBe` b
        checkFromInteger 0 (Integral 0)
        checkFromInteger 1 (Integral 1)
        checkFromInteger 10 (Integral 10)
        
