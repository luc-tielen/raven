
module Raven.NumberSpec ( spec ) where

import Test.Tasty.Hspec
import Test.QuickCheck
import Control.Exception
import Data.Ratio
import Raven.Number


spec :: Spec
spec = describe "Numerical tower behavior" $ do
    describe "Eq" $ do
      it "should be possible to compare various numbers with each other" $ do
        RavenIntegral 1 == RavenIntegral 1 `shouldBe` True
        RavenIntegral 1 == RavenIntegral 2 `shouldBe` False
        RavenIntegral 1 == RavenRational 10 10 `shouldBe` True
        RavenIntegral 1 == RavenRational 10 11 `shouldBe` False
        RavenRational 10 10 == RavenIntegral 1 `shouldBe` True
        RavenRational 10 11 == RavenIntegral 1 `shouldBe` False
        RavenIntegral 1 == RavenReal 1 `shouldBe` True
        RavenIntegral 1 == RavenReal 1.1 `shouldBe` False
        RavenReal 1 == RavenIntegral 1 `shouldBe` True
        RavenReal 1.1 == RavenIntegral 1 `shouldBe` False
        RavenIntegral 1 == RavenComplex 1 0 `shouldBe` True
        RavenIntegral 1 == RavenComplex 1 1 `shouldBe` False
        RavenComplex 1 0 == RavenIntegral 1 `shouldBe` True
        RavenComplex 1 1 == RavenIntegral 1 `shouldBe` False
        RavenRational 1 1 == RavenRational 1 1 `shouldBe` True
        RavenRational 1 1 == RavenRational 2 2 `shouldBe` True
        RavenRational 1 3 == RavenRational 2 6 `shouldBe` True
        RavenRational 1 1 == RavenRational 1 2 `shouldBe` False
        RavenRational 1 1 == RavenReal 1 `shouldBe` True
        RavenRational 1 1 == RavenReal 1.1 `shouldBe` False
        RavenReal 1 == RavenRational 1 1 `shouldBe` True
        RavenReal 1.1 == RavenRational 1 1 `shouldBe` False
        RavenRational 1 1 == RavenComplex 1 0 `shouldBe` True
        RavenRational 1 1 == RavenComplex 1 1 `shouldBe` False
        RavenReal 1.1 == RavenReal 1.1 `shouldBe` True
        RavenReal 1.1 == RavenReal 1.2 `shouldBe` False
        RavenReal 1.1 == RavenComplex 1.1 0 `shouldBe` True
        RavenReal 1.1 == RavenComplex 1.1 1 `shouldBe` False
        RavenComplex 1.1 0 == RavenReal 1.1 `shouldBe` True
        RavenComplex 1.1 1 == RavenReal 1.1 `shouldBe` False
        RavenComplex 1 1 == RavenComplex 1 1 `shouldBe` True
        RavenComplex 1 1 == RavenComplex 1 2 `shouldBe` False

    describe "Ord" $ do
      it "should be possible to compare various kinds of numbers with each other" $ do
        let checkCmp a b c = (a `compare` b) `shouldBe` c
        checkCmp (RavenIntegral 0) (RavenIntegral 0) EQ 
        checkCmp (RavenIntegral 2) (RavenIntegral 1) GT 
        checkCmp (RavenIntegral 1) (RavenIntegral 2) LT 
        checkCmp (RavenIntegral 0) (RavenRational 0 1) EQ 
        checkCmp (RavenIntegral 0) (RavenRational 1 1) LT 
        checkCmp (RavenIntegral 2) (RavenRational 3 2) GT 
        checkCmp (RavenIntegral 0) (RavenReal 0) EQ 
        checkCmp (RavenIntegral 0) (RavenReal 0.1) LT 
        checkCmp (RavenIntegral 1) (RavenReal 0.1) GT

        checkCmp (RavenRational 4 1) (RavenIntegral 0) GT 
        checkCmp (RavenRational 1 1) (RavenIntegral 1) EQ 
        checkCmp (RavenRational 3 2) (RavenIntegral 2) LT 
        checkCmp (RavenRational 4 1) (RavenRational 3 2) GT 
        checkCmp (RavenRational 1 1) (RavenRational 3 3) EQ 
        checkCmp (RavenRational 3 2) (RavenRational 4 2) LT
        checkCmp (RavenRational 4 1) (RavenReal 0) GT 
        checkCmp (RavenRational 1 1) (RavenReal 1) EQ 
        checkCmp (RavenRational 3 2) (RavenReal 2) LT 

        checkCmp (RavenReal 1) (RavenIntegral 0) GT 
        checkCmp (RavenReal 1) (RavenIntegral 1) EQ 
        checkCmp (RavenReal 2) (RavenIntegral 3) LT 
        checkCmp (RavenReal 1) (RavenRational 1 2) GT 
        checkCmp (RavenReal 1) (RavenRational 1 1) EQ 
        checkCmp (RavenReal 2) (RavenRational 7 3) LT
        checkCmp (RavenReal 1) (RavenReal 0) GT 
        checkCmp (RavenReal 1) (RavenReal 1) EQ 
        checkCmp (RavenReal 2) (RavenReal 3) LT

      it "should fail comparing complex numbers with other numbers" $ do
        let checkCmpFail a b = evaluate (a `compare` b) `shouldThrow` anyException
        checkCmpFail (RavenIntegral 0) (RavenComplex 0 0)
        checkCmpFail (RavenRational 0 1) (RavenComplex 0 0)
        checkCmpFail (RavenReal 0) (RavenComplex 0 0)
        checkCmpFail (RavenComplex 0 0) (RavenComplex 0 0)
        checkCmpFail (RavenComplex 0 0) (RavenIntegral 0)
        checkCmpFail (RavenComplex 0 0) (RavenRational 0 1)
        checkCmpFail (RavenComplex 0 0) (RavenReal 0)
        
    describe "Show" $ do
      it "should be possible to display the various numbers as human readable text" $ do
        let checkIntShow a b = show (RavenIntegral a) `shouldBe` b
        let checkRatShow a b c = show (RavenRational a b) `shouldBe` c
        let checkRealShow a b = show (RavenReal a) `shouldBe` b
        let checkComplexShow a b c = show (RavenComplex a b) `shouldBe` c
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
          RavenIntegral a + RavenIntegral b `shouldBe` RavenIntegral (a + b)
          RavenIntegral a + RavenRational n1 d1 `shouldBe` RavenRational (a * d1) d1 + RavenRational n1 d1
          RavenIntegral a + RavenReal c `shouldBe` RavenReal (fromIntegral a + c)
          RavenIntegral a + RavenComplex r i `shouldBe` RavenComplex (fromIntegral a + r) i
          RavenRational n1 d1 + RavenIntegral a `shouldBe` RavenIntegral a + RavenRational n1 d1
          RavenRational n1 d1 + RavenRational n2 d2 `shouldBe` RavenRational (n1 * d2 + n2 * d1) (d1 * d2)
          RavenRational n1 d1 + RavenReal c `shouldBe` RavenReal ((fromIntegral n1 / fromIntegral d1) + c)
          RavenRational n1 d1 + RavenComplex r i `shouldBe` RavenComplex (r + (fromIntegral n1 / fromIntegral d1)) i
          RavenReal c + RavenIntegral a `shouldBe` RavenIntegral a + RavenReal c
          RavenReal c + RavenRational n1 d1 `shouldBe` RavenRational n1 d1 + RavenReal c
          RavenReal c + RavenReal r `shouldBe` RavenReal (c + r)
          RavenReal c + RavenComplex r i `shouldBe` RavenComplex (c + r) i
          RavenComplex r i + RavenIntegral a `shouldBe` RavenIntegral a + RavenComplex r i
          RavenComplex r i + RavenRational n1 d1 `shouldBe` RavenRational n1 d1 + RavenComplex r i
          RavenComplex r i + RavenReal c `shouldBe` RavenReal c + RavenComplex r i
          RavenComplex r i + RavenComplex c c `shouldBe` RavenComplex (r + c) (i + c)

      it "should be possible to perform substraction with numbers" $ property $ do
        \a b c n1 d1 n2 d2 r i -> d2 /= 0 && d1 /= 0 ==> do
          RavenIntegral a - RavenIntegral b `shouldBe` RavenIntegral (a - b)
          RavenIntegral a - RavenRational n1 d1 `shouldBe` RavenRational (a * d1) d1 - RavenRational n1 d1
          RavenIntegral a - RavenReal c `shouldBe` RavenReal (fromIntegral a - c)
          RavenIntegral a - RavenComplex r i `shouldBe` RavenComplex (fromIntegral a - r) (negate i)
          RavenRational n1 d1 - RavenIntegral a `shouldBe` RavenRational (n1 - a * d1) d1
          RavenRational n1 d1 - RavenRational n2 d2 `shouldBe` RavenRational (n1 * d2 - n2 * d1) (d1 * d2)
          RavenRational n1 d1 - RavenReal c `shouldBe` RavenReal ((fromIntegral n1 / fromIntegral d1) - c)
          RavenRational n1 d1 - RavenComplex r i `shouldBe` RavenComplex ((fromIntegral n1 / fromIntegral d1) - r) (negate i)
          RavenReal c - RavenIntegral a `shouldBe` RavenReal (c - fromIntegral a)
          RavenReal c - RavenRational n1 d1 `shouldBe` RavenReal (c - (fromIntegral n1 / fromIntegral d1))
          RavenReal c - RavenReal r `shouldBe` RavenReal (c - r)
          RavenReal c - RavenComplex r i `shouldBe` RavenComplex (c - r) (negate i)
          RavenComplex r i - RavenIntegral a `shouldBe` RavenComplex (r - fromIntegral a) i
          RavenComplex r i - RavenRational n1 d1 `shouldBe` RavenComplex (r - fromIntegral n1 / fromIntegral d1) i
          RavenComplex r i - RavenReal c `shouldBe` RavenComplex (r - c) i
          RavenComplex r i - RavenComplex c c `shouldBe` RavenComplex (r - c) (i - c)

      it "should be possible to perform multiplication with numbers" $ property $ do
        \a b c n1 d1 n2 d2 r i -> d2 /= 0 && d1 /= 0 ==> do
          RavenIntegral a * RavenIntegral b `shouldBe` RavenIntegral (a * b)
          RavenIntegral a * RavenRational n1 d1 `shouldBe` RavenRational (a * d1) d1 * RavenRational n1 d1
          RavenIntegral a * RavenReal c `shouldBe` RavenReal ((fromIntegral a) * c)
          RavenIntegral a * RavenComplex r i `shouldBe` RavenComplex (fromIntegral a * r) (fromIntegral a * i)
          RavenRational n1 d1 * RavenIntegral a `shouldBe` RavenIntegral a * RavenRational n1 d1
          RavenRational n1 d1 * RavenRational n2 d2 `shouldBe` RavenRational (n1 * n2) (d1 * d2)
          RavenRational n1 d1 * RavenReal c `shouldBe` RavenReal (c * (fromIntegral n1 / fromIntegral d1))
          RavenRational n1 d1 * RavenComplex r i `shouldBe` RavenComplex ((fromIntegral n1 / fromIntegral d1) * r) ((fromIntegral n1 / fromIntegral d1) * i)
          RavenReal c * RavenIntegral a `shouldBe` RavenIntegral a * RavenReal c
          RavenReal c * RavenRational n1 d1 `shouldBe` RavenRational n1 d1 * RavenReal c
          RavenReal c * RavenReal r `shouldBe` RavenReal (c * r)
          RavenReal c * RavenComplex r i `shouldBe` RavenComplex (c * r) (c * i)
          RavenComplex r i * RavenIntegral a `shouldBe` RavenIntegral a * RavenComplex r i
          RavenComplex r i * RavenRational n1 d1 `shouldBe` RavenRational n1 d1 * RavenComplex r i
          RavenComplex r i * RavenReal c `shouldBe` RavenReal c * RavenComplex r i
          RavenComplex r i * RavenComplex c c `shouldBe` RavenComplex (r * c - i * c) (r * c + i * c)

      it "should be possible to calculate abs value for a number (except complex numbers)" $ do
        let checkIntAbs a b = abs (RavenIntegral a) `shouldBe` (RavenIntegral b)
        let checkRatAbs a b = abs (RavenRational a 1) `shouldBe` (RavenRational b 1)
        let checkRealAbs a b = abs (RavenReal a) `shouldBe` (RavenReal b)
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
        let checkAbs a b = evaluate (abs (RavenComplex a b)) `shouldThrow` anyException
        checkAbs 0 0
        checkAbs 0 1
        checkAbs 1 0
        checkAbs 1 1

      it "should be possible to get the sign of a number (except complex numbers)" $ do
        let checkIntSign a b = signum (RavenIntegral a) `shouldBe` (RavenIntegral b)
        let checkRatSign a b = signum (RavenRational a 1) `shouldBe` (RavenIntegral b)
        let checkRealSign a b = signum (RavenReal a) `shouldBe` (RavenIntegral b)
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
        let checkSign a b = evaluate (signum (RavenComplex a b)) `shouldThrow` anyException
        checkSign 0 0
        checkSign 0 1
        checkSign 1 0
        checkSign 1 1
        
      it "should be possible to convert an Integer to a Number" $ do
        let checkFromInteger a b = ((fromInteger a) :: Number) `shouldBe` b
        checkFromInteger 0 (RavenIntegral 0)
        checkFromInteger 1 (RavenIntegral 1)
        checkFromInteger 10 (RavenIntegral 10)
        
    describe "Fractional" $ do
      it "should be possible to divide 2 numbers" $ property $ do
        \a b c n1 d1 n2 d2 r i ->
          a /= 0 && b /= 0 && c/= 0 && n1 /= 0 && d1 /= 0 && n2 /= 0 && d2 /= 0 ==> do
          let a' = fromIntegral a
          let f = (fromIntegral n1 / fromIntegral d1)
          RavenIntegral a / RavenIntegral b `shouldBe` RavenRational a b
          RavenIntegral a / RavenRational n1 d1 `shouldBe` RavenRational (a * d1) n1
          RavenIntegral a / RavenReal c `shouldBe` RavenReal (fromIntegral a / c)
          RavenIntegral a / RavenComplex r i `shouldBe` RavenComplex (fromIntegral a) 0 / RavenComplex r i
          RavenRational n1 d1 / RavenIntegral a `shouldBe` RavenRational n1 (a * d1)
          RavenRational n1 d1 / RavenRational n2 d2 `shouldBe` RavenRational n1 d1 * RavenRational d2 n2
          RavenRational n1 d1 / RavenReal c `shouldBe` RavenReal ((fromIntegral n1 / fromIntegral d1) / c)
          RavenRational n1 d1 / RavenComplex r i `shouldBe` RavenComplex (fromIntegral n1 / fromIntegral d1) 0 / RavenComplex r i
          RavenReal c / RavenIntegral a `shouldBe` RavenReal (c / (fromIntegral a))
          RavenReal c / RavenRational n1 d1 `shouldBe` RavenReal (c / (fromIntegral n1 / fromIntegral d1))
          RavenReal c / RavenReal r `shouldBe` RavenReal (c / r)
          RavenReal c / RavenComplex r i `shouldBe` RavenComplex c 0 / RavenComplex r i
          RavenComplex r i / RavenIntegral a `shouldBe` RavenComplex (r / a') (i / a') 
          RavenComplex r i / RavenRational n1 d1 `shouldBe` RavenComplex (r / f) (i / f)
          RavenComplex r i / RavenReal c `shouldBe` RavenComplex (r / c) (i / c)
          RavenComplex r i / RavenComplex c c `shouldBe` (RavenComplex r i * RavenComplex c (negate c)) / RavenReal (c * c * 2)
            

      it "should be possible to calculate the reciprocal" $ property $ do
        \a b n d r i ->
          a /= 0 && b /= 0 && n /= 0 && d/= 0 && r /= 0 && i /= 0 ==> do
          recip (RavenIntegral a) `shouldBe` RavenRational 1 a
          recip (RavenRational n d) `shouldBe` RavenRational d n
          recip (RavenReal b) `shouldBe` RavenReal (fromIntegral 1 / b)
          recip (RavenComplex r i) `shouldBe` RavenComplex (fromIntegral 1) 0 / RavenComplex r i

      it "should be possible to convert from a rational number" $ do
        fromRational (1 % 5) `shouldBe` (RavenRational 1 5)
        fromRational (2 % 10) `shouldBe` (RavenRational 1 5)
        fromRational (3 % 10) `shouldBe` (RavenRational 3 10)
        
    describe "Simplifying fractions" $ do
      it "works" $ do
        let checkSimplify a b c d = simplify (RavenRational a b) `shouldBe` (RavenRational c d)
        let checkSimplifyOther a = simplify a `shouldBe` a
        checkSimplify 1 2 1 2
        checkSimplify 2 1 2 1
        checkSimplify 2 4 1 2
        checkSimplify 33 6 11 2
        checkSimplifyOther (RavenIntegral 1)
        checkSimplifyOther (RavenReal 1)
        checkSimplifyOther (RavenComplex 1 1)
        simplify (RavenRational 1 1) `shouldBe` (RavenIntegral 1)
        simplify (RavenRational 5 5) `shouldBe` (RavenIntegral 1)
      
