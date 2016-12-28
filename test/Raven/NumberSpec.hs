
module Raven.NumberSpec ( spec ) where

import Test.Tasty.Hspec
import Test.QuickCheck
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
