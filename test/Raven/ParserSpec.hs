
module Raven.ParserSpec ( spec ) where

import Test.Tasty.Hspec
import Test.QuickCheck
import Test.Hspec.Megaparsec
import Text.Megaparsec hiding (parse, string)
import Raven.Types
import Raven.Parser


spec :: Spec
spec = describe "Parser" $ do
    describe "parsing bools" $ do
      it "should be able to parse true and false" $ do
        parse bool "true" `shouldParse` RBool True
        parse bool "false" `shouldParse` RBool False

      it "should fail to parse invalid input" $ do
        parse bool `shouldFailOn` "truf" 
        parse bool `shouldFailOn` "fals"
        parse bool `shouldFailOn` ""


    describe "parsing strings" $ do
      it "should be able to parse valid strings" $ do
        parse string "\"\"" `shouldParse` RString ""
        parse string "\"a\"" `shouldParse` RString "a"
        parse string "\"ab\"" `shouldParse` RString "ab"
        parse string "\"a b\"" `shouldParse` RString "a b"
        parse string "\"a  b\"" `shouldParse` RString "a  b"

      it "should be able to parse escape characters" $ do
        parse string "\"a\rb\"" `shouldParse` RString "a\rb"
        parse string "\"a\nb\"" `shouldParse` RString "a\nb"
        parse string "\"a\tb\"" `shouldParse` RString "a\tb"
        parse string "\"a\bb\"" `shouldParse` RString "a\bb"
        parse string "\"a\vb\"" `shouldParse` RString "a\vb"
        parse string "\"a\\b\"" `shouldParse` RString "a\\b"
        parse string "\"a\'b\"" `shouldParse` RString "a\'b"
 
      it "should fail to parse invalid input" $ do
        parse string `shouldFailOn` ""
        parse string `shouldFailOn` "\""
        parse string `shouldFailOn` "\"a"
        parse string `shouldFailOn` "a\""


    describe "parsing comments" $ do
      it "should be able to parse valid comments" $ do
        parse comment ";;" `shouldParse` RComment ""
        parse comment ";;a" `shouldParse` RComment "a"
        parse comment ";; a" `shouldParse` RComment " a"
        parse comment ";; a\n" `shouldParse` RComment " a"
        
      it "should fail to parse invalid comments" $ do
        parse comment `shouldFailOn` "" 
        parse comment `shouldFailOn` ";" 
        parse comment `shouldFailOn` "; ;"  


    describe "parsing symbols" $ do
      it "should be able to parse valid symbols" $ do
        parse symbol "a" `shouldParse` RSymbol "a"
        parse symbol "ab" `shouldParse` RSymbol "ab"
        parse symbol "a1" `shouldParse` RSymbol "a1"
        
      it "should be able to parse symbols containing 'extended chars'" $ do
        parse symbol "a+-.*/<=>!?$%_&^,~" `shouldParse` RSymbol "a+-.*/<=>!?$%_&^,~"
        
      it "should fail to parse invalid symbols" $ do
        parse symbol `shouldFailOn` "1"
        parse symbol `shouldFailOn` "1a"
        parse symbol `shouldFailOn` "1\n"
        parse symbol `shouldFailOn` "!"

      -- TODO test to check for overlap with keywords

    describe "parsing numbers" $ do
      it "should be able to parse (positive) decimal numbers" $ do
        let checkInt a b = parse number a `shouldParse` RNumber (Integral b)
        checkInt "0" 0
        checkInt "1" 1
        checkInt "2" 2
        checkInt "11" 11
        -- NOTE: negative integers are represented as (- X) -> handled with a different parser

      it "should be able to parse hexadecimal numbers" $ do
        let checkHex a b = parse number a `shouldParse` RNumber (Integral b)
        checkHex "0x0" 0
        checkHex "0x1" 1
        checkHex "0x2" 2
        checkHex "0xa" 0x0A
        checkHex "0xf" 0x0F
        checkHex "0xA" 0x0A
        checkHex "0x0F" 0x0F
        checkHex "0xFF" 0xFF

      it "should be able to parse binary numbers" $ do
        let checkBin a b = parse number a `shouldParse` RNumber (Integral b)
        checkBin "0b0" 0
        checkBin "0b1" 1
        checkBin "0b01" 1
        checkBin "0b10" 2
        checkBin "0b11" 3
        checkBin "0b1000" 8

      --it "should be able to parse real (floating point) numbers" $ do
        --let checkDouble a b = parse number a `shouldParse` RNumber (Real b)
        --checkDouble "0.0" 0.0
        --checkDouble "0.1" 0.1
        --checkDouble "1.1" 1.1
        --checkDouble "1e3" 1000
        -- NOTE: negative floats not checked here, represented as (- F)
  
      --it "should be able to parse complex numbers" $ do
        --let checkComplex a b c = parse number a `shouldParse` RNumber (Complex b c)
        --let checkComplexI a b = checkComplex a 0 c
        --checkComplexI "0i" 0
        --checkComplexI "1i" 1
        --checkComplexI "0.1i" 1
        --checkComplex "1+1i" 1 1
        --checkComplex "1+0.1i" 1 0.1
        --checkComplex "0.1+0.1i" 0.1 0.1

    -- TODO rational
    -- TODO check bad cases
