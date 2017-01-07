
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
        parse bool "true" `shouldParse` (RavenLiteral $ RavenBool True)
        parse bool "false" `shouldParse` (RavenLiteral $ RavenBool False)

      it "should fail to parse invalid input" $ do
        parse bool `shouldFailOn` "truf" 
        parse bool `shouldFailOn` "fals"
        parse bool `shouldFailOn` ""
        parse bool `shouldFailOn` " "


    describe "parsing strings" $ do
      it "should be able to parse valid strings" $ do
        let checkStr a b = parse string a `shouldParse` (RavenLiteral $ RavenString b)
        checkStr "\"\"" ""
        checkStr "\"a\"" "a"
        checkStr "\"ab\"" "ab"
        checkStr "\"a b\"" "a b"
        checkStr "\"a  b\"" "a  b"

      it "should be able to parse escape characters" $ do
        let checkStr a b = parse string a `shouldParse` (RavenLiteral $ RavenString b)
        checkStr "\"a\r\n\t\b\v\\\'b\"" "a\r\n\t\b\v\\\'b"
 
      it "should fail to parse invalid input" $ do
        let checkFailStr a = parse string `shouldFailOn` a
        checkFailStr ""
        checkFailStr "\""
        checkFailStr "\"a"
        checkFailStr "a\""

    describe "parsing comments" $ do
      it "should be able to parse valid comments" $ do
        parse number "1 ; comment\n" `shouldParse` (RavenLiteral . RavenNumber $ RavenIntegral 1)
        parse define "(def a ; comment goes here\n 3)" `shouldParse` (RavenDefine "a" (RavenLiteral . RavenNumber $ RavenIntegral 3))
        parse number "1 ;; more comments\n" `shouldParse` (RavenLiteral . RavenNumber $ RavenIntegral 1)

    describe "parsing identifiers" $ do
      it "should be able to parse valid identifiers" $ do
        let checkIdent a = parse identifier a `shouldParse` a
        checkIdent "lambda"
        checkIdent "list->vector"
        checkIdent "+"
        checkIdent "<=?"
        checkIdent "the-word-recursion-has-many-meanings"
        checkIdent "q"
        checkIdent "soup"
        checkIdent "V17a"
        checkIdent "a34kTMNs"
        checkIdent "-"
        checkIdent "..."
        checkIdent "a!$%&*+-./:<=>?@^_~"

      it "should fail to parse invalid identifiers" $ do
        let checkFailIdent a = parse identifier `shouldFailOn` a
        checkFailIdent "1"
        checkFailIdent "1.0a"
        checkFailIdent "."
        checkFailIdent ".."

    describe "parsing symbols" $ do
      it "should be able to parse valid symbols" $ do
        let checkSymbol a b = parse symbol a `shouldParse` (RavenLiteral $ RavenSymbol b)
        checkSymbol "'a" "a"
        checkSymbol "'ab" "ab"
        checkSymbol "'a1" "a1"
        checkSymbol "'a+-.*/<=>!?$%_&^~" "a+-.*/<=>!?$%_&^~"
        
      it "should fail to parse invalid symbols" $ do
        let checkFailSymbol a = parse symbol `shouldFailOn` a
        checkFailSymbol "1"
        checkFailSymbol "1a"
        checkFailSymbol "1\n"
        checkFailSymbol "!"

        -- TODO test to check for overlap with keywords

    describe "parsing numbers" $ do
      it "should be able to parse (positive) decimal numbers" $ do
        let checkInt a b = parse number a `shouldParse` (RavenLiteral . RavenNumber . RavenIntegral $ b)
        checkInt "0" 0
        checkInt "1" 1
        checkInt "2" 2
        checkInt "11" 11
        -- NOTE: negative integers are represented as (- X) -> handled with a different parser

      it "should fail to parse invalid decimal numbers" $ do
        let checkFailInt a = parse number `shouldFailOn` a
        checkFailInt "-0"
        checkFailInt "-1"
        checkFailInt "a1"

      it "should be able to parse hexadecimal numbers" $ do
        let checkHex a b = parse number a `shouldParse` (RavenLiteral . RavenNumber . RavenIntegral $ b)
        checkHex "0x0" 0
        checkHex "0x1" 1
        checkHex "0x2" 2
        checkHex "0xa" 0x0A
        checkHex "0xf" 0x0F
        checkHex "0xA" 0x0A
        checkHex "0x0F" 0x0F
        checkHex "0xFF" 0xFF

      it "should fail to parse invalid hexadecimal numbers" $ do
        let checkFailHex a = parse number `shouldFailOn` a
        checkFailHex "0x"
        checkFailHex "0x 0"
        checkFailHex "0xG"
        checkFailHex "0x.1"
        --checkFailHex "0x1.0"
        --checkFailHex "0x0.1"
        
      it "should be able to parse binary numbers" $ do
        let checkBin a b = parse number a `shouldParse` (RavenLiteral . RavenNumber . RavenIntegral $ b)
        checkBin "0b0" 0
        checkBin "0b1" 1
        checkBin "0b01" 1
        checkBin "0b10" 2
        checkBin "0b11" 3
        checkBin "0b1000" 8
        
      it "should fail to parse invalid binary numbers" $ do
        let checkFailBin a = parse number `shouldFailOn` a
        checkFailBin "0b"
        checkFailBin "0b 0"
        checkFailBin "0b2"
        checkFailBin "0b.1"
        --checkFailBin "0b1.0"

      it "should be able to parse rational numbers" $ do
        let checkRat a b c = parse number a `shouldParse` (RavenLiteral . RavenNumber $ RavenRational b c)
        checkRat "0/1" 0 1
        checkRat "1/1" 1 1
        checkRat "1/2" 1 2
        checkRat "3/2" 3 2
        checkRat "-3/2" (negate 3) 2

      --it "should fail to parse invalid rational numbers" $ do
        --let checkFailRat a = parse number `shouldFailOn` a
        --checkFailRat "0.1/1"
        --checkFailRat "1i/1"
        --checkFailRat "1 / 1"
        --checkFailRat "1/ 1"
        --checkFailRat "1 /1"

      it "should be able to parse real (floating point) numbers" $ do
        let checkDouble a b = parse number a `shouldParse` (RavenLiteral . RavenNumber . RavenReal $ b)
        checkDouble "0.0" 0.0
        checkDouble "0.1" 0.1
        checkDouble "1.1" 1.1
        checkDouble "1e3" 1000
        checkDouble "1e-3" 0.001
        -- NOTE: negative floats not checked here, represented as (- F)

      it "should fail to parse invalid real (floating point) numbers" $ do
        let checkFailDouble a = parse number `shouldFailOn` a
        checkFailDouble ".1"
        checkFailDouble "a.1"
        checkFailDouble ".a1"
        checkFailDouble "-1e3"
  
      it "should be able to parse complex numbers" $ do
        let checkComplex a b c = parse number a `shouldParse` (RavenLiteral . RavenNumber $ RavenComplex b c)
        let checkComplexI a b = checkComplex a 0 b
        checkComplexI "0i" 0
        checkComplexI "0.1i" 0.1
        checkComplexI "1i" 1
        checkComplex "1+1i" 1 1
        checkComplex "1+0.1i" 1 0.1
        checkComplex "0.1+0.1i" 0.1 0.1
        checkComplex "0.1-0.1i" 0.1 (negate 0.1)
        checkComplex "-0.1-0.1i" (negate 0.1) (negate 0.1)

      it "should fail to parse invalid complex numbers" $ do
        let checkFailComplex a = parse number `shouldFailOn` a
        checkFailComplex "i"
        checkFailComplex "ai"
        checkFailComplex ".1i"
        --checkFailComplex "1*1i"
        --checkFailComplex "1/1i"
        --checkFailComplex "0.1e3i"

    describe "parsing defines" $ do
      it "should be able to parse a valid define expression (no procedure)" $ do
        let checkDefine a b c = parse define a `shouldParse` RavenDefine b c
        checkDefine "(def a 3)" "a" (RavenLiteral . RavenNumber . RavenIntegral $ 3)
        checkDefine "(def b \"test\")" "b" (RavenLiteral . RavenString $ "test")
        checkDefine "(def a true)" "a" (RavenLiteral . RavenBool $ True)

      -- TODO parse other form of define

      it "should fail to parse invalid define expression" $ do
        let checkFailDefine a = parse define `shouldFailOn` a
        checkFailDefine "def a 3"
        checkFailDefine "(ef a 3)"
        checkFailDefine "(def a)"
        checkFailDefine "(def 3)"
        checkFailDefine "(def true false)"

  -- TODO fix failing test cases (mostly due to lack of end of number indicator: " " or ")")
