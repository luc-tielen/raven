
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

    describe "parsing variables" $ do
      it "should be able to parse valid variables" $ do
        let checkVar a b = parse variable a `shouldParse` (RavenVariable b)
        checkVar "a" "a"
        checkVar "a123" "a123"

      it "should fail to parse invalid variables" $ do
        let checkFailVar a = parse variable `shouldFailOn` a
        checkFailVar "lambda"
        checkFailVar "true"
        checkFailVar "def"
        -- TODO check remaining keywords

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

      it "should be able to parse funcions defined with 'def'" $ do
        let makeFuncDef b c d = RavenDefine b (RavenFunction $ Function c d)
        let checkDefine a b c d = parse define a `shouldParse` (makeFuncDef b c d)
        let int = RavenLiteral . RavenNumber . RavenIntegral
        let var = RavenVariable
        let func a b c = RavenFunctionCall (var a) [var b, var c]
        checkDefine "(def (test-func) 1)" "test-func" [] [int 1]
        checkDefine "(def (test-func a) a)" "test-func" ["a"] [var "a"]
        checkDefine "(def (test-func a b) a)" "test-func" ["a", "b"] [var "a"]
        checkDefine "(def (test-func a b) (+ a b))" "test-func" ["a", "b"] [func "+" "a" "b"]

      it "should fail to parse invalid define expression" $ do
        let checkFailDefine a = parse define `shouldFailOn` a
        checkFailDefine "def a 3"
        checkFailDefine "(ef a 3)"
        checkFailDefine "(def a)"
        checkFailDefine "(def 3)"
        checkFailDefine "(def true false)"

        checkFailDefine "(def (test-func 1)"
        checkFailDefine "(def test-func) 1)"
        checkFailDefine "(def () 1)"

    describe "parsing function calls" $ do
      it "should be able to parse valid function calls" $ do
        let checkFunc a b c = parse functionCall a `shouldParse` (RavenFunctionCall b c)
        let op = RavenVariable  -- TODO this should return a procedure/function
        let int = RavenLiteral . RavenNumber . RavenIntegral
        checkFunc "(test-func)" (op "test-func") []
        checkFunc "(print \"test123\")" (op "print") [RavenLiteral . RavenString $ "test123"]
        checkFunc "(+ 1 2)" (op "+") (map int [1, 2])
        checkFunc "(- 1 2 3)" (op "-") (map int [1, 2, 3])

      it "should fail to parse invalid function call syntax" $ do
        let checkFailFunc a = parse functionCall `shouldFailOn` a
        checkFailFunc "()"
        checkFailFunc "(test-func"
        checkFailFunc "test-func)"

    describe "parsing lambdas" $ do
      it "should be able to parse valid lambdas" $ do
        let checkFunc a b c = parse lambda a `shouldParse` (RavenFunction $ Function b c)
        let var = RavenVariable
        let int = RavenLiteral . RavenNumber . RavenIntegral
        let func a b = RavenFunctionCall (RavenVariable a) b  -- TODO this should be a procedure
        checkFunc "(lambda () 1)" [] [int 1]
        checkFunc "(lambda (a) a)" ["a"] [var "a"]
        checkFunc "(lambda (a) (test-func) (test-func2))" ["a"] [ func "test-func" []
                                                                , func "test-func2" []
                                                                ]
        checkFunc "(lambda (a b) (+ a b))" ["a", "b"] [func "+" [var "a", var "b"]]

      it "should fail to parse invalid lambda syntax" $ do
        let checkFailFunc a = parse lambda `shouldFailOn` a
        checkFailFunc "(lambd () 1)"
        checkFailFunc "(lambda 1)"
        checkFailFunc "(lambda ( 1)"
        checkFailFunc "(lambda ())"

    describe "parsing and expressions" $ do
      it "should be able to parse valid and expressions" $ do
        let checkAnd a b = parse andExpr a `shouldParse` (RavenAnd $ And b)
        let boolean = RavenLiteral . RavenBool
        let int = RavenLiteral . RavenNumber . RavenIntegral
        let func a b = RavenFunctionCall (RavenVariable a) b  -- TODO this should be a procedure
        checkAnd "(and)" []
        checkAnd "(and true)" [boolean True]
        checkAnd "(and true true)" [boolean True, boolean True]
        checkAnd "(and true 1)" [boolean True, int 1]
        checkAnd "(and true (+ 1 2))" [boolean True, func "+" [int 1, int 2]]

      it "should fail to parse invalid and expressions" $ do
        let checkFailAnd a = parse andExpr `shouldFailOn` a
        checkFailAnd "(nd)"
        checkFailAnd "(and"
        checkFailAnd "and)"

    describe "parsing or expressions" $ do
      it "should be able to parse valid or expressions" $ do
        let checkAnd a b = parse orExpr a `shouldParse` (RavenOr $ Or b)
        let boolean = RavenLiteral . RavenBool
        let int = RavenLiteral . RavenNumber . RavenIntegral
        let func a b = RavenFunctionCall (RavenVariable a) b  -- TODO this should be a procedure
        checkAnd "(or)" []
        checkAnd "(or true)" [boolean True]
        checkAnd "(or true true)" [boolean True, boolean True]
        checkAnd "(or true 1)" [boolean True, int 1]
        checkAnd "(or true (+ 1 2))" [boolean True, func "+" [int 1, int 2]]

      it "should fail to parse invalid or expressions" $ do
        let checkFailAnd a = parse orExpr `shouldFailOn` a
        checkFailAnd "(r)"
        checkFailAnd "(or"
        checkFailAnd "or)"

    describe "parsing begin expressions" $ do
      it "should be able to parse valid begin expressions" $ do
        let checkBegin a b = parse begin a `shouldParse` (RavenBegin $ Begin b)
        let func a b = RavenFunctionCall (RavenVariable a) b
        let int = RavenLiteral . RavenNumber . RavenIntegral
        checkBegin "(begin (f1))" [func "f1" []]
        checkBegin "(begin (f1) (f2))" [func "f1" [], func "f2" []]
        checkBegin "(begin 1 (f1))" [int 1, func "f1" []]
        checkBegin "(begin (f1) 1)" [func "f1" [], int 1]

      it "should fail to parse invalid begin expressions" $ do
        let checkFailBegin a = parse begin `shouldFailOn` a
        checkFailBegin "(begi)"
        checkFailBegin "(begin)"
        checkFailBegin "(begin ())"

    describe "parsing assignments" $ do
      it "should be possible to parse valid assignment expressions" $ do
        let int = RavenLiteral . RavenNumber . RavenIntegral
        let str = RavenLiteral . RavenString
        let checkAssign a b c = parse assignment a `shouldParse` (RavenAssign $ Assign (b) c)
        checkAssign "(set! a 3)" "a" (int 3)
        checkAssign "(set! b \"123456789\")" "b" (str "123456789")

      it "should fail to parse invalid assignment expressions" $ do
        let checkFailAssign a = parse assignment `shouldFailOn` a
        checkFailAssign "(set!)"
        checkFailAssign "(set!x a 3)"
        checkFailAssign "(set! a)"
        checkFailAssign "(set! 3)"
        checkFailAssign "(set! 3 3)"

    describe "parsing if expressions" $ do
      it "should be possible to parse valid if expressions" $ do
        let checkIf a b = parse ifExpr a `shouldParse` b
        let int = RavenLiteral . RavenNumber . RavenIntegral
        let boolean = RavenLiteral . RavenBool
        let str = RavenLiteral . RavenString
        checkIf "(if true 1)" (RavenIf (If (boolean True) (int 1) Nothing))
        checkIf "(if true 1 2)" (RavenIf (If (boolean True) (int 1) (Just (int 2))))
        let func = RavenFunctionCall (RavenVariable "test-func") []
        checkIf "(if \"test\" (test-func))" (RavenIf (If (str "test") func Nothing))

      it "should fail to parse invalid if expressions" $ do
        let checkFailIf a = parse ifExpr `shouldFailOn` a
        checkFailIf "(if)"
        checkFailIf "(if () true)"
        checkFailIf "(if true)"
        checkFailIf "(iff true 1)"

    describe "parsing delay expressions" $ do
      it "should be able to parse valid delay expressions" $ do
        let checkDelay a b = parse delay a `shouldParse` b
        let int = RavenLiteral . RavenNumber . RavenIntegral
        let boolean = RavenLiteral . RavenBool
        checkDelay "(delay 1)" (RavenDelay (int 1))
        checkDelay "(delay true)" (RavenDelay (boolean True))

      it "should fail to parse invalid delay expressions" $ do
        let checkFailDelay a = parse delay `shouldFailOn` a
        checkFailDelay "(delay"
        checkFailDelay "delay)"
        checkFailDelay "(delay)"
        checkFailDelay "(delayy (+ 1 2))"

    describe "parsing cond expressions" $ do
      it "should be able to parse valid cond expressions" $ do
        let checkCond a b = parse cond a `shouldParse` b
        let condExpr a b = RavenCond $ Cond a b
        let boolean = RavenLiteral . RavenBool
        let int = RavenLiteral . RavenNumber . RavenIntegral
        let var = RavenVariable
        let func a b = RavenFunctionCall (var a) b
        let listFunc = func "list" [int 1, int 2]
        let lambdaFunc a = RavenFunctionCall (RavenFunction (Function ["x"] [var "x"])) a
        checkCond "(cond (true))" (condExpr [[boolean True]] [])
        checkCond "(cond (true 1))" (condExpr [[boolean True, int 1]] [])
        checkCond "(cond (else 1))" (condExpr [] [int 1])
        checkCond "(cond (false 1) (true 2))" (condExpr [[boolean False, int 1], [boolean True, int 2]] [])
        checkCond "(cond (false 1) (false 2) (else 3))" (condExpr [[boolean False, int 1], [boolean False, int 2]] [int 3])
        checkCond "(cond ((list 1 2) => car))" (condExpr [[listFunc, func "car" [listFunc]]] [])
        checkCond "(cond (1 => (lambda (x) x)))" (condExpr [[int 1, lambdaFunc [int 1]]] [])

      it "should fail to parse invalid cond expressions" $ do
        let checkFailCond a = parse cond `shouldFailOn` a
        checkFailCond "(cond)"
        checkFailCond "(cond ())"
        checkFailCond "(cnd (true))"
        checkFailCond "(cond (true)"
        checkFailCond "cond (true))"
        checkFailCond "cond (true))"
        checkFailCond "(cond ((list 1 2) =>)"
        checkFailCond "(cond (=> (list 1 2))"
        checkFailCond "(cond (else))"
        checkFailCond "(cond (else 1) (true))"

    describe "parsing case expressions" $ do
      it "should be able to parse valid case expressions" $ do
        let checkCase a b = parse caseExpr a `shouldParse` b
        let caseExpr' a b c = RavenCase $ Case a b c
        let boolean = RavenLiteral . RavenBool
        let int = RavenLiteral . RavenNumber . RavenIntegral
        checkCase "(case 1 ((1) 2))" (caseExpr' (int 1) [([int 1], [int 2])] [])
        checkCase "(case 1 ((1) 2 3))" (caseExpr' (int 1) [([int 1], [int 2, int 3])] [])
        checkCase "(case 1 ((1) 2) ((3) 4))" (caseExpr' (int 1) [([int 1], [int 2]), ([int 3], [int 4])] [])
        checkCase "(case 1 ((1) 2) ((3) 4) (else 5))" (caseExpr' (int 1) [([int 1], [int 2]), ([int 3], [int 4])] [int 5])
        checkCase "(case 2 ((1 2) 3))" (caseExpr' (int 2) [([int 1, int 2], [int 3])] [])
        checkCase "(case 1 (else 2))" (caseExpr' (int 1) [] [int 2])
        checkCase "(case 1 (else 2 3))" (caseExpr' (int 1) [] [int 2, int 3])

      it "should fail to parse invalid case expressions" $ do
        let checkFailCase a = parse caseExpr `shouldFailOn` a
        checkFailCase "(case)"
        checkFailCase "(case 1)"
        checkFailCase "(case 1 ())"
        checkFailCase "(case 1 ((1) ))"
        checkFailCase "(cas 1 ((1) 2))"
        checkFailCase "(case 1 ((1) 1)"
        checkFailCase "case 1 ((1) 1))"
        checkFailCase "(case (else 1))"
        checkFailCase "(case 1 (else))"

    describe "parsing do expressions" $ do
      it "should be able to parse valid do expressions" $ do
        let checkDo a b = parse doExpr a `shouldParse` b
        let doExpr' a b c = RavenDo $ Do a b c
        let boolean = RavenLiteral . RavenBool
        let int = RavenLiteral . RavenNumber . RavenIntegral
        let var = RavenVariable
        let func a b = RavenFunctionCall (var a) b
        checkDo "(do () (true))" (doExpr' [] (boolean True, []) [])
        checkDo "(do () (true 1))" (doExpr' [] (boolean True, [int 1]) [])
        checkDo "(do () (true 1 2))" (doExpr' [] (boolean True, [int 1, int 2]) [])
        checkDo "(do ((a 1)) (true))" (doExpr' [(var "a", int 1, var "a")] (boolean True, []) [])
        checkDo "(do ((a 1 (+ a 1))) (true))" (doExpr' [(var "a", int 1, func "+" [var "a", int 1])] (boolean True, []) [])
        checkDo "(do ((a 1) (b 2)) (true))" (doExpr' [(var "a", int 1, var "a"), (var "b", int 2, var "b")] (boolean True, []) [])
        checkDo "(do () (true 1))" (doExpr' [] (boolean True, [int 1]) [])
        checkDo "(do () (true 1 2))" (doExpr' [] (boolean True, [int 1, int 2]) [])
        checkDo "(do () (true) 1)" (doExpr' [] (boolean True, []) [int 1])
        checkDo "(do () (true) 1 2)" (doExpr' [] (boolean True, []) [int 1, int 2])

      it "should fail to parse invalid do expressions" $ do
        let checkFailDo a = parse doExpr `shouldFailOn` a
        checkFailDo "(do)"
        checkFailDo "(do ())"
        checkFailDo "(do () ())"
        checkFailDo "(d () (true))"
        checkFailDo "(do () (true ()))"
        checkFailDo "(do (a) (true))"
        checkFailDo "(do (true))"
        checkFailDo "(do () true)"




  -- TODO fix failing test cases (mostly due to lack of end of number indicator: " " or ")")
