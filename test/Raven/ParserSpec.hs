
module Raven.ParserSpec ( spec ) where

import Test.Tasty.Hspec
import Test.QuickCheck
import Text.Parsec hiding (parse, string)
import Raven.Types
import Raven.Parser (parse, bool, string)


spec :: Spec
spec = describe "Parser" $ do
    describe "parsing bools" $ do
      it "should be able to parse true and false" $ do
        parse bool "true" `shouldBe` Right (RBool True)
        parse bool "false" `shouldBe` Right (RBool False)

      it "should fail to parse invalid input" $ do
        parse bool "truf" `shouldNotBe` Right (RBool True)
        parse bool "fals" `shouldNotBe` Right (RBool False)
        parse bool "" `shouldNotBe` Right (RBool True)
        parse bool "" `shouldNotBe` Right (RBool False)


    describe "parsing strings" $ do
      it "should be able to parse valid strings" $ do
        parse string "\"\"" `shouldBe` Right (RString "")
        parse string "\"a\"" `shouldBe` Right (RString "a")
        parse string "\"ab\"" `shouldBe` Right (RString "ab")
        parse string "\"a b\"" `shouldBe` Right (RString "a b")
        parse string "\"a  b\"" `shouldBe` Right (RString "a  b")

      it "should be able to parse escape characters" $ do
        parse string "\"a\rb\"" `shouldBe` Right (RString "a\rb")
        parse string "\"a\nb\"" `shouldBe` Right (RString "a\nb")
        parse string "\"a\tb\"" `shouldBe` Right (RString "a\tb")
        parse string "\"a\bb\"" `shouldBe` Right (RString "a\bb")
        parse string "\"a\vb\"" `shouldBe` Right (RString "a\vb")
        parse string "\"a\\b\"" `shouldBe` Right (RString "a\\b")
        parse string "\"a\'b\"" `shouldBe` Right (RString "a\'b")
 
      it "should fail to parse invalid input" $ do
        parse string "" `shouldNotBe` Right (RString "")
        parse string "\"" `shouldNotBe` Right (RString "")
        parse string "\"a" `shouldNotBe` Right (RString "a")
        parse string "a\"" `shouldNotBe` Right (RString "a")

        -- TODO quickcheck properties? -> custom generator?
