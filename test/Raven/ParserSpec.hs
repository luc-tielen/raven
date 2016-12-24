
module Raven.ParserSpec ( spec ) where

import Test.Tasty.Hspec
import Text.Parsec hiding (parse)
import Raven.Types
import Raven.Parser (parse, bool)


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

