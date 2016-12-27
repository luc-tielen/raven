
import Test.Tasty
import Test.Tasty.Hspec as Hspec
import qualified Raven.ParserSpec
import qualified Raven.NumberSpec


main :: IO ()
main = do
    unitTests <- Hspec.testSpec "Unit tests" mainSpec
    defaultMain $ testGroup "Tests" [unitTests]


mainSpec :: Spec
mainSpec = do
    describe "NumberSpec" Raven.NumberSpec.spec
    describe "ParserSpec" Raven.ParserSpec.spec

