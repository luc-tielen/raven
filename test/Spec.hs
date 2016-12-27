
import Test.Tasty
import Test.Tasty.Hspec as Hspec
import qualified Raven.ParserSpec
import qualified Raven.TypesSpec


main :: IO ()
main = do
    unitTests <- Hspec.testSpec "Unit tests" mainSpec
    defaultMain $ testGroup "Tests" [unitTests]


mainSpec :: Spec
mainSpec = do
    describe "TypesSpec" Raven.TypesSpec.spec
    describe "ParserSpec" Raven.ParserSpec.spec

