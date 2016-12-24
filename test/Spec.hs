
import Test.Tasty
import Test.Tasty.Hspec as Hspec
import qualified Raven.ParserSpec


main :: IO ()
main = do
    unitTests <- Hspec.testSpec "Unit tests" mainSpec
    defaultMain $ testGroup "Tests" [unitTests]


mainSpec :: Spec
mainSpec = do
    describe "ParserSpec" Raven.ParserSpec.spec
    -- describe "OtherSpec" OtherSpec.spec

