module Vcpl.Parse.LexSpec
  ( spec
  ) where

import Data.Foldable (for_)
import Data.Semigroup ((<>))
import Test.Hspec (Spec, it, shouldBe)

import Vcpl.Parse.Lex (Token, runAlex, scanTokens)

spec :: Spec
spec =
  for_ testCases $ \(name, sourceFile, tokensFile) ->
    it ("passes '" <> name <> "'") $ do
      sourceText <- readFile sourceFile
      tokensList <- read @(Either String [Token]) <$> readFile tokensFile
      runAlex sourceText scanTokens `shouldBe` tokensList

testCases :: [(String, FilePath, FilePath)]
testCases =
  let testCase a = (a, "testdata/lex/" <> a <> ".vcpl"
                     , "testdata/lex/" <> a <> ".tokens") in
  [ testCase "empty"
  , testCase "identity" ]
