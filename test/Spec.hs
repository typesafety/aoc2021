{-# OPTIONS -Wno-missing-signatures #-}

import CustomPrelude

import Data.Text qualified as T
import Test.Hspec

import Solutions.Day1 qualified as D1


main :: IO ()
main = hspec $ do
  test1_1
  test1_2

input1 = T.intercalate "\n"
  ["199", "200", "208", "210", "200", "207", "240", "269", "260", "263"]
test1_1 = describe "1-1" $ it "sample input" $ D1.solveP1 input1 `shouldBe` 7
test1_2 = describe "1-2" $ it "sample input" $ D1.solveP2 input1 `shouldBe` 5
