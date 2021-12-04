{-# OPTIONS -Wno-missing-signatures #-}

import CustomPrelude

import Data.Text qualified as T
import Test.Hspec (describe, hspec, it, shouldBe)

import Solutions.Day1 qualified as D1
import Solutions.Day2 qualified as D2
import Solutions.Day3 qualified as D3


main :: IO ()
main = hspec $ do
  test1_1
  test1_2

  test2_1
  test2_2

  test3_1
  test3_2

test1_1 = describe "1-1" $ it "sample input" $ D1.solveP1 input1 `shouldBe` 7
test1_2 = describe "1-2" $ it "sample input" $ D1.solveP2 input1 `shouldBe` 5
input1 = T.intercalate "\n"
  ["199", "200", "208", "210", "200", "207", "240", "269", "260", "263"]

test2_1 = describe "2-1" $ it "sample input" $ D2.solveP1 input2 `shouldBe` 150
test2_2 = describe "2-2" $ it "sample input" $ D2.solveP2 input2 `shouldBe` 900
input2 = T.intercalate "\n"
  [ "forward 5" , "down 5" , "forward 8" , "up 3" , "down 8" , "forward 2" ]


test3_1 = describe "3-1" $ it "sample input" $ D3.solveP1 input3 `shouldBe` 198
test3_2 = describe "3-2" $ it "sample input" $ D3.solveP2 input3 `shouldBe` undefined
input3 = T.intercalate "\n"
  [ "00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100"
  , "10000", "11001", "00010", "01010"
  ]
