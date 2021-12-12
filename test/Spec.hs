{-# OPTIONS -Wno-missing-signatures #-}

import CustomPrelude

import Data.Text qualified as T
import Test.Hspec (describe, hspec, it, shouldBe)

import Solutions.Day01 qualified as D1
import Solutions.Day02 qualified as D2
import Solutions.Day03 qualified as D3
import Solutions.Day04 qualified as D4
import Solutions.Day05 qualified as D5
import Solutions.Day06 qualified as D6
import Solutions.Day07 qualified as D7
import Solutions.Day09 qualified as D9
import Solutions.Day10 qualified as D10
import Solutions.Day11 qualified as D11


main :: IO ()
main = do
  -- Set up tests

  let input1 = T.intercalate "\n"
        ["199", "200", "208", "210", "200", "207", "240", "269", "260", "263"]
  let test1_1 = describe "1-1" $ it "sample input" $ D1.solveP1 input1 `shouldBe` 7
  let test1_2 = describe "1-2" $ it "sample input" $ D1.solveP2 input1 `shouldBe` 5

  let input2 = T.intercalate "\n"
        [ "forward 5" , "down 5" , "forward 8" , "up 3" , "down 8" , "forward 2" ]
  let test2_1 = describe "2-1" $ it "sample input" $ D2.solveP1 input2 `shouldBe` 150
  let test2_2 = describe "2-2" $ it "sample input" $ D2.solveP2 input2 `shouldBe` 900

  let input3 = T.intercalate "\n"
        [ "00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100"
        , "10000", "11001", "00010", "01010"
        ]
  let test3_1 = describe "3-1" $ it "sample input" $ D3.solveP1 input3 `shouldBe` 198
  let test3_2 = describe "3-2" $ it "sample input" $ D3.solveP2 input3 `shouldBe` 230

  input4 <- readFileText "test/day4sample.txt"
  let test4_1 = describe "4-1" $ it "sample input" $ D4.solveP1 input4 `shouldBe` 4512
  let test4_2 = describe "4-2" $ it "sample input" $ D4.solveP2 input4 `shouldBe` undefined

  let input5 = T.intercalate "\n"
        [ "0,9 -> 5,9", "8,0 -> 0,8", "9,4 -> 3,4", "2,2 -> 2,1", "7,0 -> 7,4"
        , "6,4 -> 2,0", "0,9 -> 2,9", "3,4 -> 1,4", "0,0 -> 8,8", "5,5 -> 8,2"
        ]
  let test5_1 = describe "5-1" $ it "sample input" $ D5.solveP1 input5 `shouldBe` 5
  let test5_2 = describe "5-2" $ it "sample input" $ D5.solveP2 input5 `shouldBe` 12

  let input6 = "3,4,3,1,2"
  let test6_1 = describe "6-1" $ it "sample input" $ D6.solveP1 input6 `shouldBe` 5934
  let test6_2 = describe "6-2" $ it "sample input" $ D6.solveP2 input6 `shouldBe` 26984457539

  let input7 = "16,1,2,0,4,2,7,1,2,14"
  let test7_1 = describe "7-1" $ it "sample input" $ D7.solveP1 input7 `shouldBe` 37
  let test7_2 = describe "7-2" $ it "sample input" $ D7.solveP2 input7 `shouldBe` 168

  let input9 = T.intercalate "\n"
        [ "2199943210"
        , "3987894921"
        , "9856789892"
        , "8767896789"
        , "9899965678"
        , ""  -- FIXME: Look up how to write parsers
        ]
  let test9_1 = describe "9-1" $ it "sample input" $ D9.solveP1 input9 `shouldBe` 15
  let test9_2 = describe "9-2" $ it "sample input" $ D9.solveP2 input9 `shouldBe` 1134

  let input10 = T.intercalate "\n"
        [ " [({(<(())[]>[[{[]{<()<>>"
        , " [(()[<>])]({[<{<<[]>>("
        , " {([(<{}[<>[]}>{[]{[(<()>"
        , " (((({<>}<{<{<>}{[]{[]{}"
        , " [[<[([]))<([[{}[[()]]]"
        , " [{[{({}]{}}([{[{{{}}([]"
        , " {<[[]]>}<{[{[{[]{()[[[]"
        , " [<(<(<(<{}))><([]([]()"
        , " <{([([[(<>()){}]>(<<{{"
        , " <{([{{}}[<[[[<>{}]]]>[]]"
        ]
  let test10_1 = describe "10-1" $ it "sample input" $ D10.solveP1 input10 `shouldBe` 26397
  let test10_2 = describe "10-2" $ it "sample input" $ D10.solveP2 input10 `shouldBe` 288957

  let input11 = T.intercalate "\n"
        [ "5483143223"
        , "2745854711"
        , "5264556173"
        , "6141336146"
        , "6357385478"
        , "4167524645"
        , "2176841721"
        , "6882881134"
        , "4846848554"
        , "5283751526"
        ]
  let test11_1 = describe "11-1" $ it "sample input" $ D11.solveP1 input11 `shouldBe` 1656
  let test11_2 = describe "11-2" $ it "sample input" $ D11.solveP2 input11 `shouldBe` undefined

  -- Run tests
  hspec $ do

    test1_1
    test1_2

    test2_1
    test2_2

    test3_1
    test3_2

    test4_1
    test4_2

    test5_1
    test5_2

    test6_1
    test6_2

    test7_1
    test7_2

    test9_1
    test9_2

    test10_1
    test10_2

    test11_1
    test11_2
