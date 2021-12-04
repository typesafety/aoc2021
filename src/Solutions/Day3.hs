module Solutions.Day3 where

import CustomPrelude

import Data.Foldable (Foldable (foldl))
import Data.Sequence qualified as Seq
import Data.Text qualified as T


-- * Part 1

solveP1 :: Text -> Int
solveP1 input =
  let gammaBin = fmap @Seq (`mostCommonAt` linesSeq input) [0 .. numLen - 1]
      epsilonBin = binFlip gammaBin
  in binToDec gammaBin * binToDec epsilonBin
  where
    numLen :: Int
    numLen = T.length $ T.takeWhile (/= '\n') input

binToDec :: Seq Int -> Int
binToDec = Seq.foldlWithIndex (\acc idx n -> acc + n * (2 ^ idx)) 0 . Seq.reverse

binFlip :: Seq Int -> Seq Int
binFlip = fmap (`xor` 0b1)

mostCommonAt :: Int -> Seq Text -> Int
mostCommonAt idx texts =
  if foldl (\acc t -> acc + f (T.index t idx)) 0 texts > 0
    then 1
    else 0
  where
    f :: Char -> Int
    f '1' = 1
    f '0' = (-1)
    f _ = error "f"

-- * Part 2

solveP2 :: Text -> Int
solveP2 = undefined

