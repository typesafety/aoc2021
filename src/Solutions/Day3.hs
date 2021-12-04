module Solutions.Day3 where

import CustomPrelude

import Data.Foldable (Foldable (foldl))
import Data.Sequence qualified as Seq
import Data.Text qualified as T


-- * Part 1

-- TODO: Better binary number representation
--       Maybe use vectors for better speed?
solveP1 :: Text -> Int
solveP1 input =
  let gammaBin = fmap @Seq (flip mostCommonAt (linesSeq input)) [0 .. numLen - 1]
      epsilonBin = binFlip gammaBin
  in binToDec gammaBin * binToDec epsilonBin
  where
    numLen :: Int
    numLen = T.length $ T.takeWhile (/= '\n') input

-- | Given a binary number represented as a sequence of ints, calculate the decimal value.
binToDec :: Seq Int -> Int
binToDec ns = Seq.foldrWithIndex (\idx n acc -> acc + n * (2 ^ (maxPos - idx))) 0 ns
  where
    maxPos :: Int
    maxPos = Seq.length ns - 1

binFlip :: Seq Int -> Seq Int
binFlip = fmap (\n -> (n + 1) `mod` 2)

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

