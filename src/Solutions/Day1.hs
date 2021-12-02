module Solutions.Day1 (
  solveP1,
  solveP2,
  ) where

import CustomPrelude

import Data.Sequence qualified as Seq
import Relude.Extra (dup)


solveP1 :: Text -> Int
solveP1 = p1 . fmap unsafeReadInt . linesSeq

solveP2 :: Text -> Int
solveP2 = p1 . makeSums . fmap unsafeReadInt . linesSeq

p1 :: Seq Int -> Int
p1 =
  length
  . Seq.filter id
  . uncurry (Seq.zipWith (>))
  . first (Seq.drop 1)
  . dup

makeSums :: Seq Int -> Seq Int
makeSums = \case
  Empty -> Empty
  ls@(_ :<| xs)
    | length ls >= 3 -> sum (Seq.take 3 ls) :<| makeSums xs
    | otherwise      -> Empty
