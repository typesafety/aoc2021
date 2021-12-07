module Solutions.Day07 where

import CustomPrelude

import Data.Sequence qualified as Seq
import Data.Text qualified as T


-- * Part 1

solveP1 :: Text -> Int
solveP1 =
  T.splitOn ","
  >>> fromList
  >>> fmap unsafeReadInt
  >>> Seq.sort
  >>> (&&&) identity (identity &&& (Seq.length >>> flip div 2))
  >>> second (uncurry Seq.index >>> subtract >>> (>>> abs))
  >>> uncurry (<&>)
  >>> sum

unsafeParseInput :: Text -> Seq Int
unsafeParseInput = fmap unsafeReadInt . fromList . T.splitOn ","

-- * Part 2

solveP2 :: Text -> Int
solveP2 input = case (&&&) Seq.viewl Seq.viewr sorted of
  (l :< _, _ :> r) -> flip Seq.index 0 . Seq.sort . fmap (cost sorted) $ [l .. r]
  _ -> error "solveP2: bad list"
  where
    sorted :: Seq Int
    sorted = Seq.sort . unsafeParseInput $ input

    cost :: Seq Int -> Int -> Int
    cost xs n = foldl' (curry $ uncurry (+) . second (sumN . abs . subtract n)) 0 xs

    sumN :: Int -> Int
    sumN n = n * (n + 1) `div` 2
