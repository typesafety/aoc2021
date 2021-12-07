module Solutions.Day07 where

import CustomPrelude

import Control.Arrow ((***))
import Data.Sequence qualified as Seq
import Data.Text qualified as T


-- * Part 1

solveP1 :: Text -> Int
solveP1 input =
  let sorted = Seq.sort . unsafeParseInput $ input
  in uncurry max
      . (***) (fuelCost sorted) (fuelCost sorted)
      . (&&&) (`div` 2) ((`div` 2) . subtract 1)
      . Seq.length
      $ sorted
  where
    fuelCost :: Seq Int -> Int -> Int
    fuelCost xs ix = sum . fmap (abs . subtract (Seq.index xs ix)) $ xs

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
