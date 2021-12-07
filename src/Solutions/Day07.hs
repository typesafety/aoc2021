module Solutions.Day07 where

import CustomPrelude

import Control.Arrow ((***))
import Data.Sequence qualified as Seq
import Data.Text qualified as T


-- * Part 1

solveP1 :: Text -> Int
solveP1 input =
  let sorted = Seq.sort . parse $ input
  in uncurry max
      . (***) (fuelCost sorted) (fuelCost sorted)
      . (&&&) (`div` 2) ((`div` 2) . subtract 1)
      . Seq.length
      $ sorted
  where
    fuelCost :: Seq Int -> Int -> Int
    fuelCost xs ix = sum . fmap (abs . subtract (Seq.index xs ix)) $ xs

    parse :: Text -> Seq Int
    parse = fmap unsafeReadInt . fromList . T.splitOn ","


-- * Part 2

solveP2 :: Text -> Int
solveP2 = undefined
