module Main (main) where

import CustomPrelude

import System.Environment (getArgs)

import DayPart (Day (..), Part (..), parseDayPart)
import Solutions.Day1 qualified as D1


main :: IO ()
main = map toText <$> getArgs >>= \case
  [dayPart] -> either print (uncurry run) . parseDayPart $ dayPart
  _ -> showUsage
    where
      showUsage :: IO ()
      showUsage = do
        putTextLn $ "Usage: aoc2021-exe 'DAY-PART'"
        putTextLn $ "     Example: aoc2021 11-2"

-- | Runs the solver given the day and part.
run :: Day -> Part -> IO ()
run (Day d) part = do
  input <- readFileText $ "inputs/day" <> show d <> ".txt"
  case d of
    1 -> print $ chooseSolver D1.solveP1 D1.solveP2 part input
    _ -> putTextLn "Invalid day"
  where
    chooseSolver :: (Text -> a) -> (Text -> a) -> Part -> (Text -> a)
    chooseSolver solver1 solver2 p = case p of
      P1 -> solver1
      P2 -> solver2
