module Main (main) where

import CustomPrelude

import Data.Text qualified as T
import Text.Printf (printf)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)

import DayPart (Day (..), Part (..), parseDayPart)
import Solutions.Day01 qualified as D1
import Solutions.Day02 qualified as D2
import Solutions.Day03 qualified as D3
import Solutions.Day05 qualified as D5
import Solutions.Day06 qualified as D6
import Solutions.Day07 qualified as D7
import Solutions.Day09 qualified as D9
import Solutions.Day10 qualified as D10


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
  putTextLn $ ":: Day " <> show d <> ", " <> show part <> " ::"
  putText "Result: "

  startTime <- getCPUTime
  case d of
    1 -> print $ chooseSolver D1.solveP1 D1.solveP2 part input
    2 -> print $ chooseSolver D2.solveP1 D2.solveP2 part input
    3 -> print $ chooseSolver D3.solveP1 D3.solveP2 part input
    5 -> print $ chooseSolver D5.solveP1 D5.solveP2 part input
    6 -> print $ chooseSolver D6.solveP1 D6.solveP2 part input
    7 -> print $ chooseSolver D7.solveP1 D7.solveP2 part input
    9 -> print $ chooseSolver D9.solveP1 D9.solveP2 part input
    10 -> print $ chooseSolver D10.solveP1 D10.solveP2 part input
    _ -> putTextLn "Invalid day"
  endTime <- getCPUTime

  let diff = endTime - startTime
  let elapsedSeconds :: Double = fromIntegral diff / 10 ** 12
  putTextLn . T.pack $ printf "Time:   %0.6f seconds" elapsedSeconds
  where
    chooseSolver :: (Text -> a) -> (Text -> a) -> Part -> (Text -> a)
    chooseSolver solver1 solver2 p = case p of
      P1 -> solver1
      P2 -> solver2
