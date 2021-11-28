module Main (main) where

import CustomPrelude

import Data.Text qualified as T
import System.Environment (getArgs)

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

data Part = P1 | P2
  deriving Show
newtype Day = Day Int
  deriving Show via Int

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

{- | Parse a string in the form "N-N" and return a tuple with the day and part.
>>> parseDayPart "11-2"
(11, 2)
-}
parseDayPart :: Text -> Either Text (Day, Part)
parseDayPart t = do
  [day, part] <- pure $ T.splitOn "-" t
  d <- Day <$> readInt day
  p <- parsePart =<< readInt part
  pure (d, p)
    where
      parsePart :: Int -> Either Text Part
      parsePart = \case
        1 -> Right P1
        2 -> Right P2
        n -> Left $ show n <> " is not a valid part (1 or 2)"
