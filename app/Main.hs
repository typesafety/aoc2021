module Main (main) where

import Relude

import Data.Text qualified as T
import Data.Text.Read qualified as T
import System.Environment (getArgs)

import Solutions.Day0 qualified as D0


main :: IO ()
main = (map T.pack <$> getArgs) >>= \case
  [_, dayPart] -> case parsePart dayPart of
    Just (d, p) -> run d p
    Nothing -> putTextLn "Parsing of date and part failed"
  _ -> showUsage
    where
      showUsage :: IO ()
      showUsage = do
        putTextLn $ "Usage: aoc2021 'DAY-PART'"
        putTextLn $ "     Example: aoc2021 11-2"

data Part = P1 | P2
  deriving Show
newtype Day = Day Int
  deriving Show via Int

-- | Runs the solver given the day and
run :: Day -> Part -> IO ()
run (Day d) part = do
  input <- readFileText $ "inputs/day" <> show d
  case d of
    0 -> putTextLn $ chooseSolver D0.solveP1 D0.solveP2 part input
    -- 1 -> putTextLn $ runSolver (chooseSolver D1.solveP1 D1.solveP2 p) input
    -- 2 -> putTextLn $ runSolver (chooseSolver D2.solveP1 D2.solveP2 p) input
    -- 3 -> putTextLn $ runSolver (chooseSolver D3.solveP1 D3.solveP2 p) input
    -- 4 -> putTextLn $ runSolver (chooseSolver D4.solveP1 D4.solveP2 p) input
    -- 5 -> putTextLn $ runSolver (chooseSolver D5.solveP1 D5.solveP2 p) input
    _ -> putTextLn "Invalid day"
  where
    chooseSolver :: (Text -> Text) -> (Text -> Text) -> Part -> (Text -> Text)
    chooseSolver solver1 solver2 p = case p of
      P1 -> solver1
      P2 -> solver2

{- | Parse a string in the form "N-N" and return a tuple with the day and part.
>>> parsePart "11-2"
(11, 2)
-}
parsePart :: Text -> Maybe (Day, Part)
parsePart t = do
  [day, part] <- pure $ T.splitOn "-" t
  dayN <- fst <$> rightToMaybe (T.decimal day)
  (partN :: Int) <- fst <$> rightToMaybe (T.decimal part)
  p <- case partN of
    1 -> Just P1
    2 -> Just P2
    _ -> Nothing
  pure (Day dayN, p)

