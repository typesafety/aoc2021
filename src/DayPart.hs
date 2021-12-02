module DayPart (
  Part (..),
  Day (..),

  parseDayPart,
  ) where

import CustomPrelude

import Data.Text qualified as T


data Part = P1 | P2
  deriving (Eq, Ord, Show)
newtype Day = Day Int
  deriving (Eq, Ord, Show)

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