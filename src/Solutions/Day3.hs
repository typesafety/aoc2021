{-# LANGUAGE ScopedTypeVariables #-}

module Solutions.Day3 where

import CustomPrelude

import Data.Bits qualified as B
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P


-- * Part 1

solveP1 :: Text -> Int
solveP1 input =
  let gammaBin = fmap @Seq (`mostCommonAt` linesSeq input) [0 .. numLen - 1]
      epsilonBin = binFlip gammaBin
  in binToDec gammaBin * binToDec epsilonBin
  where
    numLen :: Int
    numLen = T.length $ T.takeWhile (/= '\n') input

binToDec :: Seq Int -> Int
binToDec = Seq.foldlWithIndex (\acc idx n -> acc + n * (2 ^ idx)) 0 . Seq.reverse

binFlip :: Seq Int -> Seq Int
binFlip = fmap (`xor` 0b1)

mostCommonAt :: Int -> Seq Text -> Int
mostCommonAt idx texts =
  if foldl' (\acc t -> acc + f (T.index t idx)) 0 texts > 0
    then 1
    else 0
  where
    f :: Char -> Int
    f '1' = 1
    f '0' = (-1)
    f _ = error "f"

-- * Part 2

solveP2 :: Text -> Int
solveP2 input = o2rating parsed * co2rating parsed
  where
    parsed :: Seq Int
    parsed = partialParse input

    size :: Int
    size = subtract 1 . T.length . T.takeWhile (/= '\n') $ input

    o2rating :: Seq Int -> Int
    o2rating = filterReport O2 size

    co2rating :: Seq Int -> Int
    co2rating = filterReport CO2 size

data Rating = O2 | CO2
  deriving (Eq, Show)

ratingBias :: Rating -> Bool
ratingBias = \case
  O2  -> True
  CO2 -> False

filterReport :: Rating -> Int -> Seq Int -> Int
filterReport rating idx = \case
  Empty       -> error "Found no rating value"
  n :<| Empty -> n
  binNums     -> filterReport rating (idx - 1) $ Seq.filter criteria binNums
    where
      criteria :: Int -> Bool
      criteria
        | score == 0 = case rating of
          O2  -> isSet
          CO2 -> not . isSet
        | otherwise = case rating of
          O2 -> (== mostCommon) . isSet
          CO2 -> not . (== mostCommon) . isSet

      isSet :: Int -> Bool
      isSet x = (x `B.testBit` idx)

      bitVal :: Int -> Int
      bitVal x = if B.testBit x idx then 1 else 0

      score :: Int
      score = foldl' (\acc b -> acc + comp b) 0 (fmap isSet binNums)

      comp :: Bool -> Int
      comp x = if x then 1 else (-1)

      -- mostCommon :: Int
      mostCommon = score > 0 -- if score > 0 then 1 else 0

type Parser = P.Parsec Void Text

partialParse :: Text -> Seq Int
partialParse = fromMaybe (error "Day3: failed parse") .  P.parseMaybe pBits
  where
    pBit :: Parser Int
    pBit = do
      bin <- P.binary
      void (P.newline) <|> P.eof
      pure bin

    pBits :: Parser (Seq Int)
    pBits = fromList <$> many pBit
