module Solutions.Day03 (
  solveP1,
  solveP2,
  ) where

import CustomPrelude

import Data.Bits qualified as B
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex


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
solveP2 input = filterReport O2 numberSize parsed * filterReport CO2 numberSize parsed
  where
    parsed :: Seq Int
    parsed = partialParse input

    numberSize :: Int
    numberSize = subtract 1 . T.length . T.takeWhile (/= '\n') $ input

data Rating = O2 | CO2
  deriving (Eq, Show)

filterReport :: Rating -> Int -> Seq Int -> Int
filterReport rating idx = \case
  Empty       -> error "Found no rating value"
  n :<| Empty -> n
  binNums     -> filterReport rating (idx - 1) $ Seq.filter criteria binNums
    where
      criteria :: Int -> Bool
      criteria = case rating of
        O2  -> commonCheck . targetIxSet
        CO2 -> not . commonCheck . targetIxSet
        where
          commonCheck :: Bool -> Bool
          commonCheck = if score == 0 then identity else (== (score > 0))

      score :: Int
      score = foldl' (\acc b -> acc + bool (-1) 1 b) 0 (fmap targetIxSet binNums)

      targetIxSet :: Int -> Bool
      targetIxSet x = (x `B.testBit` idx)

type Parser = P.Parsec Void Text

partialParse :: Text -> Seq Int
partialParse = fromMaybe (error "Day3: failed parse") .  P.parseMaybe pBits
  where
    pBit :: Parser Int
    pBit = do
      bin <- Lex.binary
      void (P.newline) <|> P.eof
      pure bin

    pBits :: Parser (Seq Int)
    pBits = fromList <$> many pBit
