module Solutions.Day04
  where

import CustomPrelude

import Data.IntSet qualified as IS
import Data.Sequence qualified as Seq
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex


-- * Part 1

solveP1 :: Text -> Int
solveP1 = undefined

boardLines :: Board -> Seq IntSet
boardLines = uncurry (Seq.><) . (&&&) rowSets colSets
  where
    rowSets :: Board -> Seq IntSet
    rowSets = V.foldl' (\sq v -> vecToIs v :<| sq) []

    colSets :: Board -> Seq IntSet
    colSets = foldlCols (\is n -> IS.insert n is) []

    foldlCols :: (acc -> Int -> acc) -> acc -> Board -> Seq acc
    foldlCols f start b =
      let width = VU.length (b V.! 0)
      in V.foldl' (\sq colIx -> foldlCol f start colIx b :<| sq) [] [0 .. width - 1]

    foldlCol :: (acc -> Int -> acc) -> acc -> Int -> Board -> acc
    foldlCol f start colIx = V.foldl' f start . V.map (\uv -> uv VU.! colIx)

    vecToIs :: VU.Vector Int -> IntSet
    vecToIs = VU.foldl' (\is n -> IS.insert n is) []

type Board = V.Vector (VU.Vector Int)
type Parser = P.Parsec Void Text

bingoP :: Parser (Seq Int, Seq Board)
bingoP = do
  ns <- numbersP
  _  <- P.some P.newline
  bs <- boardsP
  _  <- P.many P.spaceChar
  pure (ns, bs)
  where
    numbersP :: Parser (Seq Int)
    numbersP = fromList <$> P.sepBy Lex.decimal (P.char ',')

    boardsP :: Parser (Seq Board)
    boardsP = fromList <$> P.sepEndBy1 boardP P.newline

    boardP :: Parser Board
    boardP = fromList <$> P.sepEndBy1 rowP P.newline

    rowP :: Parser (VU.Vector Int)
    rowP = do
      void $ P.many (P.char ' ')
      r <- P.sepBy1 Lex.decimal (P.some $ P.char ' ')
      pure $ fromList r

-- * Part 2

solveP2 :: Text -> Int
solveP2 = undefined
