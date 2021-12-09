module Solutions.Day09 where

import CustomPrelude

import Data.Char (digitToInt)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as UV
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex


-- * Part 1

solveP1 :: Text -> Int
solveP1 = undefined

type HeightMap = V.Vector (UV.Vector Int)

type Parser = P.Parsec Void Text

pHeightMap :: Parser HeightMap
pHeightMap = do
  ps <- P.some pRow
  pure $ V.fromList ps

pRow :: Parser (UV.Vector Int)
pRow = do
  d <- P.someTill P.digitChar P.newline
  pure . UV.fromList . map digitToInt $ d


-- * Part 2

solveP2 :: Text -> Int
solveP2 = undefined
