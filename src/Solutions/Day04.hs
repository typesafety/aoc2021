module Solutions.Day04
  where

import CustomPrelude

import Data.Sequence qualified as Seq
import Data.Vector.Unboxed qualified as VU
import Data.Vector qualified as V
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex
import Text.Megaparsec.Debug


-- * Part 1

solveP1 :: Text -> Int
solveP1 = undefined

type Board = V.Vector (VU.Vector Int)

type Parser = P.Parsec Void Text

partialParseInput :: Text -> (Seq Int, Seq Board)
partialParseInput = fromMaybe (error "Day4: bad parse") . P.parseMaybe pInput

pInput :: Parser (Seq Int, Seq Board)
pInput = do
  ns <- pNumbers
  void . P.some $ P.newline
  bs <- pBoards
  pure (ns, bs)

pNumbers :: Parser (Seq Int)
pNumbers = fromList <$> P.sepBy Lex.decimal (P.char ',')

pBoards :: Parser (Seq Board)
pBoards = dbg "pBoards" $ do
  bs <- P.sepBy pBoard (P.some P.newline)
  pure $ fromList bs

pBoard :: Parser Board
pBoard = dbg "pBoard" $ do
  b <- P.sepBy1 (P.try pRow) P.newline
  pure $ fromList b

pRow :: Parser (VU.Vector Int)
pRow = dbg "pRow" $ do
  void $ P.many P.spaceChar
  r <- P.sepBy1 Lex.decimal (P.some $ P.char ' ')
  pure $ fromList r

-- * Part 2

solveP2 :: Text -> Int
solveP2 = undefined
