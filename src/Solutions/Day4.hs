module Solutions.Day4
  where

import CustomPrelude

import Data.Sequence qualified as Seq
import Data.Vector.Unboxed qualified as VU
import Data.Vector qualified as V
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex


-- * Part 1

solveP1 :: Text -> Int
solveP1 = undefined

type Board = V.Vector (VU.Vector Int)

type Parser = P.Parsec Void Text

unsafeParseInput :: Text -> (Seq Int, Seq Board)
unsafeParseInput = fromMaybe (error "Day4: bad parse") . P.parseMaybe pInput

pInput :: Parser (Seq Int, Seq Board)
pInput = do
  ns <- pNumbers
  void . P.many $ P.newline
  bs <- pBoards
  P.many $ void P.newline <|> P.eof
  pure (ns, bs)

pNumbers :: Parser (Seq Int)
pNumbers = do
  ns <- P.sepBy Lex.decimal (P.char ',')
  pure $ fromList ns

pBoards :: Parser (Seq Board)
pBoards = do
  bs <- P.sepBy pBoard (P.newline)
  pure $ fromList bs

pBoard :: Parser Board
pBoard = do
  b <- P.sepBy pRow (P.newline)
  pure $ fromList b

pRow :: Parser (VU.Vector Int)
pRow = do
  nums <- P.sepBy Lex.decimal P.space
  pure $ fromList nums

-- * Part 2

solveP2 :: Text -> Int
solveP2 = undefined
