module Solutions.Day5 where

import CustomPrelude

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex


-- * Part 1

solveP1 :: Text -> Int
solveP1 = undefined

data Line = Line (Int, Int) (Int, Int)
  deriving (Eq, Show)

type Parser = P.Parsec Void Text

pInput :: Parser (Seq Line)
pInput = fromList <$> P.many (pLine >>= \l -> P.newline >> pure l)

pLine :: Parser Line
pLine = do
  start <- pCoord
  _ <- P.space *> P.string "->" *> P.space
  end <- pCoord
  pure $ Line start end

pCoord :: Parser (Int, Int)
pCoord = do
  x <- Lex.decimal
  _ <- P.char ',' 
  y <- Lex.decimal
  pure (x, y)
 
-- * Part 2

solveP2 :: Text -> Int
solveP2 = undefined



