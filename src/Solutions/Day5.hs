module Solutions.Day5 where

import CustomPrelude

import Data.Sequence qualified as Seq
import Data.HashMap.Strict qualified as M
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex


-- * Part 1

solveP1 :: Text -> Int
solveP1 =
  M.foldl' (\acc x -> if x >= 2 then acc + 1 else acc) 0
  . foldl' (\acc l -> fromMaybe acc (addLine acc l)) M.empty
  . fromMaybe (error "solveP1: bad parse")
  . P.parseMaybe pInput

-- | Map from coordinate to number of vents.
type VentMap = M.HashMap (Int, Int) Int

-- | Add the vents described by a Line to a VentMap.
addLine :: VentMap -> Line -> Maybe VentMap
addLine vm = foldl' (flip addVent) vm <.> ventsInNonDiagonalLine
  where
    addVent :: (Int, Int) -> VentMap -> VentMap
    addVent v = M.insertWith (+) v 1 

-- | Only considers horizontal and vertical lines.
ventsInNonDiagonalLine :: Line -> Maybe (Seq (Int, Int))
ventsInNonDiagonalLine (Line (x1, y1) (x2, y2))
  -- Vertical line
  | x1 == x2 = Just $
    let ys = [min y1 y2 .. max y1 y2]
    in Seq.zip (Seq.replicate (length ys) x1) ys
  -- Horizontal line
  | y1 == y2 = Just $
    let xs = [min x1 x2 .. max x1 x2]
    in Seq.zip xs (Seq.replicate (length xs) y1)
  -- Diagonal line 
  | otherwise = Nothing


data Line = Line (Int, Int) (Int, Int)
  deriving (Eq, Show)

type Parser = P.Parsec Void Text

pInput :: Parser (Seq Line)
pInput = fromList <$> P.many (pLine >>= \l -> void P.newline <|> P.eof >> pure l)

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
