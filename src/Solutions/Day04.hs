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

-- TODO: cleanup

solveP1 :: Text -> Int
solveP1 =
  uncurry score
  . uncurry (\nums bls -> evalState (run bls) nums)
  . second (fmap gridToSets)
  . partialParseText bingoP

-- Boards are represented as sequences of sets, where each sets represents
-- the remaining numbers for a row/column.
type Board = Seq IntSet

-- | Score a board.
score :: Int -> Board -> Int
score winningNum = (* winningNum) . IS.foldl' (+) 0 . foldl' IS.union IS.empty

-- | Simulate the bingo game.
run :: Seq Board -> State (Seq Int) (Int, Board)
run boards = get >>= \case
  n :<| ns ->
    let newBoards = fmap (draw n) boards
    in maybe (put ns >> run newBoards) pure ((n, ) <$> find (any IS.null) newBoards)
  Empty -> error "no bingo"

-- | Update a board with a number drawing.
draw :: Int -> Board -> Board
draw n bs = fmap (IS.delete n) bs

-- | Convert the grid representation into the sets representation.
gridToSets :: BoardGrid -> Board
gridToSets = uncurry (Seq.><) . (&&&) rowSets colSets
  where
    rowSets :: BoardGrid -> Seq IntSet
    rowSets = V.foldl' (\sq v -> vecToIs v :<| sq) []

    colSets :: BoardGrid -> Seq IntSet
    colSets = foldlCols (\is n -> IS.insert n is) []

    foldlCols :: (acc -> Int -> acc) -> acc -> BoardGrid -> Seq acc
    foldlCols f start b =
      let width = VU.length (b V.! 0)
      in V.foldl' (\sq colIx -> foldlCol f start colIx b :<| sq) [] [0 .. width - 1]

    foldlCol :: (acc -> Int -> acc) -> acc -> Int -> BoardGrid -> acc
    foldlCol f start colIx = V.foldl' f start . V.map (\uv -> uv VU.! colIx)

    vecToIs :: VU.Vector Int -> IntSet
    vecToIs = VU.foldl' (\is n -> IS.insert n is) []

-- Parse the input

type BoardGrid = V.Vector (VU.Vector Int)
type Parser = P.Parsec Void Text

bingoP :: Parser (Seq Int, Seq BoardGrid)
bingoP = do
  ns <- numbersP
  _  <- P.some P.newline
  bs <- boardsP
  _  <- P.many P.spaceChar
  pure (ns, bs)
  where
    numbersP :: Parser (Seq Int)
    numbersP = fromList <$> P.sepBy Lex.decimal (P.char ',')

    boardsP :: Parser (Seq BoardGrid)
    boardsP = fromList <$> P.sepEndBy1 boardP P.newline

    boardP :: Parser BoardGrid
    boardP = fromList <$> P.sepEndBy1 rowP P.newline

    rowP :: Parser (VU.Vector Int)
    rowP = do
      void $ P.many (P.char ' ')
      r <- P.sepBy1 Lex.decimal (P.some $ P.char ' ')
      pure $ fromList r

-- * Part 2

solveP2 :: Text -> Int
solveP2 =
  partialParseText bingoP
  >>> second (fmap gridToSets)
  >>> uncurry run2
  >>> uncurry score

-- | Simulate the bingo game, part 2.
run2 :: Seq Int -> Seq Board -> (Int, Board)
run2 Empty _ = error "ran out of numbers"
run2 (n :<| ns) boards = case boards of
  [lastBoard]
    | Seq.null nextBoards -> (n, draw n lastBoard)
    | otherwise -> run2 ns boards
  _ -> run2 ns nextBoards
  where
    nextBoards :: Seq Board
    nextBoards = Seq.filter (not . any IS.null) (fmap (draw n) boards)
