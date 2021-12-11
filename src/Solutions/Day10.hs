module Solutions.Day10 where

import CustomPrelude hiding (catMaybes)

import Data.Sequence qualified as Seq
import Data.Sequences (catMaybes)
import Data.Set qualified as S
import Data.Text qualified as T


-- * Part 1

solveP1 :: Text -> Int
solveP1 =
  getSum
  . foldMap' (Sum . score1)
  . catMaybes
  . fmap (flip evalState [] . findCorrupted)
  . parseInput

findCorrupted :: Seq Char -> State [Char] (Maybe Char)
findCorrupted = \case 
  Empty    -> pure Nothing
  c :<| cs -> get >>= \case
    -- TODO: remove repetition
    [] 
      | c `S.member` openers -> modify' (c :) >> findCorrupted cs
      | otherwise -> pure (Just c)
    lastOpener : rest
      | c `S.member` openers -> modify' (c :) >> findCorrupted cs
      | c == closerOf lastOpener -> put rest >> findCorrupted cs
      | otherwise -> pure (Just c)
  where
    openers :: Set Char
    openers = ['(', '[', '{', '<'] 

closerOf :: Char -> Char
closerOf = \case
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  '<' -> '>'
  x   -> error $ "closerOf: unexpected char: " <> [x]

parseInput :: Text -> Seq (Seq Char)
parseInput = fmap (T.foldl' (:|>) Empty) . fromList . T.lines

score1 :: Char -> Int
score1 = \case
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137
  _   -> error "score: unexpected char"

-- * Part 2

solveP2 :: Text -> Int
solveP2 =
  uncurry Seq.index
  . (&&&) Seq.sort (flip div 2 . Seq.length)
  . fmap (scoreCompletion . map closerOf . flip execState [] . findCorrupted)
  . parseInput

scoreCompletion :: [Char] -> Int
scoreCompletion = foldl' (curry (uncurry (+) . bimap (* 5) score2)) 0

score2 :: Char -> Int
score2 = \case
  ')' -> 1
  ']' -> 2
  '}' -> 3
  '>' -> 4
  _   -> error "score: unexpected char"
