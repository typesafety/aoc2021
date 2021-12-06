module Solutions.Day6 where

import CustomPrelude

import Data.Sequence qualified as Seq
import Data.IntMap.Strict qualified as IM
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex


-- * Part 1

solveP1 :: Text -> Int
solveP1 = Seq.length . applyN 80 step . partialParseText pInput

applyN :: Int -> (a -> a) -> a -> a
applyN n f x
  | n == 0 = x
  | otherwise = applyN (n - 1) f (f $! x)

step :: (Eq a, Num a) => Seq a -> Seq a
step = \case
  Empty -> Empty
  0 :<| xs -> 8 :<| 6 :<| step xs
  x :<| xs -> x - 1 :<| step xs

type Parser = P.Parsec Void Text

pInput :: Parser (Seq Int)
pInput = do
  nums <- P.sepBy Lex.decimal (P.char ',')
  pure . fromList $ nums

-- * Part 2

solveP2 :: Text -> Int
solveP2 input =
  let ns = partialParseText pInput input
  in Seq.length ns + foldl' (\acc n -> acc + evalState (breed n 256) IM.empty) 0 ns

type Memo = IM.IntMap Int

breed :: Int -> Int -> State Memo Int
breed startCount until = go startCount 0
  where
    go :: Int -> Int -> State Memo Int
    go count day
      | day == until = pure 0
      | count == 0   = get <&> IM.lookup day >>= maybe noMemo useMemo
      | otherwise = go (pred count) (succ day)
      where
        useMemo :: Int -> State Memo Int
        useMemo n = (1 + n +) <$> go 6 (succ day)

        noMemo :: State Memo Int
        noMemo = do
          res <- go 8 (succ day)
          modify' (IM.insert day res)
          rest <- go 6 (succ day)
          pure (1 + res + rest)
