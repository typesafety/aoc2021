{-# LANGUAGE TemplateHaskell #-}

module Solutions.Day11 where

import CustomPrelude

import Data.Char (digitToInt)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as UV
import Lens.Micro.Platform (makeLenses, view)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex


-- * Part 1

solveP1 :: Text -> Int
solveP1 = undefined

newtype Level = Level { unLevel :: Int }
  deriving (Eq, Show)

-- Grid DSL

data Grid = Grid
  { _gDigits :: !(UV.Vector Int)
  , _gWidth :: !Int
  }
  deriving (Eq, Show)
$(makeLenses ''Grid)

data Direction = N | S | W | E | NW | NE | SW | SE
  deriving (Eq, Show)

-- | Total lookup
(@?) :: Grid -> Int -> Maybe Level
g @? n = Level <$> view gDigits g UV.!? n

at :: Int -> Grid -> Maybe Level
at = flip (@?)

-- | Partial lookup
(@!) :: Grid -> Int -> Level
g @! n = Level $ view gDigits g UV.! n

-- | Get the index and levels of neighboring octopi.
neighbors :: Int -> Grid -> Seq (Int, Level)
neighbors ix grid = undefined

-- | Get the neighbor of an index in a direction.
neighborAt :: Int -> Direction -> Grid -> Maybe Level
neighborAt ix dir grid = (grid @!) <$> guarded (not . (`isOob` dir)) (toNeighbor dir ix)
  where
    isOob :: Int -> Direction -> Bool
    isOob n d = isOobVertical || isOobHorizontal
      where
        isOobVertical :: Bool
        isOobVertical = n < 0 || n > UV.length (view gDigits grid)

        isOobHorizontal :: Bool
        isOobHorizontal = case d of
          NW -> isOob (n + width) W
          NE -> isOob (n + width) E
          SW -> isOob (n - width) W
          SE -> isOob (n - width) E
          _ | elem @[] d [W, E] -> uncurry (/=) . bboth (`div` width) $ (n, ix)
            | otherwise -> False

    toNeighbor :: Direction -> Int -> Int
    toNeighbor = \case
      N -> subtract width
      S -> (+ width)
      W -> subtract 1
      E -> (+ 1)
      NE -> toNeighbor E . toNeighbor N
      NW -> toNeighbor W . toNeighbor N
      SE -> toNeighbor E . toNeighbor S
      SW -> toNeighbor W . toNeighbor S

    width :: Int
    width = view gWidth grid






type Parser = P.Parsec Void Text

gridP :: Parser Grid
gridP = do
  width <- length <$> P.lookAhead (P.someTill P.digitChar P.newline)
  digits <- allDigitsP
  pure (Grid digits width)
  where
    allDigitsP :: Parser (UV.Vector Int)
    allDigitsP = UV.fromList <$> P.someTill multiRowDigitP P.eof

    multiRowDigitP :: Parser Int
    multiRowDigitP = digitToInt <$> Lex.lexeme (P.skipMany P.newline) P.digitChar



-- * Part 2

solveP2 :: Text -> Int
solveP2 = undefined
