{-# LANGUAGE TemplateHaskell #-}

module Solutions.Day11 (
  solveP1,
  solveP2,
) where

import CustomPrelude

import Data.Char (digitToInt)
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.Sequences qualified as Seqs
import Data.Vector.Unboxed qualified as V
import Lens.Micro.Platform hiding (ix)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex


-- * Part 1

data Grid = Grid
  { _gDigits :: !(V.Vector Int)
  , _gWidth :: !Int
  }
  deriving (Eq, Show)
$(makeLenses ''Grid)

solveP1 :: Text -> Int
solveP1 = countFlashes 100 . partialParseText gridP

countFlashes :: Int -> Grid -> Int
countFlashes 0 _ = 0
countFlashes iterations g =
  let (newG, flashes) = step g
  in flashes + countFlashes (iterations - 1) newG

step :: Grid -> (Grid, Int)
step grid =
  let digits = view gDigits grid
      allIxes = IS.fromAscList [0 .. V.length digits - 1]
      addedEnergy = IM.fromSet (const 1) allIxes
      (didFlash, additions) =
        runState
          (react grid (IS.foldl' (:|>) [] allIxes) IS.empty)
          addedEnergy
      newEnergies = map (\(k, v) -> (k, (grid @! k) + v)) (IM.toAscList additions)
      updates = newEnergies ++ (zip (IS.toAscList didFlash) (repeat 0))
  in (over gDigits (V.// updates) grid, IS.size didFlash)

  where
    react :: Grid -> Seq Int -> IntSet -> State (IntMap Int) IntSet
    react _ [] _ = pure IS.empty
    react g toCheck hasFlashed = do
      didFlash <-
        foldlM
          (\acc n -> checkDumbo g n hasFlashed <&> \b -> bool acc (n :<| acc) b)
          []
          toCheck
      let adjacentToFlash =
            foldl'
              (\acc n -> IS.union acc (neighborIxsSet n g))
              IS.empty
              didFlash
      rest <- react g (isToSeq adjacentToFlash) (hasFlashed `IS.union` seqToIs didFlash)
      pure $ seqToIs didFlash `IS.union` rest

    checkDumbo :: Grid -> Int -> IntSet -> State (IntMap Int) Bool
    checkDumbo g ix hasFlashed =
      if ix `IS.member` hasFlashed
        then pure False
        else do
          energyFromNeighbors <- gets (IM.! ix)
          if (grid @! ix) + energyFromNeighbors > 9
            then do
              let neighs = fst <$> neighbors ix g
              forM_ neighs (\neighIx -> modify' (IM.insertWith (+) neighIx 1))
              pure True
            else pure False

    isToSeq :: IntSet -> Seq Int
    isToSeq = fromList . IS.toAscList

    seqToIs :: Seq Int -> IntSet
    seqToIs = foldl' (flip IS.insert) IS.empty

-- Grid DSL

data Direction = N | S | W | E | NW | NE | SW | SE
  deriving (Eq, Show, Enum, Bounded)

-- | Partial lookup
(@!) :: Grid -> Int -> Int
g @! n = view gDigits g V.! n

neighborIxsSet :: Int -> Grid -> IntSet
neighborIxsSet ix grid =
  foldl' (\acc x -> IS.insert (fst x) acc) IS.empty (neighbors ix grid)

-- | Get the index and levels of neighboring octopi.
neighbors :: Int -> Grid -> Seq (Int, Int)
neighbors ix grid =
  Seqs.catMaybes $ fmap @Seq (flip (neighborAt ix) grid) [minBound .. maxBound]

-- | Get the neighbor of an index in a direction.
neighborAt :: Int -> Direction -> Grid -> Maybe (Int, Int)
neighborAt ix dir grid =
  (&&&) identity (grid @!) <$> guarded (not . (`isOob` dir)) (toNeighbor dir ix)
  where
    isOob :: Int -> Direction -> Bool
    isOob n d = isOobVertical || isOobHorizontal
      where
        isOobVertical :: Bool
        isOobVertical = n < 0 || n > (V.length (view gDigits grid) - 1)

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
    allDigitsP :: Parser (V.Vector Int)
    allDigitsP = V.fromList <$> P.someTill multiRowDigitP P.eof

    multiRowDigitP :: Parser Int
    multiRowDigitP = digitToInt <$> Lex.lexeme (P.skipMany P.newline) P.digitChar

-- * Part 2

solveP2 :: Text -> Int
solveP2 = uncurry (syncedAt 1) . (&&&) identity gridSize . partialParseText gridP

gridSize :: Grid -> Int
gridSize = V.length . view gDigits

syncedAt :: Int -> Grid -> Int -> Int
syncedAt stepNo g gSize =
  let (newG, flashes) = step g
  in if flashes == gSize
      then stepNo
      else syncedAt (stepNo + 1) newG gSize
