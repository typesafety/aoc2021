module Solutions.Day09 where

import CustomPrelude

import Data.Char (digitToInt)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as UV
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P


-- * Part 1

solveP1 :: Text -> Int
solveP1 = riskSum . partialParseText pHeightMap

-- | Return the values of low points.
riskSum :: HeightGrid -> Int
riskSum hg = V.ifoldl' accRisk 0 hg
  where
    accRisk :: Int -> Int -> HeightRow -> Int
    accRisk acc y hr = acc + (uncurry (+) . (UV.sum &&& UV.length) $ rowLowVals y hr)

    rowLowVals :: Int -> HeightRow -> UV.Vector Int
    rowLowVals y hr = UV.mapMaybe valIfLow $ rowLows hr
      where
        -- Return the value at the given index if it is a low point (in both directions).
        valIfLow :: (Int, Int) -> Maybe Int
        valIfLow (x, v) = if isLow hg x then Just v else Nothing
          where
            isLow :: HeightGrid -> Int -> Bool
            isLow g i =
              let atI = (g V.! y) UV.! i
                  north = hAtCoord (i, y - 1) g
                  south = hAtCoord (i, y + 1) g
              in north > atI && atI < south

            -- Get the height at the given coordinate, defaulting to 9 if out of bounds.
            hAtCoord :: (Int, Int) -> HeightGrid -> Int
            hAtCoord (col, row) = fromMaybe 10 . ((UV.!? col) <=< (V.!? row))

-- | Return the indices and values in a row whose neighbors are higher.
rowLows :: HeightRow -> UV.Vector (Int, Int)
rowLows hr = UV.imapMaybe ixValIfLow hr
  where
    ixValIfLow :: Int -> Int -> Maybe (Int, Int)
    ixValIfLow ix height
      | isLow hr ix = Just (ix, height)
      | otherwise = Nothing
      where
        -- Return true if neighbor values are higher than at the given index.
        isLow :: HeightRow -> Int -> Bool
        isLow r i = let atI = r UV.! i in hAt r (i - 1) > atI && atI < hAt r (i + 1)

        -- Get height at the given index, defaulting to 9 if out of bounds.
        hAt :: HeightRow -> Int -> Int
        hAt r = fromMaybe 10 . (r UV.!?)

type HeightGrid = V.Vector HeightRow
type HeightRow = UV.Vector Int
type Parser = P.Parsec Void Text

pHeightMap :: Parser HeightGrid
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
