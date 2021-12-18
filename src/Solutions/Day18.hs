{-# OPTIONS_GHC -Wno-missing-methods #-}

module Solutions.Day18 where

import CustomPrelude

import Data.Char (digitToInt)
import GHC.Show qualified (show)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P


-- * Part 1

solveP1 :: Text -> Int
solveP1 = undefined

-- | Find the first explosion candidate in the SN, if it exists, and return its
-- regular values and the path to locate it.
findExplode :: SN -> Maybe ((Int, Int), Path)
findExplode = go 0 []
go :: Int -> Path -> SN -> Maybe ((Int, Int), Path)
go depth path = \case
  Pair (Reg a) (Reg b)
    | depth >= 4 -> Just ((a, b), path)
    | otherwise -> Nothing
  Pair l r -> go (depth + 1) (path :|> L) l <|> go (depth + 1) (path :|> R) r
  Reg _ -> Nothing  -- Shouldn't occur

-- | Return the path to the closest regular number to the left or right of the
-- given path.
-- updateClosest :: (SN -> SN) -> Choice -> Path -> SN -> Maybe SN
-- updateClosest f c path sn = case path of
--   Empty -> update f [c] sn
--   ps :|> _ -> update f (ps :|> c) sn
--   _ -> undefined

closestReg :: Choice -> Path -> SN -> Maybe Path
closestReg c path sn = undefined

-- Functions on SN

lookup :: Path -> SN -> Maybe SN
lookup p sn = case (p, sn) of
  (L :<| ps, Pair a _) -> lookup ps a
  (R :<| ps, Pair _ b) -> lookup ps b
  (_ :<| _, Reg _) -> Nothing
  (Empty, x) -> Just x

update :: (SN -> SN) -> Path -> SN -> Maybe SN
update f p sn = case (p, sn) of
  (L :<| ps, Pair a b) -> flip Pair b <$> update f ps a
  (R :<| ps, Pair a b) -> Pair a <$> update f ps b
  (_ :<| _, Reg _) -> Nothing
  (Empty, x) -> Just (f x)

-- Snailfish numbers

-- | Snailfish Numbers are represented as binary trees
data SN = Reg Int | Pair SN SN
  deriving Eq

instance Show SN where
  -- show = \case
  --   Reg n -> show n
  show (Reg n) = GHC.Show.show n
  show (Pair a b) = "[" <> GHC.Show.show a <> "," <> GHC.Show.show b <> "]"

instance Semigroup SN where
  (<>) = Pair

-- Main reason for defining this partial Num instance is to enable the use of
-- numeric literals via fromInteger.
instance Num SN where
  fromInteger = Reg . fromIntegral
  (+) = Pair

data Choice = L | R
  deriving (Eq, Show)

type Path = Seq Choice

-- Parsing

type Parser = P.Parsec Void Text

snsP :: Parser (Seq SN)
snsP = fromList <$> P.sepEndBy1 snP P.newline

snP :: Parser SN
snP = do
  _ <- P.char '['
  a <- digitP <|> snP
  _ <- P.char ','
  b <- digitP <|> snP
  _ <- P.char ']'
  pure (a <> b)
  where
    digitP :: Parser SN
    digitP = fromIntegral . digitToInt <$> P.digitChar

-- * Part 2

solveP2 :: Text -> Int
solveP2 = undefined
