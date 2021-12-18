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

-- Define partial Num instance with fromInteger only, so that we can use numeric
-- literals. Other methods will throw exceptions.
instance Num SN where
  fromInteger = Reg . fromIntegral

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
