module Solutions.Day12
  where

import CustomPrelude

import Data.Char (isLower)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Text qualified as T
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P


-- * Part 1

solveP1 :: Text -> Int
solveP1 =
  runReader (count "start")
  . (, [])
  . graphFromPaths
  . partialParseText pathsP

-- | Return the number of paths from the given node to end. Assumes that there
-- aren't any adjacent large caves.
count :: Text -> Reader (Graph, HashSet Text) Int
count "end" = pure 1
count current = do
  (adjacent, closed) <- asks (first (HM.findWithDefault [] current))
  HS.foldl'
    (\acc node -> (+) <$> acc <*> local (updateClosed current) (count node))
    (pure 0)
    (HS.filter (not . flip HS.member closed) adjacent)
  where
    updateClosed :: Text -> (Graph, HashSet Text) -> (Graph, HashSet Text)
    updateClosed t = second (bool identity (HS.insert t) (isLower . T.head $ t))

type Graph = HashMap Text (HashSet Text)

graphFromPaths :: Seq (Text, Text) -> Graph
graphFromPaths = foldl' (flip build) mempty
  where
    build :: (Text, Text) -> Graph -> Graph
    build (t1, t2) =
        insertAdjust (HS.insert t1 ) t2 [t1]
        >>> insertAdjust (HS.insert t2) t1 [t2]
      where
        insertAdjust ::
          (Eq k, Hashable k) => (v -> v) -> k -> v -> HashMap k v -> HashMap k v
        insertAdjust f k v hm = HM.insertWith (\_ old -> f old) k v hm

type Parser = P.Parsec Void Text

pathsP :: Parser (Seq (Text, Text))
pathsP = fromList <$> P.sepEndBy1 pathP P.newline
  where
    pathP :: Parser (Text, Text)
    pathP = do
      a <- P.some P.letterChar
      _ <- P.char '-'
      b <- P.some P.letterChar
      pure $ bboth toText (a, b)

-- * Part 2

solveP2 :: Text -> Int
solveP2 = undefined
