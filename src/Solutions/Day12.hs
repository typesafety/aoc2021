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
        HM.alter (maybe (Just [t2]) (Just . HS.insert t2)) t1
        >>> HM.alter (maybe (Just [t1]) (Just . HS.insert t1)) t2

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
solveP2 =
  runReader (count2 "start")
  . (, [])
  . graphFromPaths
  . partialParseText pathsP

count2 :: Text -> Reader (Graph, HashSet Text) Int
count2 "end" = pure 1
count2 current = do
  (adjacent, visited) <- asks (first (HM.findWithDefault [] current))
  let avail = HS.filter (not . flip HS.member visited) adjacent
  let closed = adjacent `HS.difference` avail
  x <- recurse (updateVisited current) count2 avail
  y <- recurse (updateVisited current) countPostRevisit (closed `HS.difference` ["start"])
  pure $ x + y
  where
    countPostRevisit :: Text -> Reader (Graph, HashSet Text) Int
    countPostRevisit "end" = pure 1
    countPostRevisit t = do
      (adjacent, visited) <- asks (first (HM.findWithDefault [] t))
      let avail = HS.filter (not . flip HS.member visited) adjacent
      recurse (updateVisited t) countPostRevisit avail

    recurse updater counter nodesToVisit = do
      HS.foldl'
        (\acc node -> (+) <$> acc <*> local updater (counter node))
        (pure 0)
        nodesToVisit

    updateVisited :: Text -> (Graph, HashSet Text) -> (Graph, HashSet Text)
    updateVisited t = second (bool identity (HS.insert t) (isLower . T.head $ t))
