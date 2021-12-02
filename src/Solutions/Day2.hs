{-# LANGUAGE TemplateHaskell #-}

module Solutions.Day2 where

import CustomPrelude hiding (Down)

import Data.Text qualified as T


data Pos = Pos
  { _posDepth :: Int
  , _posHoriz :: Int
  }
  deriving (Eq, Show)
$(makeLenses ''Pos)

startingPos :: Pos
startingPos = Pos 0 0

solveP1 :: Text -> Int
solveP1 =
  uncurry (*)
  . bimap (view posDepth) (view posHoriz)
  . dup
  . flip execState startingPos
  . p1
  . parseActions

solveP2 :: Text -> Int
solveP2 = undefined  -- TODO


p1 :: Seq Action -> State Pos ()
p1 = \case
  Empty -> pass
  a :<| as -> do
    case a of
      Forward n -> modifying posHoriz (+ n)
      Up n -> modify' (over posDepth (subtract n))
      Down n -> modify' (over posDepth (+ n))
    p1 as

data Action
  = Forward Int
  | Up Int
  | Down Int
  deriving (Eq, Show)

parseActions :: Text -> Seq Action
parseActions = fromMaybe (error "parseActions failure") . mapM parseAction . linesSeq

-- TODO: Make look nice
parseAction :: Text -> Maybe Action
parseAction txt = do
  [action, amt] <- pure $ T.splitOn " " txt
  f <- case action of
    "forward" -> Just Forward
    "down" -> Just Down
    "up" -> Just Up
    _ -> Nothing
  n <- readMaybeInt amt
  pure $ f n

  where
    list2pair :: [Text] -> Maybe (Text, Text)
    list2pair = \case
      [x, y] -> Just (x, y)
      _      -> Nothing


