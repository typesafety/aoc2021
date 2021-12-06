module CustomPrelude (
  -- * Re-exported
  module Relude,
  module Relude.Extra.Tuple,
  module Lens.Micro.Platform,

  Seq (Empty, (:<|), (:|>)),

  -- * Custom
  bboth,
  linesSeq,
  readInt,
  readMaybeInt,
  unsafeReadInt,
  partialParseText,
  (<.>),
  ) where

import Relude
import Relude.Extra.Tuple

import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Internal.Private (span_)
import Data.Text.Read qualified as T
import Data.Text.Unsafe (unsafeTail)
import Lens.Micro.Platform
import Text.Megaparsec qualified as P


-- * Parsing

-- | Attempt to read a Text value as an integer.
readInt :: Text -> Either Text Int
readInt = T.decimal >>> bimap toText fst

readMaybeInt :: Text -> Maybe Int
readMaybeInt = rightToMaybe . readInt

unsafeReadInt :: Text -> Int
unsafeReadInt = either error identity . readInt

partialParseText :: P.Parsec Void Text b -> Text -> b
partialParseText p = fromMaybe (error "partialParseText: failed parse") . P.parseMaybe p

-- * Tuple-related helpers

bboth :: Bifunctor f => (a -> b) -> f a a -> f b b
bboth f = bimap f f
{-# INLINE bboth #-}

-- * Sequence-related helpers

linesSeq :: Text -> Seq Text
linesSeq ps
  | T.null ps = Seq.Empty
  | otherwise = h Seq.:<| rest
  where
    rest :: Seq Text
    rest = if T.null t then Seq.Empty else linesSeq (unsafeTail t)

    (# h, t #) = span_ (/= '\n') ps
{-# INLINE linesSeq #-}

-- * Functor

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(<.>) f g x = f <$> g x
{-# INLINE (<.>) #-}
