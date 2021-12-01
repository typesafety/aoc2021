module CustomPrelude (
  -- * Re-exported
  module Relude,

  Seq (Empty, (:<|), (:|>)),

  -- * Custom
  readInt,
  unsafeReadInt,
  linesSeq,
  ) where

import Relude

import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Internal.Private (span_)
import Data.Text.Read qualified as T
import Data.Text.Unsafe (unsafeTail)


-- | Attempt to read a Text value as an integer.
readInt :: Text -> Either Text Int
readInt = T.decimal >>> bimap toText fst

unsafeReadInt :: Text -> Int
unsafeReadInt = either error identity . readInt

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
