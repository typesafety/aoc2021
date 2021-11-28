module CustomPrelude (
  module Relude,

  readInt,
  unsafeReadInt,
  ) where

import Relude

import Data.Text.Read qualified as T


-- | Attempt to read a Text value as an integer.
readInt :: Text -> Either Text Int
readInt = T.decimal >>> bimap toText fst

unsafeReadInt :: Text -> Int
unsafeReadInt = either error identity . readInt
