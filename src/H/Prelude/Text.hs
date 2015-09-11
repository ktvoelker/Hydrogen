
module H.Prelude.Text
  ( module Data.Char
  , module Data.Monoid
  , ByteString(), Text(), pack, unpack
  , encodeUtf8, decodeUtf8
  , Read(), Show()
  , read, show
  ) where

import Data.ByteString (ByteString)
import Data.Char
import Data.Monoid
import Data.Text (Text(), pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Prelude (Read(), Show())
import qualified Prelude as P

import H.Prelude.Core
import H.Prelude.Monad

read :: (Read a) => Text -> Maybe a
read = fmap fst . listToMaybe . P.reads . unpack

show :: (Show a) => a -> Text
show = pack . P.show

