
module H.Prelude.Core
  ( module Data.Bool
  , module Data.Maybe
  , module Data.Either
  , Eq(..), Ord(..), Ordering(..), Down(..), comparing
  , Enum(..), Bounded(..)
  , Category(..), (<<<), (>>>), const, flip, ($), on
  , fst, snd, fst3, snd3, thd3, curry, uncurry, swap, onFst, onSnd
  , onLeft, onRight, boolToMaybe
  , asTypeOf, Proxy(..), asProxied
  , todo, undefined, error, impossible
  ) where

import Control.Category
  ( Category(..), (<<<), (>>>) )
import Data.Bool
import Data.Either
import Data.Function (const, flip, ($), on)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Ord
import Data.Text (Text(), unpack)
import Data.Tuple (uncurry, curry, swap, fst, snd)
import Prelude
  ( Eq(..), Enum(..), Bounded(..), undefined, asTypeOf )
import qualified Prelude as P

onFst :: (a -> c) -> (a, b) -> (c, b)
onFst f (a, b) = (f a, b)

onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (a, b) = (a, f b)

data Proxy a = Proxy

asProxied :: a -> Proxy a -> a
asProxied = const

infix 8 `asProxied`

-- | Get the first element of a triple
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- | Get the second element of a triple
snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

-- | Get the third element of a triple
thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

onLeft :: (a -> c) -> Either a b -> Either c b
onLeft f = \case
  Left x  -> Left $ f x
  Right y -> Right y

onRight :: (b -> c) -> Either a b -> Either a c
onRight f = \case
  Left x  -> Left x
  Right y -> Right $ f y

boolToMaybe :: Bool -> Maybe ()
boolToMaybe False = Nothing
boolToMaybe True = Just ()

todo :: a
todo = error "Not implemented"

error :: Text -> a
error = P.error . unpack

impossible :: Text -> a
impossible = error . ("Impossible: " <>)

