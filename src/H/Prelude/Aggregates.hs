
module H.Prelude.Aggregates
  ( module Data.Monoid
  , module Data.Foldable
  , module Data.Traversable
  , module Data.List
  , module Data.Map
  , module Data.Semigroup
  , module Data.Set
  , headView, unionWithM, setCatMaybes, setSequence
  ) where

import Data.Foldable
import Data.List (filter, iterate, map, takeWhile)
import Data.Map (Map())
import qualified Data.Map as M
import Data.Monoid
import Data.Semigroup (Semigroup(..))
import Data.Set (Set())
import qualified Data.Set as S
import Data.Traversable

import H.Prelude.Core
import H.Prelude.Monad

-- | Deconstruct a list into its head and tail
headView :: [a] -> Maybe (a, [a])
headView [] = Nothing
headView (x : xs) = Just (x, xs)

-- | Union two maps, with a monadic action for merging duplicates
unionWithM :: (Ord k, Monad m) => (a -> a -> m a) -> Map k a -> Map k a -> m (Map k a)
unionWithM f a b =
  liftA M.fromList
    . sequence
    . fmap (\(k, v) -> liftA (k,) v)
    . M.toList
    $ M.unionWith f' (M.map return a) (M.map return b)
  where
    f' mx my = mx >>= \x -> my >>= \y -> f x y

setCatMaybes :: (Ord a) => Set (Maybe a) -> Set a
setCatMaybes = S.minView >>> \case
  Nothing      -> S.empty
  Just (x, xs) -> maybe id S.insert x $ setCatMaybes xs

setSequence :: (Ord a, Applicative f) => Set (f a) -> f (Set a)
setSequence = S.minView >>> \case
  Nothing      -> pure S.empty
  Just (x, xs) -> S.insert <$> x <*> setSequence xs

