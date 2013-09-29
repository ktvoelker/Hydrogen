
module H.Monad.RefGraph
  ( RefGraph()
  , emptyRefGraph
  , addRef
  , RefGraphT
  , runRefGraphT
  , addRefM
  , toGraph
  , toEdgeList
  , sccs
  , SCC(..)
  , flattenSCC
  ) where

import Data.Graph
import qualified Data.Map as M
import qualified Data.Set as S

import H.Common

newtype RefGraph a = RefGraph { unRefGraph :: Map a (Set a) } deriving (Eq, Ord, Show)

type EdgeList a = [(a, a, [a])]

emptyRefGraph :: (Ord a) => RefGraph a
emptyRefGraph = RefGraph M.empty

addRef :: (Ord a) => a -> a -> RefGraph a -> RefGraph a
addRef from to (RefGraph rg) =
  RefGraph
  . (mapLens from ^%= ((S.insert to <$>) . (<> Just S.empty)))
  $ rg

toGraph :: (Ord a) => RefGraph a -> Graph
toGraph = fst3 . graphFromEdges . toEdgeList

toEdgeList :: (Ord a) => RefGraph a -> EdgeList a
toEdgeList =
  map (\(k, xs) -> (k, k, S.toList xs))
  . M.toList
  . unRefGraph

sccs :: (Ord a) => RefGraph a -> [SCC a]
sccs = stronglyConnComp . toEdgeList

type RefGraphT k m = StateT (RefGraph k) m

runRefGraphT :: (Ord k, Monad m) => RefGraphT k m a -> m (a, [SCC k])
runRefGraphT = liftM (\(a, rg) -> (a, sccs rg)) . flip runStateT emptyRefGraph

addRefM :: (Ord k, Monad m) => k -> k -> RefGraphT k m ()
addRefM from to = modify $ addRef from to

