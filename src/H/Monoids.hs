
module H.Monoids where

import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S

import H.Import

-- | Law: msize mempty == 0
-- | Law: msize a + msize b >= msize (a <> b) >= max (msize a) (msize b)
class (Monoid a) => MonotonicMonoid a where
  msize :: a -> Integer

instance (MonotonicMonoid a) => MonotonicMonoid (Maybe a) where
  msize Nothing = 0
  msize (Just m) = msize m

instance MonotonicMonoid [a] where
  msize = genericLength

instance (Ord a) => MonotonicMonoid (S.Set a) where
  msize = fromIntegral . S.size

instance (Ord k) => MonotonicMonoid (M.Map k a) where
  msize = fromIntegral . M.size

-- | Law: maybe (a == mempty) (\(e, a') -> a == msingleton e <> a') (mextract a)
-- | Law: maybe (msize a == 0) (\(_, a') -> msize a' + 1 == msize a) (mextract a)
class (MonotonicMonoid a) => ElementalMonoid a where
  type Unit a :: *
  msingleton :: Unit a -> a
  mextract :: a -> Maybe (Unit a, a)

instance (ElementalMonoid a) => ElementalMonoid (Maybe a) where
  type Unit (Maybe a) = Unit a
  msingleton = Just . msingleton
  mextract Nothing = Nothing
  mextract (Just m) = case mextract m of
    Nothing -> Nothing
    Just (x, m') -> Just (x, Just m')

instance ElementalMonoid [a] where
  type Unit [a] = a
  msingleton = (: [])
  mextract [] = Nothing
  mextract (x : xs) = Just (x, xs)

instance (Ord a) => ElementalMonoid (S.Set a) where
  type Unit (S.Set a) = a
  msingleton = S.singleton
  mextract = S.minView

instance (Ord k) => ElementalMonoid (M.Map k a) where
  type Unit (M.Map k a) = (k, a)
  msingleton = uncurry M.singleton
  mextract = M.minViewWithKey

mfold :: (ElementalMonoid a) => (Unit a -> b -> b) -> b -> a -> b
mfold f z xs = case mextract xs of
  Nothing -> z
  Just (x, xs') -> f x $ mfold f z xs'

mmap :: (ElementalMonoid a, ElementalMonoid b) => (Unit a -> Unit b) -> a -> b
mmap f = mfold ((<>) . msingleton . f) mempty

mconvert :: (ElementalMonoid a, ElementalMonoid b, Unit a ~ Unit b) => a -> b
mconvert = mmap id

mmconcat :: (ElementalMonoid a, ElementalMonoid b, Unit a ~ b) => a -> b
mmconcat = mfold (<>) mempty

-- | Law: msize a + msize b == msize (a <> b)
class (ElementalMonoid a) => PreservingMonoid a where

instance PreservingMonoid [a] where

-- | Law: melements (mlist xs) == xs
class (PreservingMonoid a) => StableMonoid a where

instance StableMonoid [a] where

-- | Law: a <> b <> b == a <> b
class (Monoid a) => IdempotentMonoid a where

instance (Ord a) => IdempotentMonoid (S.Set a) where

instance (Ord k) => IdempotentMonoid (M.Map k a) where

-- TODO laws
class (ElementalMonoid a, Unit a ~ (Index a, Value a)) => IndexedMonoid a where
  type Index a :: *
  type Value a :: *
  mindices :: (ElementalMonoid b, Index a ~ Unit b) => a -> b
  mlookup :: (ElementalMonoid b, Value a ~ Unit b) => Index a -> a -> b
  mvalues :: (ElementalMonoid b, Value a ~ Unit b) => a -> b
  mvalues a = mmconcat $ (mmap (`mlookup` a) $ mindices a `asTypeOf` []) `asTypeOf` []

instance (IndexedMonoid a) => IndexedMonoid (Maybe a) where
  type Index (Maybe a) = Index a
  type Value (Maybe a) = Value a
  mindices Nothing = mempty
  mindices (Just m) = mindices m
  mlookup _ Nothing = mempty
  mlookup i (Just m) = mlookup i m
  mvalues Nothing = mempty
  mvalues (Just m) = mvalues m

instance (Ord k) => IndexedMonoid (Map k a) where
  type Index (Map k a) = k
  type Value (Map k a) = a
  mindices = mconvert . M.keys
  mlookup k m = case M.lookup k m of
    Nothing -> mempty
    Just v -> msingleton v
  mvalues = mconvert . M.elems

