
module H.MTM where

import Data.Monoid
import qualified Data.Set as S

import H.Import

data WithOrd a = WithOrd (a -> a -> Ordering) a

instance Eq (WithOrd a) where
  (==) = ((== EQ) .) . compare

instance Ord (WithOrd a) where
  compare (WithOrd c a) (WithOrd _ b) = c a b

newtype Set a = Set (S.Set (WithOrd a)) deriving (Eq, Ord)

-- | Law: msize mempty == 0
-- | Law: msize a + msize b >= msize (a <> b) >= max (msize a) (msize b)
class (Monoid a) => MonotonicMonoid a where
  msize :: a -> Integer

-- | Law: maybe (a == mempty) (\(e, a') -> a == msingleton e <> a') (mextract a)
-- | Law: maybe (msize a == 0) (\(_, a') -> msize a' + 1 == msize a) (mextract a)
class (MonotonicMonoid a) => ElementalMonoid a where
  type Unit a :: *
  msingleton :: Unit a -> a
  mextract :: a -> Maybe (Unit a, a)
  mconvert :: (ElementalMonoid b, Unit a ~ Unit b) => a -> b
  mconvert xs = case mextract xs of
    Nothing -> mempty
    Just (x, xs') -> msingleton x <> mconvert xs'

-- | Law: msize a + msize b == msize (a <> b)
class (ElementalMonoid a) => PreservingMonoid a where

-- | Law: melements (mlist xs) == xs
class (PreservingMonoid a) => StableMonoid a where

-- | Law: a <> b <> b == a <> b
class IdempotentMonoid a where

-- TODO laws
class (ElementalMonoid a, Unit a ~ (Index a, Value a)) => IndexedMonoid a where
  type Index a :: *
  type Value a :: *
  mlookup :: (ElementalMonoid b, Value a ~ Unit b) => Index a -> a -> b

