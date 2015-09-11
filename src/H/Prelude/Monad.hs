
module H.Prelude.Monad
  ( Functor(..), Applicative(..), Alternative(..)
  , Monad(..), MonadPlus(..), Const(..), ZipList(..)
  , (<$>), (<**>), liftA, liftA2, liftA3, optional
  , (=<<), (>=>), (<=<), forever, void
  , mfilter, filterM, zipWithM, zipWithM_, foldM, foldM_, onFstF, onSndF
  , replicateM, replicateM_, guard, unless
  , when, whenM, whenJust, whenJustM, sequenceWhileJust
  , minimumByM, eitherAlt
  ) where

import Control.Applicative
  ( Applicative(..), Alternative(..), Const(..), ZipList(..)
  , (<$>), (<**>), liftA, liftA2, liftA3, optional
  )
import Control.Monad
  ( Functor(..), Monad(..), MonadPlus(..)
  , (=<<), (>=>), (<=<), forever, void
  , mfilter, filterM, zipWithM, zipWithM_, foldM, foldM_
  , replicateM, replicateM_, guard, when, unless
  )

import H.Prelude.Core

-- | If the input is Just, do a monadic action on the value
whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust x f = maybe (return ()) f x

onFstF :: (Functor f) => (a -> f c) -> (a, b) -> f (c, b)
onFstF f (a, b) = (, b) <$> f a

onSndF :: (Functor f) => (b -> f c) -> (a, b) -> f (a, c)
onSndF f (a, b) = (a, ) <$> f b

-- | Find the minimum element of a list using a monadic comparison action.
minimumByM :: (Monad m) => (a -> a -> m Ordering) -> [a] -> m a
minimumByM _ [] = error "minimumByM: Empty list"
minimumByM c (x : xs) = f x c xs
  where
    f acc _ [] = return acc
    f acc c (x : xs) = do
      o <- c x acc
      case o of
        LT -> f x c xs
        _ -> f acc c xs

-- | Like <|>, but the operands may have different value types, with Either providing
-- a union of those two types in the result
eitherAlt :: (Alternative f) => f a -> f b -> f (Either a b)
eitherAlt la ra = (Left <$> la) <|> (Right <$> ra)

infixl 3 `eitherAlt`

-- | Sequence a list of actions that return Maybes, stopping at the first Nothing
sequenceWhileJust :: (Monad m) => [m (Maybe a)] -> m [a]
sequenceWhileJust [] = return []
sequenceWhileJust (m : ms) =
  m >>= maybe (return []) (\x -> liftA (x :) $ sequenceWhileJust ms)

-- | Like when, but the condition is also a monadic action
whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond m = cond >>= \case
  True  -> m
  False -> return ()

whenJustM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM m f = m >>= maybe (return ()) f

