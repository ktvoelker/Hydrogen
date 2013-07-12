
{-# LANGUAGE UndecidableInstances #-}
module H.Phase.Types
  ( StageNames()
  , Result(..)
  , Pipeline()
  , stage
  , execPipeline
  , evalPipeline
  , runPipeline
  ) where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Set as S

import H.Common

class (Eq a, Ord a, Enum a, Bounded a, Read a, Show a) => StageNames a where

data Finished = Finished deriving (Eq, Ord, Enum, Bounded, Show)

data Options n =
  Options
  { finalStage  :: Maybe n
  , debugStages :: S.Set n
  } deriving (Eq, Ord, Show)

newtype PM n e m a =
  PM
  { getPM :: ReaderT (Options n) (ErrorT Finished (MT e m)) a
  }

instance Error Finished where
  noMsg = Finished

instance (Monad' m) => Monad (PM n e m) where
  (>>=) (PM m) f = PM $ m >>= getPM . f
  return = PM . return
  fail = PM . fail

instance (Monad' m) => MonadError Finished (PM n e m) where
  throwError = PM . throwError
  catchError (PM m) h = PM $ catchError m (getPM . h)

instance (Monad' m) => MonadReader (Options n) (PM n e m) where
  ask = PM ask
  local f (PM m) = PM $ local f m

instance (Monad' m) => MonadWriter [Result e] (PM n e m) where
  listen (PM m) = PM $ listen m
  pass (PM m) = PM $ pass m

runPM :: (Monad' m) => Options n -> PM n e m a -> m (Maybe a, [Result e])
runPM opts = getPM >>> flip runReaderT opts >>> runErrorT >>> runMT >>> liftM f
  where
    f (Right (Right a), w) = (Just a, w)
    f (_, w) = (Nothing, w)

newtype Pipeline n e m a b = Pipeline (a -> PM n e m b)

stage
  :: (Monad' m, StageNames n)
  => n
  -> (a -> MT e m b)
  -> Pipeline n e m a b
stage name trans = Pipeline $ \input -> do
  Options{..} <- ask
  let c = if name `S.member` debugStages then id else filter isArtifact
  output <- censor c . PM . lift2 . trans $ input
  when (Just name == finalStage) $ throwError Finished
  return output

instance (Monad' m) => Category (Pipeline n e m) where
  id = Pipeline return
  (.) (Pipeline a) (Pipeline b) = Pipeline $ b >=> a

execPipeline
  :: (Monad' m)
  => Pipeline n e m a b
  -> n
  -> S.Set n
  -> a
  -> m [Result e]
execPipeline (Pipeline trans) final debugs =
  liftM snd . runPM (Options (Just final) debugs) . trans

evalPipeline
  :: (Monad' m)
  => Pipeline n e m a b
  -> a
  -> m b
evalPipeline (Pipeline trans) =
  liftM (fromJust . fst) . runPM (Options Nothing S.empty) . trans

runPipeline
  :: (Monad' m)
  => Pipeline n e m a b
  -> S.Set n
  -> a
  -> m (b, [Result e])
runPipeline (Pipeline trans) debugs =
  liftM (\(a, w) -> (fromJust a, w)) . runPM (Options Nothing debugs) . trans

