
module H.Monad
  ( ErrType(..)
  , Err(..)
  , fatal
  , fatal'
  , report
  , report'
  , check
  , internal
  , log
  , Unique()
  , uniqueSourceName
  , nextUnique
  , PrimId()
  , primName
  , primId
  , primUnique
  , MT()
  , MonadM(..)
  , runMT
  , execMT
  ) where

import System.IO

import H.Error
import H.Import
import H.Monad.State
import H.Monad.Types
import H.Util

fatal :: (MonadM m, Show e) => Err e -> m a
fatal = todo

fatal' :: (MonadM m) => Err () -> m a
fatal' = fatal

report :: (MonadM m, Show e) => Err e -> m ()
report = todo

report' :: (MonadM m) => Err () -> m ()
report' = report

check :: (MonadM m) => m ()
check = todo

internal :: (MonadM m, Show a) => a -> m b
internal = fatal' . Err EInternal Nothing Nothing . Just . show

log :: (MonadM m) => Text -> m ()
log xs = liftMT . MT $ gets _mtLogger >>= liftIO . ($ xs)

class (Functor m, Applicative m, Monad m) => MonadM m where
  liftMT :: MT a -> m a

instance MonadM MT where
  liftMT = id

instance (MonadM m) => MonadM (ReaderT r m) where
  liftMT = lift . liftMT

nextUnique :: (MonadM m) => Text -> m Unique
nextUnique name =
  liftMT . MT $ Unique <$> (mtNextUnique %%= \u -> (u, u + 1)) <*> pure name

runMT :: MT a -> IO a
runMT = flip evalStateT emptyMTState . unMT

execMT :: MT a -> IO ()
execMT = void . runMT

