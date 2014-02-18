
module H.Monad.Types where

import H.Import
import H.Monad.Errors
import H.Monad.State
import H.Util

data Result e =
    ErrResult (Err e)
  | DebugResult String
  | ArtifactResult String String
  deriving (Show)

type MTRaw e m = ErrorT (Err e) (StateT MTState (WriterT [Result e] m))

newtype MT e m a = MT { unMT :: MTRaw e m a }

instance (Functor m) => Functor (MT e m) where
  fmap f = MT . fmap f . unMT

instance (Applicative m, Monad m) => Applicative (MT e m) where
  pure = MT . pure
  (<*>) f x = MT $ unMT f <*> unMT x

instance (Monad m) => Monad (MT e m) where
  (>>=) (MT m) f = MT $ m >>= unMT . f
  return = MT . return
  fail = MT . fail

instance MonadTrans (MT e) where
  lift = MT . lift3

instance (MonadIO m) => MonadIO (MT e m) where
  liftIO = lift . liftIO

instance (Monad m) => MonadError (Err e) (MT e m) where
  throwError = MT . throwError
  catchError m h = MT $ catchError (unMT m) (unMT . h)

instance (Monad m) => MonadState MTState (MT e m) where
  state = MT . state

instance (Monad m) => MonadWriter [Result e] (MT e m) where
  tell = MT . tell
  listen = MT . listen . unMT
  pass = MT . pass . unMT

