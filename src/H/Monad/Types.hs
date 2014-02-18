
module H.Monad.Types where

import H.Import
import H.Monad.Errors
import H.Monad.Stages
import H.Monad.State
import H.Util

data Result e =
    ErrResult (Err e)
  | DebugResult String
  | ArtifactResult String String
  deriving (Show)

type MTRaw n e m =
  ReaderT (Options n)
  (ErrorT (Either (Err e) (Finished n))
  (StateT MTState
  (WriterT [Result e] m)))

newtype MTInner n e m a = MTInner { getMTInner :: MTRaw n e m a }

instance (Functor m) => Functor (MTInner n e m) where
  fmap f = MTInner . fmap f . getMTInner

instance (Applicative m, Monad m) => Applicative (MTInner n e m) where
  pure = MTInner . pure
  (<*>) f x = MTInner $ getMTInner f <*> getMTInner x

instance (Monad m) => Monad (MTInner n e m) where
  (>>=) (MTInner m) f = MTInner $ m >>= getMTInner . f
  return = MTInner . return
  fail = MTInner . fail

instance MonadTrans (MTInner n e) where
  lift = MTInner . lift4

instance (MonadIO m) => MonadIO (MTInner n e m) where
  liftIO = lift . liftIO

instance (Monad m) => MonadReader (Options n) (MTInner n e m) where
  ask = MTInner ask
  local f = MTInner . local f . getMTInner

instance (Monad m) => MonadError (Either (Err e) (Finished n)) (MTInner n e m) where
  throwError = MTInner . throwError
  catchError m h = MTInner $ catchError (getMTInner m) (getMTInner . h)

instance (Monad m) => MonadState MTState (MTInner n e m) where
  state = MTInner . state

instance (Monad m) => MonadWriter [Result e] (MTInner n e m) where
  tell = MTInner . tell
  listen = MTInner . listen . getMTInner
  pass = MTInner . pass . getMTInner

newtype MT n e m a = MT { getMT :: MTInner n e m a }

instance (Functor m, Applicative m, Monad m) => Monad (MT n e m) where
  return = pure
  (MT m) >>= f = MT $ m >>= getMT . f
  fail = MT . fail

instance MonadTrans (MT n e) where
  lift = MT . lift

instance (Applicative m, MonadIO m) => MonadIO (MT n e m) where
  liftIO = lift . liftIO

instance (Functor m) => Functor (MT n e m) where
  fmap f (MT m) = MT . fmap f $ m

instance (Functor m, Applicative m, Monad m) => Applicative (MT n e m) where
  pure = MT . return
  (MT a) <*> (MT b) = MT (a <*> b)

instance (Applicative m, Monad m) => MonadError (Err e) (MT n e m) where
  throwError = MT . throwError . Left
  catchError (MT m) h = MT $ catchError m $ \case
    Left err -> getMT . h $ err
    finished -> throwError finished

instance (Applicative m, Monad m) => MonadWriter [Result e] (MT n e m) where
  listen (MT m) = MT $ listen m
  pass (MT m) = MT $ pass m

