
module H.Monad.Internal
  ( module H.Monad.Internal
  , module H.Monad.Internal.Types
  ) where

import H.Import
import H.Monad.Internal.Types
import H.Util

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

