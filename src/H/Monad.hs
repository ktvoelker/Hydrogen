
module H.Monad
  ( ErrType(..)
  , Err(..)
  , fatal
  , report
  , internal
  , log
  , dump
  , artifact
  , Result(..)
  , writeResults
  , writeResult
  , isArtifact
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
  , check
  ) where

import System.IO

import H.Import
import H.Monad.Errors
import H.Monad.State
import H.Monad.Types
import H.Util

fatal :: (MonadM m) => Err (LowerE m) -> m a
fatal err = liftMT $ report err >> MT (throwError err)

report :: (MonadM m) => Err (LowerE m) -> m ()
report = liftMT . MT . tell . (: []) . ErrResult

internal :: (MonadM m, Show a) => a -> m b
internal = fatal . Err EInternal Nothing Nothing . Just . show

log :: (MonadM m, MonadIO (LowerM m)) => Text -> m ()
log xs = liftMT . MT $ gets _mtLogger >>= liftIO . ($ xs)

dump :: (Monad m) => String -> MT e m ()
dump = tell . (: []) . DebugResult

artifact :: (MonadM m) => String -> String -> m ()
artifact fp = liftMT . MT . tell . (: []) . ArtifactResult fp

writeResults :: (Monad m, MonadIO m, Show e) => [Result e] -> m ExitCode
writeResults = liftM mconcat . mapM writeResult

writeResult :: (Monad m, MonadIO m, Show e) => Result e -> m ExitCode
writeResult (ErrResult e) = liftIO (hPrint stderr e) >> return ExitFailure
writeResult (DebugResult xs) = liftIO (hPutStrLn stderr xs) >> return ExitSuccess
writeResult (ArtifactResult fp xs) = liftIO (writeFile fp xs) >> return ExitSuccess

isArtifact :: Result e -> Bool
isArtifact (ArtifactResult _ _) = True
isArtifact _ = False

class
  ( Monad m
  , Applicative m
  , Functor m
  , Monad (LowerM m)
  , Applicative (LowerM m)
  , Functor (LowerM m)
  ) => MonadM m where
  type LowerE (m :: * -> *) :: *
  type LowerM (m :: * -> *) :: * -> *
  liftMT :: MT (LowerE m) (LowerM m) a -> m a

instance (Functor m, Applicative m, Monad m) => MonadM (MT e m) where
  type LowerE (MT e m) = e
  type LowerM (MT e m) = m
  liftMT = id

instance (MonadM m) => MonadM (ReaderT r m) where
  type LowerE (ReaderT r m) = LowerE m
  type LowerM (ReaderT r m) = LowerM m
  liftMT = lift . liftMT

nextUnique :: (MonadM m) => Text -> m Unique
nextUnique name =
  liftMT . MT $ Unique <$> (mtNextUnique %%= \u -> (u, u + 1)) <*> pure name

runMT
  :: (Monad m)
  => MT e m a
  -> m (Either (Err e) a, [Result e])
runMT =
  runWriterT
  . flip evalStateT emptyMTState
  . runErrorT
  . unMT

execMT :: (Monad m) => MT e m a -> m [Result e]
execMT = liftM snd . runMT

-- Fail if any errors have occurred so far
check :: (Monad m) => MT e m ()
check = todo

