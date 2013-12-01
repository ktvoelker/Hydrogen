
module H.Monad
  ( StageNames()
  , ErrType(..)
  , Err(..)
  , fatal
  , report
  , internal
  , log
  , dump
  , artifact
  , Options(..)
  , Result(..)
  , writeResults
  , writeResult
  , isArtifact
  , Unique()
  , nextUnique
  , MT()
  , MonadM(..)
  , runMT
  , execMT
  , stage
  ) where

import qualified Data.Set as S
import System.IO

import H.Import
import H.Monad.Internal
import H.Util

newtype MT n e m a = MT { getMT :: MTInner n e m a }

fatal :: (MonadM m) => Err (LowerE m) -> m a
fatal err = liftMT $ report err >> MT (throwError . Left $ err)

report :: (MonadM m) => Err (LowerE m) -> m ()
report = liftMT . MT . tell . (: []) . ErrResult

internal :: (MonadM m, Show a) => a -> m b
internal = fatal . Err EInternal Nothing Nothing . Just . show

log :: (MonadM m) => String -> m ()
log = report . Err EOutput Nothing Nothing . Just

dump :: (MonadM m) => String -> m ()
dump = liftMT . MT . dump'

dump' :: (Monad m) => String -> MTInner n e m ()
dump' = tell . (: []) . DebugResult

dumpResult' :: (Monad m, Show a) => MTInner n e m a -> MTInner n e m a
dumpResult' m = do
  r <- m
  dump' . show $ r
  return r

artifact :: (MonadM m) => String -> String -> m ()
artifact fp = liftMT . MT . tell . (: []) . ArtifactResult fp

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
  (<*>) = liftA2 ($)

instance (Applicative m, Monad m) => MonadError (Err e) (MT n e m) where
  throwError = MT . throwError . Left
  catchError (MT m) h = MT $ catchError m $ \case
    Left err -> getMT . h $ err
    finished -> throwError finished

instance (Applicative m, Monad m) => MonadWriter [Result e] (MT n e m) where
  listen (MT m) = MT $ listen m
  pass (MT m) = MT $ pass m

writeResults :: (Monad m, MonadIO m, Show e) => [Result e] -> m ExitCode
writeResults = liftM mconcat . mapM writeResult

writeResult :: (Monad m, MonadIO m, Show e) => Result e -> m ExitCode
writeResult (ErrResult e) = liftIO (hPrint stderr e) >> return ExitFailure
writeResult (DebugResult xs) = liftIO (hPutStrLn stderr xs) >> return ExitSuccess
writeResult (ArtifactResult fp xs) = liftIO (writeFile fp xs) >> return ExitSuccess

isArtifact :: Result e -> Bool
isArtifact (ArtifactResult _ _) = True
isArtifact _ = False

isDebug :: Result e -> Bool
isDebug (DebugResult _) = True
isDebug _ = False

class
  ( Monad m
  , Applicative m
  , Functor m
  , Monad (LowerM m)
  , Applicative (LowerM m)
  , Functor (LowerM m)
  ) => MonadM m where
  type LowerN (m :: * -> *) :: *
  type LowerE (m :: * -> *) :: *
  type LowerM (m :: * -> *) :: * -> *
  liftMT :: MT (LowerN m) (LowerE m) (LowerM m) a -> m a

instance (Functor m, Applicative m, Monad m) => MonadM (MT n e m) where
  type LowerN (MT n e m) = n
  type LowerE (MT n e m) = e
  type LowerM (MT n e m) = m
  liftMT = id

instance (Functor m, Applicative m, Monad m) => MonadM (MTInner n e m) where
  type LowerN (MTInner n e m) = n
  type LowerE (MTInner n e m) = e
  type LowerM (MTInner n e m) = m
  liftMT (MT inner) = inner

instance (MonadM m) => MonadM (ReaderT r m) where
  type LowerN (ReaderT r m) = LowerN m
  type LowerE (ReaderT r m) = LowerE m
  type LowerM (ReaderT r m) = LowerM m
  liftMT = lift . liftMT

nextUnique :: (MonadM m) => Text -> m Unique
nextUnique name =
  liftMT . MT $ Unique <$> (mtNextUnique %%= \u -> (u, u + 1)) <*> pure name

runMT
  :: (Monad m)
  => Options n
  -> MT n e m a
  -> m (Either (Either (Err e) (Finished n)) a, [Result e])
runMT opts =
  runWriterT
  . flip evalStateT emptyMTState
  . runErrorT
  . flip runReaderT opts
  . getMTInner
  . getMT

execMT :: (Monad m) => Options n -> MT n e m a -> m [Result e]
execMT = (liftM snd .) . runMT

stage
  :: (Monad m, StageNames n, Show a)
  => n
  -> MT n e m a
  -> MT n e m a
stage name (MT m) = MT $ do
  Options{..} <- ask
  let c = if name `S.member` debugStages then id else filter (not . isDebug)
  output <- censor c . dumpResult' $ m
  when (Just name == finalStage) . throwError . Right . Finished $ name
  return output

