
{-# LANGUAGE UndecidableInstances #-}
module H.Monad
  ( StageNames()
  , ErrType(..)
  , Err(..)
  , fatal
  , report
  , internal
  , log
  , Options(..)
  , Result(..)
  , writeResults
  , writeResult
  , isArtifact
  , MT()
  , MonadUnique(..)
  , runMT
  , execMT
  , stage
  ) where

import qualified Data.Set as S
import Text.Parsec.Pos (SourcePos())
import System.IO

import H.Import
import H.Util

class (Eq a, Ord a, Enum a, Bounded a, Read a, Show a) => StageNames a where

data Options n =
  Options
  { finalStage  :: Maybe n
  , debugStages :: S.Set n
  } deriving (Eq, Ord, Show)

data MTState =
  MTState
  { mtNextUnique :: Integer
  }

emptyMTState = MTState 0

newtype MT n e m a =
  MT
  { getMT
    :: ReaderT (Options n)
       (ErrorT (Either (Err e) (Finished n))
       (StateT MTState
       (WriterT [Result e] m))) a
  }

newtype Finished n = Finished n deriving (Eq, Ord, Show)

data ErrType e = EUnknown | EInternal | ELexer | EParser | EOutput | ECustom e
  deriving (Eq, Ord, Show)

instance (Bounded e) => Bounded (ErrType e) where
  minBound = EUnknown
  maxBound = ECustom maxBound

instance (Enum e) => Enum (ErrType e) where
  toEnum 0 = EUnknown
  toEnum 1 = EInternal
  toEnum 2 = ELexer
  toEnum 3 = EParser
  toEnum 4 = EOutput
  toEnum n = ECustom $ toEnum $ n - 5
  fromEnum EUnknown = 0
  fromEnum EInternal = 1
  fromEnum ELexer = 2
  fromEnum EParser = 3
  fromEnum EOutput = 4
  fromEnum (ECustom e) = fromEnum e + 5

data Err e =
  Err
  { errType      :: ErrType e
  , errSourcePos :: Maybe SourcePos
  , errName      :: Maybe String
  , errMore      :: Maybe String
  } deriving (Show)

instance Error (Err e) where
  noMsg  = Err EUnknown Nothing Nothing Nothing
  strMsg = Err EUnknown Nothing Nothing . Just

instance Error (Either (Err e) (Finished n)) where
  noMsg  = Left noMsg
  strMsg = Left . strMsg

fatal :: (Monad' m) => Err e -> MT n e m a
fatal err = report err >> MT (throwError . Left $ err)

report :: (Monad' m) => Err e -> MT n e m ()
report = MT . tell . (: []) . ErrResult

internal :: (Monad' m, Show a) => a -> MT n e m ()
internal = report . Err EInternal Nothing Nothing . Just . show

log :: (Monad' m) => String -> MT n e m ()
log = report . Err EOutput Nothing Nothing . Just

instance (Monad' m) => Monad (MT n e m) where
  return = pure
  (MT m) >>= f = MT $ m >>= getMT . f
  fail = MT . fail

instance MonadTrans (MT n e) where
  lift = MT . lift4

instance (Functor m) => Functor (MT n e m) where
  fmap f (MT m) = MT . fmap f $ m

instance (Monad' m) => Applicative (MT n e m) where
  pure = MT . return
  (<*>) = liftM2 ($)

instance (Monad' m) => MonadError (Err e) (MT n e m) where
  throwError = MT . throwError . Left
  catchError (MT m) h = MT $ catchError m $ \case
    Left err -> getMT . h $ err
    finished -> throwError finished

instance (Monad' m) => MonadWriter [Result e] (MT n e m) where
  listen (MT m) = MT $ listen m
  pass (MT m) = MT $ pass m

data Result e =
    ErrResult (Err e)
  | DebugResult String
  | ArtifactResult String String
  deriving (Show)

writeResults :: (Monad' m, MonadIO m, Show e) => [Result e] -> m ExitCode
writeResults = fmap mconcat . mapM writeResult

writeResult :: (Monad' m, MonadIO m, Show e) => Result e -> m ExitCode
writeResult (ErrResult e) = liftIO (hPrint stderr e) >> return ExitFailure
writeResult (DebugResult xs) = liftIO (hPutStrLn stderr xs) >> return ExitSuccess
writeResult (ArtifactResult fp xs) = liftIO (writeFile fp xs) >> return ExitSuccess

isArtifact :: Result e -> Bool
isArtifact (ArtifactResult _ _) = True
isArtifact _ = False

class (Monad' m) => MonadUnique m where
  nextUnique :: m Integer

instance (Monad' m) => MonadUnique (MT n e m) where
  nextUnique = MT $ do
    s <- get
    let u = mtNextUnique s
    put $ s { mtNextUnique = u + 1 }
    return u

runMT
  :: (Monad' m)
  => Options n
  -> MT n e m a
  -> m (Either (Either (Err e) (Finished n)) a, [Result e])
runMT opts =
  runWriterT
  . flip evalStateT emptyMTState
  . runErrorT
  . flip runReaderT opts
  . getMT

execMT :: (Monad' m) => Options n -> MT n e m a -> m [Result e]
execMT = (fmap snd .) . runMT

stage
  :: (Monad' m, StageNames n)
  => n
  -> MT n e m b
  -> MT n e m b
stage name (MT m) = MT $ do
  Options{..} <- ask
  let c = if name `S.member` debugStages then id else filter isArtifact
  output <- censor c m
  when (Just name == finalStage) . throwError . Right . Finished $ name
  return output

