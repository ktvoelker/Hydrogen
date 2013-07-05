
{-# LANGUAGE UndecidableInstances #-}
module H.Monad
  ( ErrType(..)
  , Err(..)
  , fatal
  , report
  , internal
  , log
  , Result(..)
  , writeResults
  , writeResult
  , isArtifact
  , MT()
  , MonadUnique(..)
  , runMT
  ) where

import Text.Parsec.Pos (SourcePos())
import System.IO

import H.Import
import H.Util

data MTState =
  MTState
  { mtNextUnique :: Integer
  }

emptyMTState = MTState 0

newtype MT e m a =
  MT { getMT :: ErrorT (Err e) (StateT MTState (WriterT [Result e] m)) a }

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

fatal :: (Monad' m) => Err e -> MT e m a
fatal err = report err >> MT (throwError err)

report :: (Monad' m) => Err e -> MT e m ()
report = MT . tell . (: []) . ErrResult

internal :: (Monad' m, Show a) => a -> MT e m ()
internal = report . Err EInternal Nothing Nothing . Just . show

log :: (Monad' m) => String -> MT e m ()
log = report . Err EOutput Nothing Nothing . Just

instance (Monad' m) => Monad (MT e m) where
  return = pure
  (MT m) >>= f = MT $ m >>= getMT . f
  fail = MT . fail

instance MonadTrans (MT e) where
  lift = MT . lift3

instance (Functor m) => Functor (MT e m) where
  fmap f (MT m) = MT . fmap f $ m

instance (Monad' m) => Applicative (MT e m) where
  pure = MT . return
  (<*>) = liftM2 ($)

instance (Monad' m) => MonadError (Err e) (MT e m) where
  throwError = MT . throwError
  catchError (MT m) h = MT $ catchError m (getMT . h)

instance (Monad' m) => MonadWriter [Result e] (MT e m) where
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

instance (Monad' m) => MonadUnique (MT e m) where
  nextUnique = MT $ do
    s <- get
    let u = mtNextUnique s
    put $ s { mtNextUnique = u + 1 }
    return u

runMT :: (Monad' m) => MT e m a -> m (Either (Err e) a, [Result e])
runMT = runWriterT . flip evalStateT emptyMTState . runErrorT . getMT

