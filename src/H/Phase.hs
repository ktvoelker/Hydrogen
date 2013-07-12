
module H.Phase
  ( InputFile(..)
  , MainOptions(..)
  , phasedMain
  , module H.Phase.Types
  ) where

import qualified Data.Set as S
import System.Console.CmdTheLine (Term, run, defTI, termName, version)
import System.IO

import H.Common
import H.Common.IO
import qualified H.Phase.CmdLine as CL
import H.Phase.Types

data InputFile =
  InputFile
  { ifName :: String
  , ifText :: String
  } deriving (Eq, Ord, Show)

argMain
  :: (Show e, StageNames n, Monad' m, MonadIO m)
  => Pipeline n e m [InputFile] b  -- ^ The pipeline
  -> n                             -- ^ The name of the final phase to run
  -> [n]                           -- ^ The names of phases whose output to dump
  -> [String]                      -- ^ The names of the input files
  -> m ()
argMain p f ds inputs = do
  files <- liftIO $ mapM (\name -> InputFile name <$> readFile name) inputs
  execPipelineIO p f (S.fromList ds) files >>= exitWith

data MainOptions n e m b =
  MainOptions
  { moPipeline :: Pipeline n e m [InputFile] b
  , moName     :: String
  , moVersion  :: String
  , moRunMonad :: m () -> IO ()
  }

termMain
  :: (StageNames n, Show e, Show b, Monad' m, MonadIO m)
  => Pipeline n e m [InputFile] b
  -> Term (m ())
termMain p = argMain p <$> CL.phase <*> CL.dump <*> CL.inputs

phasedMain
  :: (StageNames n, Show e, Show b, Monad' m, MonadIO m)
  => MainOptions n e m b
  -> IO ()
phasedMain MainOptions{..} =
  run
    ( moRunMonad <$> termMain moPipeline
    , defTI { termName = moName, version = moVersion }
    )

