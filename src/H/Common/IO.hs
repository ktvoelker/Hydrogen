
module H.Common.IO
  ( module System.Environment
  , module System.Exit
  , module H.Common.IO
  ) where

import System.Environment
import System.Exit (exitSuccess, exitFailure)

import H.Import
import H.Util

exitWith :: (MonadIO m) => ExitCode -> m a
exitWith ExitSuccess = liftIO exitSuccess
exitWith ExitFailure = liftIO exitFailure

