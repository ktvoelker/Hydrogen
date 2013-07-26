
module H.Common.IO
  ( module H.Common.IO
  , module H.Import.IO
  ) where

import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString as BS

import H.Import
import H.Import.IO
import H.Util

exitWith :: (MonadIO m) => ExitCode -> m a
exitWith ExitSuccess = liftIO exitSuccess
exitWith ExitFailure = liftIO exitFailure

sourceFile :: (MonadResource m) => FilePath -> Producer m BS.ByteString
sourceFile = CB.sourceFile . encodeString

getInput :: (MonadResource m) => String -> Producer m BS.ByteString
getInput "-" = CB.sourceHandle stdin
getInput xs  = CB.sourceFile xs
 
