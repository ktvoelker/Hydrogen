
module H.Common.IO
  ( module H.Common.IO
  , module H.Import.IO
  ) where

import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified System.Environment as E

import H.Import
import H.Import.IO
import H.Util

getArgs :: (MonadIO m) => m [Text]
getArgs = liftM (fmap pack) $ liftIO E.getArgs

exitWith :: (MonadIO m) => ExitCode -> m a
exitWith ExitSuccess = liftIO exitSuccess
exitWith ExitFailure = liftIO exitFailure

sourceFile :: (MonadResource m) => FilePath -> Producer m BS.ByteString
sourceFile = CB.sourceFile . encodeString

getInput :: (MonadResource m) => Text -> Producer m BS.ByteString
getInput "-" = CB.sourceHandle stdin
getInput xs  = CB.sourceFile $ unpack xs

readFile :: (MonadIO m) => FilePath -> m Text
readFile fp =
  liftIO
  . fmap mconcat
  . runResourceT
  $ sourceFile fp $= decode utf8 $$ CL.consume

readFiles :: (MonadIO m) => [FilePath] -> m (FileMap Text)
readFiles = liftIO . fmap M.fromList . mapM (\fp -> (fp,) <$> readFile fp)

