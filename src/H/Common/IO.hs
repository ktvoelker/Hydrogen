
module H.Common.IO
  ( module H.Common.IO
  , module H.Import.IO
  ) where

import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Map as M

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

readFile :: (MonadIO m) => FilePath -> m T.Text
readFile fp =
  liftIO
  . fmap T.concat
  . runResourceT
  $ sourceFile fp $= decode utf8 $$ CL.consume

readFiles :: (MonadIO m) => [FilePath] -> m (FileMap T.Text)
readFiles = liftIO . fmap M.fromList . mapM (\fp -> (fp,) <$> readFile fp)

