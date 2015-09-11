
module H.IO
  ( module Data.Text.IO
  , module H.Import.IO
  , module H.IO
  ) where

import Data.Text.IO (putStr, putStrLn)
import qualified System.Environment as E

import H.Import.IO
import H.Prelude

getArgs :: (MonadIO m) => m [Text]
getArgs = liftM (fmap pack) $ liftIO E.getArgs

