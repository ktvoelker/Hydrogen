
module H.IO
  ( module Data.Text.IO
  , module H.Import.IO
  , module H.IO
  ) where

import Data.Text.IO (putStr, putStrLn)
import qualified System.Environment as E

import H.Import
import H.Import.IO

getArgs :: (MonadIO m) => m [Text]
getArgs = liftM (fmap pack) $ liftIO E.getArgs

