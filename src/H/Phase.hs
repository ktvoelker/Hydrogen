
module H.Phase where

import H.Common
import H.Common.IO

runPhase :: (MonadIO m) => (i -> FM String) -> i -> m ()
runPhase f xs = liftIO . mapM_ putStrLn $ maybeToList out ++ map show errs
  where
    (out, errs) = runFM . f $ xs

getInput xs = fmap (xs,) $ case xs of
  "-" -> getContents
  _ -> readFile xs

phases = undefined

main = do
  (phase : files) <- getArgs
  case lookup phase phases of
    Nothing -> putStrLn $ "Unknown phase: " ++ phase
    Just fn -> mapM getInput files >>= fn
 
