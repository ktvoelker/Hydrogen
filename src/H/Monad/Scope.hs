
module H.Monad.Scope where

import qualified Data.Map as M

import H.Common

type ScopeT k v = ReaderT (Map k v)

findInScope :: (MonadM m, MonadReader (Map k v) m, Ord k, Show k) => k -> m v
findInScope name = join . (liftM $ maybe err return) . findInScopeMaybe $ name
  where
    err = fatal $ Err ENotFound Nothing (Just $ show name) Nothing

findInScopeMaybe :: (MonadReader (Map k v) m, Ord k) => k -> m (Maybe v)
findInScopeMaybe = ($ ask) . liftM . M.lookup

scope :: (MonadReader (Map k v) m, Ord k) => [(k, m v)] -> m a -> m a
scope = undefined

scope' :: (MonadReader (Map k v) m, Ord k) => m v -> [k] -> m a -> m a
scope' = (scope .) . flip zip . repeat

