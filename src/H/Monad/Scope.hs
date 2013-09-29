
module H.Monad.Scope where

-- import H.Common

{-
findNameInScope :: (MonadM m) => Text -> m Unique
findNameInScope name =
  inner
    $ (msum . map (M.lookup name) <$> access mtScopeStack)
      >>=
      maybe (nextUnique name) return

-- TODO make this exception-safe
enterScope :: (MonadM m) => [Text] -> m a -> m a
enterScope names m = do
  us <- inner $ M.fromList <$> mapM (\name -> (name,) <$> nextUnique name) names
  inner (mtScopeStack %= (us :)) *> m <* inner (mtScopeStack %= drop 1)
-}

