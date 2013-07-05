
module H.Util where

import H.Import

todo :: a
todo = error "Not implemented"

impossible :: String -> a
impossible = error . ("Impossible: " ++)

readMaybe :: (Read a) => String -> Maybe a
readMaybe = fmap fst . listToMaybe . reads

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust x f = maybe (return ()) f x

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

lift2 :: (MonadTrans t, MonadTrans u, Monad m, Monad (u m)) => m a -> t (u m) a
lift2 = lift . lift

lift3
  :: ( MonadTrans t
     , MonadTrans u
     , MonadTrans v
     , Monad m
     , Monad (u m)
     , Monad (t (u m))
     )
  => m a
  -> v (t (u m)) a
lift3 = lift . lift . lift

modifyM :: (MonadState s m) => (s -> m s) -> m ()
modifyM = (>>= put) . (get >>=) 

(%>>=) :: (MonadState a m) => Lens a b -> (b -> m b) -> m b
(%>>=) lens f = access lens >>= f >>= (lens ~=)

infixr 4 %>>=

newtype AppendList a = AppendList ([a] -> [a])

emptyAppendList :: AppendList a
emptyAppendList = AppendList id

appendElem :: a -> AppendList a -> AppendList a
appendElem x (AppendList f) = AppendList $ f . (x :)

appendList :: [a] -> AppendList a -> AppendList a
appendList xs (AppendList f) = AppendList $ f . (xs ++)

realizeList :: AppendList a -> [a]
realizeList (AppendList f) = f []

-- | Get a list of all possible splits of the input list, where the element at each
-- | position is treated as a pivot (not included in either side of the split).
splits :: [a] -> [([a], [a])]
splits = f emptyAppendList emptyAppendList
  where
    f acc _ [] = realizeList acc
    f acc h (t : ts) = f acc' h' ts
      where
        h' = appendElem t h
        acc' = appendElem (realizeList h, ts) acc

minimumByM :: (Monad m) => (a -> a -> m Ordering) -> [a] -> m a
minimumByM _ [] = error "minimumByM: Empty list"
minimumByM c (x : xs) = f x c xs
  where
    f acc _ [] = return acc
    f acc c (x : xs) = do
      o <- c x acc
      case o of
        LT -> f x c xs
        _ -> f acc c xs

