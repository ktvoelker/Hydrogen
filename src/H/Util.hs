
module H.Util where

import qualified Control.Monad.Error as E
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Prelude as P

import H.Import

show :: (Show a) => a -> Text
show = T.pack . P.show

read :: (Read a) => Text -> Maybe a
read = fmap fst . listToMaybe . P.reads . T.unpack

todo :: a
todo = error "Not implemented"

error :: Text -> a
error = P.error . T.unpack

impossible :: Text -> a
impossible = error . ("Impossible: " <>)

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust x f = maybe (return ()) f x

onFst :: (a -> c) -> (a, b) -> (c, b)
onFst f (a, b) = (f a, b)

onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (a, b) = (a, f b)

onFstF :: (Functor f) => (a -> f c) -> (a, b) -> f (c, b)
onFstF f (a, b) = (, b) <$> f a

onSndF :: (Functor f) => (b -> f c) -> (a, b) -> f (a, c)
onSndF f (a, b) = (a, ) <$> f b

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

lift4
  :: ( MonadTrans t
     , MonadTrans u
     , MonadTrans v
     , MonadTrans w
     , Monad m
     , Monad (u m)
     , Monad (t (u m))
     , Monad (v (t (u m)))
     )
  => m a
  -> w (v (t (u m))) a
lift4 = lift . lift . lift . lift

modifyM :: (MonadState s m) => (s -> m s) -> m ()
modifyM = (>>= put) . (get >>=) 

newtype ListBuilder a = ListBuilder { unListBuilder :: [a] -> [a] }

instance Monoid (ListBuilder a) where
  mempty = ListBuilder id
  mappend (ListBuilder f) (ListBuilder g) = ListBuilder $ f . g
  mconcat = ListBuilder . foldr (.) id . fmap unListBuilder

emptyListBuilder :: ListBuilder a
emptyListBuilder = mempty

buildElem :: a -> ListBuilder a
buildElem x = ListBuilder (x :)

buildList :: [a] -> ListBuilder a
buildList xs = ListBuilder (xs <>)

finishList :: ListBuilder a -> [a]
finishList (ListBuilder f) = f []

-- | Get a list of all possible splits of the input list, where the element at each
-- | position is treated as a pivot (not included in either side of the split).
splits :: [a] -> [([a], [a])]
splits = f emptyListBuilder emptyListBuilder
  where
    f acc _ [] = finishList acc
    f acc h (t : ts) = f acc' h' ts
      where
        h' = buildElem t <> h
        acc' = buildElem (finishList h, ts) <> acc

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

data Proxy a = Proxy

asProxied :: a -> Proxy a -> a
asProxied = const

infix 8 `asProxied`

eitherAlt :: (Alternative f) => f a -> f b -> f (Either a b)
eitherAlt la ra = (Left <$> la) <|> (Right <$> ra)

infixl 3 `eitherAlt`

sequenceWhileJust :: (Monad m) => [m (Maybe a)] -> m [a]
sequenceWhileJust [] = return []
sequenceWhileJust (m : ms) =
  m >>= maybe (return []) (\x -> liftM (x :) $ sequenceWhileJust ms)

headView :: [a] -> Maybe (a, [a])
headView [] = Nothing
headView (x : xs) = Just (x, xs)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

unionWithM :: (Ord k, Monad m) => (a -> a -> m a) -> Map k a -> Map k a -> m (Map k a)
unionWithM f a b =
  liftM M.fromList
    . sequence
    . fmap (\(k, v) -> liftM (k,) v)
    . M.toList
    $ M.unionWith f' (M.map return a) (M.map return b)
  where
    f' mx my = mx >>= \x -> my >>= \y -> f x y

textMsg :: (Error a) => Text -> a
textMsg = E.strMsg . unpack

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond m = cond >>= \case
  True  -> m
  False -> return ()

