
module H.Util where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Debug.Trace
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

-- | If the input is Just, do a monadic action on the value
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

-- | Modify the state of a StateT using a monadic action of the inner monad.
modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM = (>>= put) . (get >>=) . (lift .)

-- | Find the minimum element of a list using a monadic comparison action.
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

-- | Like <|>, but the operands may have different value types, with Either providing
-- a union of those two types in the result
eitherAlt :: (Alternative f) => f a -> f b -> f (Either a b)
eitherAlt la ra = (Left <$> la) <|> (Right <$> ra)

infixl 3 `eitherAlt`

-- | Sequence a list of actions that return Maybes, stopping at the first Nothing
sequenceWhileJust :: (Monad m) => [m (Maybe a)] -> m [a]
sequenceWhileJust [] = return []
sequenceWhileJust (m : ms) =
  m >>= maybe (return []) (\x -> liftM (x :) $ sequenceWhileJust ms)

-- | Deconstruct a list into its head and tail
headView :: [a] -> Maybe (a, [a])
headView [] = Nothing
headView (x : xs) = Just (x, xs)

-- | Get the first element of a triple
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- | Get the second element of a triple
snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

-- | Get the third element of a triple
thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

-- | Union two maps, with a monadic action for merging duplicates
unionWithM :: (Ord k, Monad m) => (a -> a -> m a) -> Map k a -> Map k a -> m (Map k a)
unionWithM f a b =
  liftM M.fromList
    . sequence
    . fmap (\(k, v) -> liftM (k,) v)
    . M.toList
    $ M.unionWith f' (M.map return a) (M.map return b)
  where
    f' mx my = mx >>= \x -> my >>= \y -> f x y

-- | Like when, but the condition is also a monadic action
whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond m = cond >>= \case
  True  -> m
  False -> return ()

traceVal :: (Show a) => a -> a
traceVal x = traceShow x x

setCatMaybes :: (Ord a) => Set (Maybe a) -> Set a
setCatMaybes = S.minView >>> \case
  Nothing      -> S.empty
  Just (x, xs) -> maybe id S.insert x $ setCatMaybes xs

setSequence :: (Ord a, Applicative f) => Set (f a) -> f (Set a)
setSequence = S.minView >>> \case
  Nothing      -> pure S.empty
  Just (x, xs) -> S.insert <$> x <*> setSequence xs

onLeft :: (a -> c) -> Either a b -> Either c b
onLeft f = \case
  Left x  -> Left $ f x
  Right y -> Right y

onRight :: (b -> c) -> Either a b -> Either a c
onRight f = \case
  Left x  -> Left x
  Right y -> Right $ f y

whenJustM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM m f = m >>= maybe (return ()) f

boolToMaybe :: Bool -> Maybe ()
boolToMaybe False = Nothing
boolToMaybe True = Just ()
