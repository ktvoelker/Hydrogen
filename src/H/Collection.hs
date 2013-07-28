
module H.Collection where

import qualified Data.Map as M
import qualified Data.Set as S
import H.Import

{--
 - The collection laws:
 -
 - extract empty == Nothing
 -
 - extract c == Just (x, c')
 -   with
 -     size c == size c' + 1
 -     x `member` c
 -  when
 -    c /= empty
 - 
 - insert c x == c'
 -   with
 -     c' /= empty
 -     size c + 1 >= size c' >= size c
 -
 - deleteKey k c == c'
 -   with
 -     size c' == size c - length (findKey k c)
 -     not (k `member` c')
 -
 - findKey _ empty == []
 -
 - findKey k c == vs
 -   with
 -     length vs <= size c
 -
 - findKey k c == vs
 -   with
 -     length vs > 0
 -   when
 -     k `member` c
 -
 - size empty == 0
 -
 - size c > 0
 -   with
 -     c /= empty
 -
 - length (toList c) == size c
 -
 - 1 <= size (fromList xs) <= length xs
 -   when
 -     length xs > 0
 -
 - toList empty == []
 -
 - fromList [] == empty
 -
 - singleton x = insert x empty
 -
 - TODO
 -
 - member
 -
 - append
 -}

class (Ord (Key a)) => Collection a where
  type Key (a :: *) :: *
  type Val (a :: *) :: *
  type Ent (a :: *) :: *
  -- Required methods
  entKey :: Ent a -> Key a
  entVal :: Ent a -> Val a
  cempty :: a
  extract :: a -> Maybe (Ent a, a)
  insert :: Ent a -> a -> a
  deleteKey :: Key a -> a -> a
  findKey :: Key a -> a -> [Val a]
  -- findKey c k = map entVal . filter ((== k) . entKey) . toList $ c
  -- Optional methods
  size :: a -> Integer
  size c = case extract c of
    Nothing -> 0
    Just (_, c') -> 1 + size c'
  toList :: a -> [Ent a]
  toList c = case extract c of
    Nothing -> []
    Just (e, c') -> e : toList c'
  fromList :: [Ent a] -> a
  fromList [] = cempty
  fromList (e : es) = insert e $ fromList es
  singleton :: Ent a -> a
  singleton = flip insert cempty
  member :: Key a -> a -> Bool
  member = (not .) . (null .) . findKey
  append :: a -> a -> a
  append c = foldr insert c . toList

instance Collection [a] where
  type Key [a] = ()
  type Val [a] = a
  type Ent [a] = a
  entKey = const ()
  entVal = id
  cempty = []
  extract [] = Nothing
  extract (x : xs) = Just (x, xs)
  insert = (:)
  deleteKey = const $ const []
  findKey = const id
  toList = id
  fromList = id
  append = (++)

instance (Ord k) => Collection (M.Map k a) where
  type Key (M.Map k a) = k
  type Val (M.Map k a) = a
  type Ent (M.Map k a) = (k, a)
  entKey = fst
  entVal = snd
  cempty = M.empty
  extract = M.minViewWithKey
  insert = uncurry M.insert
  deleteKey = M.delete
  findKey = (maybeToList .) . M.lookup
  toList = M.toList
  fromList = M.fromList
  singleton = uncurry M.singleton
  member = M.member
  append = M.union

instance (Ord a) => Collection (S.Set a) where
  type Key (S.Set a) = a
  type Val (S.Set a) = ()
  type Ent (S.Set a) = a
  entKey = id
  entVal = const ()
  cempty = S.empty
  extract = S.minView
  insert = S.insert
  deleteKey = S.delete
  findKey k s = if k `S.member` s then [()] else []
  toList = S.toList
  fromList = S.fromList
  singleton = S.singleton
  member = S.member
  append = S.union

