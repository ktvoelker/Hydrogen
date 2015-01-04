
module Main where

import Control.Monad.Writer
import qualified Data.Map as M
import H.IO
import H.Prelude
import qualified Prelude as P
import Test.QuickCheck

prop_show :: (Eq a, Show a) => a -> Bool
prop_show x = unpack (show x) == P.show x

prop_read_show :: (Eq a, Read a, Show a) => a -> Bool
prop_read_show x = read (show x) == Just x

prop_read_empty :: Bool
prop_read_empty = read "" `asTypeOf` Just (0 :: Integer) == Nothing

prop_whenJust :: (Eq a) => a -> Maybe a -> Bool
prop_whenJust x y = f (whenJust y put) == f (maybe (return ()) put y)
  where
    f = flip execState x

prop_onFst :: (Eq a, Num a) => (a, a) -> Bool
prop_onFst x@(a, b) = onFst f x == (f a, b)
  where
    f = (+ 1)

prop_onSnd :: (Eq a, Num a) => (a, a) -> Bool
prop_onSnd x@(a, b) = onSnd f x == (a, f b)
  where
    f = (+ 1)

prop_onFstF :: (Eq a, Num a) => Maybe a -> (a, a) -> Bool
prop_onFstF r x@(a, b) = onFstF f x == fmap (, b) (f a)
  where
    f = const r

prop_onSndF :: (Eq a, Num a) => Maybe a -> (a, a) -> Bool
prop_onSndF r x@(a, b) = onSndF f x == fmap (a, ) (f b)
  where
    f = const r

prop_modifyM :: Integer -> Integer -> Bool
prop_modifyM x y = f a == f b
  where
    f :: StateT Integer (Writer [Integer]) () -> (Integer, [Integer])
    f = runWriter . flip execStateT x
    a = modifyM $ \old -> tell [old] >> return y
    b = gets (: []) >>= tell >> put y

prop_minimumByM :: [Integer] -> Property
prop_minimumByM xs =
  xs /= [] ==>
    runIdentity (minimumByM (\a b -> return (compare a b)) xs) == minimum xs

prop_eitherAlt :: Maybe Integer -> Maybe Rational -> Bool
prop_eitherAlt x y = eitherAlt x y == ((Left <$> x) <|> (Right <$> y))

prop_sequenceWhileJust :: [Maybe Integer] -> Bool
prop_sequenceWhileJust xs = a == b
  where
    f Nothing    = return Nothing
    f x@(Just n) = modify (+ n) >> return x
    a = execState (sequenceWhileJust $ fmap f xs) 0
    b = sum . catMaybes . P.takeWhile isJust $ xs

prop_triples :: (Integer, Integer, Integer) -> Bool
prop_triples x = (fst3 x, snd3 x, thd3 x) == x

type IA = [(Integer, Integer)]

prop_unionWithM :: IA -> IA -> Bool
prop_unionWithM x y = a == b
  where
    x' = M.fromList x
    y' = M.fromList y
    a = runIdentity $ unionWithM ((return .) . (+)) x' y'
    b = M.unionWith (+) x' y'

prop_whenM :: Bool -> Integer -> Integer -> Integer -> Bool
prop_whenM b x y z = f experiment == f control
  where
    f = flip execState x
    experiment = whenM (put z >> return b) (put y)
    control = put z >> when b (put y)

-- For an explanation of this odd line, see:
-- http://hackage.haskell.org/package/QuickCheck-2.7.6/docs/Test-QuickCheck.html#v:quickCheckAll
return []

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = not <$> runTests >>= flip when exitFailure
 
