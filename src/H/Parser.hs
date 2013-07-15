
module H.Parser where

import Text.Parsec hiding (parse, (<|>), many, optional)
import qualified Text.Parsec as P

import H.Common

type Parser a = Parsec [(a, SourcePos)] ()

instance MonadSourcePos (Parser a) where
  getSourcePos = getPosition

parse
  :: (Applicative m, Monad m)
  => Parser a b
  -> String
  -> [(a, SourcePos)]
  -> MT n e m b
parse file name xs = case P.parse file name xs of
  Left err -> fatal . Err EParser Nothing Nothing . Just . show $ err
  Right decl -> return decl

tok :: (Show a) => String -> (a -> Maybe b) -> Parser a b
tok msg = (<?> msg) . token (show . fst) snd . (. fst)

tokWhen :: (Show a) => String -> (a -> Bool) -> Parser a a
tokWhen msg pred = tok msg $ \t -> if pred t then Just t else Nothing

tokEq :: (Eq a, Show a) => a -> Parser a ()
tokEq t = tokWhen (show t) (== t) >> return ()

